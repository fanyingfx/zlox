const std = @import("std");
const Scanner = @import("scanner.zig").Scanner;
const Chunk = @import("common.zig").Chunk;
const OpCode = @import("common.zig").OpCode;
const Value = @import("value.zig").Value;
const Token = @import("scanner.zig").Token;
const TokenType = @import("scanner.zig").TokenType;
const number_val = @import("value.zig").number_val;
const nil_val = @import("value.zig").nil_val;
const bool_val = @import("value.zig").bool_val;
const Obj = @import("object.zig").Obj;
const ObjString = @import("object.zig").ObjString;
// const VM = @import("vm.zig").VM;
const Collector = @import("collector.zig");

const Prececdence = enum {
    prec_none,
    prec_assignment,
    prec_or,
    prec_and,
    prec_equality,
    prec_comparison,
    prec_term,
    prec_factor,
    prec_unary,
    prec_call,
    prec_primary,

    pub fn value(prec: Prececdence) usize {
        return @intFromEnum(prec);
    }
    pub fn inc(prec: Prececdence) Prececdence {
        return @enumFromInt(@intFromEnum(prec) + 1);
    }
};
// const PrefixFn = *const fn (*Compiler, bool) void;
const ParseFn = *const fn (*Compiler) void;

pub const Compiler = struct {
    current: Token,
    previous: Token,
    scanner: *Scanner,
    compilingChunk: *Chunk,
    // allocator: std.mem.Allocator,
    collector: *Collector,

    pub fn init(collector: *Collector, scanner: *Scanner, chunk: *Chunk) Compiler {
        return .{
            .current = undefined,
            .previous = undefined,
            .scanner = scanner,
            .compilingChunk = chunk,
            .collector = collector,
            // .allocator = allocator,
        };
    }
    pub fn advance(parser: *Compiler) void {
        parser.previous = parser.current;
        parser.current = parser.scanner.scanToken();
    }
    pub fn consume(parser: *Compiler, type_: TokenType, message: []const u8) void {
        if (parser.current.type == type_) {
            parser.advance();
            return;
        }
        std.debug.panic("{s}\n", .{message});
    }
    fn emitByte(parser: *Compiler, byte: u8) void {
        parser.currentChunk().write(byte, parser.previous.line);
    }
    fn emitBytes(parser: *Compiler, op: OpCode, byte: u8) void {
        parser.emitOp(op);
        parser.emitByte(byte);
    }
    fn emitOp(parser: *Compiler, op: OpCode) void {
        parser.currentChunk().writeOp(op, parser.previous.line);
    }
    fn emitOps(parser: *Compiler, op1: OpCode, op2: OpCode) void {
        parser.emitOp(op1);
        parser.emitOp(op2);
    }
    pub fn endCompiler(parser: *Compiler) void {
        parser.emitOp(.op_return);
    }

    pub fn expression(parser: *Compiler) void {
        parser.parsePrecedence(.prec_assignment);
    }
    fn getPrefixFn(token_type: TokenType) ParseFn {
        return switch (token_type) {
            .tok_minus, .tok_bang => unary,
            .tok_number => number,
            .kw_false, .kw_true, .kw_nil, .kw_quit => literal,
            .tok_string => string,
            .tok_left_paren => group,
            else => unreachable,
        };
    }
    fn getInfixFn(token_type: TokenType) ParseFn {
        return switch (token_type) {
            // arithmatic operator
            .tok_plus,
            .tok_minus,
            .tok_slash,
            .tok_star,
            // compare operator
            .tok_bang_equal,
            .tok_equal_equal,
            .tok_greater,
            .tok_greater_equal,
            .tok_less,
            .tok_less_equal,
            .tok_concat,
            => binary,
            else => undefined,
        };
    }
    fn getPrecedence(token_type: TokenType) Prececdence {
        return switch (token_type) {
            .tok_equal => .prec_assignment,
            .tok_minus, .tok_plus, .tok_concat => .prec_term,
            .tok_slash, .tok_star => .prec_factor,
            .tok_bang_equal, .tok_equal_equal, .tok_greater, .tok_greater_equal, .tok_less, .tok_less_equal => .prec_comparison,
            else => .prec_none,
        };
    }
    fn match(parser: *Compiler, tokenType: TokenType) bool {
        if (!parser.check(tokenType)) return false;
        parser.advance();
        return true;
    }
    fn check(parser: *Compiler, tokenType: TokenType) bool {
        return parser.current.type == tokenType;
    }
    fn parsePrecedence(parser: *Compiler, prec: Prececdence) void {
        parser.advance();
        const canAssign = prec.value() <= Prececdence.prec_assignment.value();
        switch (parser.previous.type) {
            .tok_identifier => parser.variable(canAssign),
            else => |tok| {
                const prefixFn = getPrefixFn(tok);
                prefixFn(parser);
            },
        }
        while (prec.value() <= getPrecedence(parser.current.type).value()) {
            parser.advance();
            const infixFn = getInfixFn(parser.previous.type);
            infixFn(parser);
            if (canAssign and parser.match(.tok_equal)) {
                std.debug.panic("Invalid assignment target.\n", .{});
            }
        }
    }
    fn number(parser: *Compiler) void {
        const num = std.fmt.parseFloat(f64, parser.tokenString(parser.previous)) catch unreachable;
        parser.emitConst(number_val(num));
    }
    fn string(parser: *Compiler) void {
        const prev_tok = parser.previous;
        const str = parser.scanner.source[prev_tok.start + 1 .. prev_tok.start + prev_tok.length - 1];
        const objString = parser.collector.copyString(str);
        parser.emitConst(objString.obj_val());
    }
    fn variable(parser: *Compiler, canAssign: bool) void {
        parser.namedVariable(parser.previous, canAssign);
    }
    fn namedVariable(parser: *Compiler, name: Token, canAssign: bool) void {
        const arg = parser.identifierConstant(name);
        if (canAssign and parser.match(.tok_equal)) {
            parser.expression();
            parser.emitBytes(.op_set_global, arg);
        } else {
            parser.emitBytes(.op_get_global, arg);
        }
    }
    fn group(parser: *Compiler) void {
        parser.expression();
        parser.consume(.tok_right_paren, "Expect ')' after expression.");
    }
    fn unary(parser: *Compiler) void {
        const opeartorType = parser.previous.type;
        parser.expression();
        switch (opeartorType) {
            .tok_minus => parser.emitOp(.op_negate),
            .tok_bang => parser.emitOp(.op_not),
            else => unreachable,
        }
    }
    fn literal(parser: *Compiler) void {
        const op: OpCode = switch (parser.previous.type) {
            .kw_false => .op_false,
            .kw_nil => .op_nil,
            .kw_true => .op_true,
            .kw_quit => .op_quit,
            else => unreachable,
        };
        parser.emitOp(op);
    }

    fn binary(parser: *Compiler) void {
        const operatorType = parser.previous.type;
        const prec = getPrecedence(operatorType);
        parser.parsePrecedence(prec.inc());
        switch (operatorType) {
            .tok_bang_equal => parser.emitOps(.op_equal, .op_not),
            .tok_equal_equal => parser.emitOp(.op_equal),
            .tok_greater => parser.emitOp(.op_greater),
            .tok_greater_equal => parser.emitOps(.op_less, .op_not),
            .tok_less => parser.emitOp(.op_less),
            .tok_less_equal => parser.emitOps(.op_greater, .op_not),
            .tok_plus => parser.emitOp(.op_add),
            .tok_minus => parser.emitOp(.op_substract),
            .tok_star => parser.emitOp(.op_multiply),
            .tok_slash => parser.emitOp(.op_divide),
            .tok_concat => parser.emitOp(.op_concat),
            else => unreachable,
        }
    }
    pub fn declaration(parser: *Compiler) void {
        if (parser.match(.kw_var)) {
            parser.varDeclaration();
        } else {
            parser.parseStatement();
        }
    }
    pub fn varDeclaration(parser: *Compiler) void {
        const global = parser.parseVariable("Expect variable name.");
        if (parser.match(.tok_equal)) {
            parser.expression();
        } else {
            parser.emitOp(.op_nil);
        }
        parser.consume(.tok_semicolon, "Expect ';' after variable declaration.");
        parser.defineVariable(global);
    }
    fn parseVariable(parser: *Compiler, errorMessage: []const u8) u8 {
        parser.consume(.tok_identifier, errorMessage);
        return parser.identifierConstant(parser.previous);
    }
    fn identifierConstant(parser: *Compiler, name: Token) u8 {
        const objStr = parser.collector.copyString(parser.tokenString(name));
        return parser.makeConst(objStr.obj_val());
    }
    fn defineVariable(parser: *Compiler, global: u8) void {
        parser.emitBytes(.op_define_global, global);
    }
    pub fn parseStatement(parser: *Compiler) void {
        if (parser.match(.kw_print)) {
            parser.parsePrintStatement();
        } else {
            parser.parseExpressionStatement();
        }
    }
    pub fn parsePrintStatement(parser: *Compiler) void {
        parser.expression();
        parser.consume(.tok_semicolon, "Expect ';' after value.");
        parser.emitOp(.op_print);
    }
    pub fn parseExpressionStatement(parser: *Compiler) void {
        parser.expression();
        parser.consume(.tok_semicolon, "Expect ';' after value.");
        parser.emitOp(.op_pop);
    }
    fn emitConst(parser: *Compiler, value: Value) void {
        parser.emitBytes(.op_constant, parser.makeConst(value));
    }
    fn makeConst(parser: *Compiler, value: Value) u8 {
        const constant = parser.currentChunk().addConstant(value);
        if (constant > std.math.maxInt(u8)) {
            std.debug.panic("Too many constants in on chunk", .{});
        }
        return @intCast(constant);
    }
    fn tokenString(parser: *const Compiler, tok: Token) []const u8 {
        return parser.scanner.source[tok.start .. tok.start + tok.length];
    }
    fn currentChunk(parser: *const Compiler) *Chunk {
        return parser.compilingChunk;
    }
};

pub fn compile(collector: *Collector, source: []const u8, chunk: *Chunk) void {
    var scanner = Scanner.init(source);
    var parser = Compiler.init(collector, &scanner, chunk);
    parser.advance();
    while (!parser.match(.tok_eof)) {
        parser.declaration();
    }
    // parser.parseExpression();
    // parser.consume(.tok_eof, "Expected end of expression.");
    parser.endCompiler();
}
