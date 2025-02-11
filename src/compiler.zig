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
const ParseFn = *const fn (*Parser) void;
const UINT8_COUNT = std.math.maxInt(u8) + 1;
const Local = struct {
    name: Token,
    depth: isize,
};
pub const Compiler = struct {
    locals: [UINT8_COUNT]Local,
    localCount: usize,
    scopeDepth: isize,
    pub fn init() Compiler {
        return .{
            .locals = undefined,
            .localCount = 0,
            .scopeDepth = 0,
        };
    }
    pub fn addLocal(current: *Compiler, name: Token) void {
        if (current.localCount == UINT8_COUNT) {
            std.debug.panic("Too many local variables in function.\n", .{});
        }
        const local = &current.locals[current.localCount];
        local.name = name;
        local.depth = -1 ;//current.scopeDepth;
        current.localCount += 1;
    }
    pub fn markInitialized(current:*Compiler)void{
        current.locals[current.localCount - 1].depth = current.scopeDepth;


    }
};
pub const Parser = struct {
    current: Token,
    previous: Token,
    scanner: *Scanner,
    compilingChunk: *Chunk,
    collector: *Collector,
    currentCompiler: *Compiler,

    pub fn init(collector: *Collector, scanner: *Scanner, chunk: *Chunk, compiler: *Compiler) Parser {
        return .{
            .current = undefined,
            .previous = undefined,
            .scanner = scanner,
            .compilingChunk = chunk,
            .collector = collector,
            .currentCompiler = compiler,
        };
    }
    pub fn advance(parser: *Parser) void {
        parser.previous = parser.current;
        parser.current = parser.scanner.scanToken();
    }
    pub fn consume(parser: *Parser, type_: TokenType, message: []const u8) void {
        if (parser.current.type == type_) {
            parser.advance();
            return;
        }
        std.debug.panic("{s}\n", .{message});
    }
    fn emitByte(parser: *Parser, byte: u8) void {
        parser.currentChunk().write(byte, parser.previous.line);
    }
    fn emitBytes(parser: *Parser, op: OpCode, byte: u8) void {
        parser.emitOp(op);
        parser.emitByte(byte);
    }
    fn emitOp(parser: *Parser, op: OpCode) void {
        parser.currentChunk().writeOp(op, parser.previous.line);
    }
    fn emitOps(parser: *Parser, op1: OpCode, op2: OpCode) void {
        parser.emitOp(op1);
        parser.emitOp(op2);
    }
    pub fn endCompiler(parser: *Parser) void {
        parser.emitOp(.op_return);
    }

    pub fn expression(parser: *Parser) void {
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
    fn match(parser: *Parser, tokenType: TokenType) bool {
        if (!parser.check(tokenType)) return false;
        parser.advance();
        return true;
    }
    fn check(parser: *Parser, tokenType: TokenType) bool {
        return parser.current.type == tokenType;
    }
    fn parsePrecedence(parser: *Parser, prec: Prececdence) void {
        parser.advance();
        const canAssign = prec.value() <= Prececdence.prec_assignment.value();
        // std.debug.print("{s}\n",.{@tagName(parser.previous.tygpe)});
        switch (parser.previous.type) {
            .tok_identifier => {
                parser.variable(canAssign);
            },
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
    fn number(parser: *Parser) void {
        const num = std.fmt.parseFloat(f64, parser.tokenString(parser.previous)) catch unreachable;
        parser.emitConst(number_val(num));
    }
    fn string(parser: *Parser) void {
        const prev_tok = parser.previous;
        const str = parser.scanner.source[prev_tok.start + 1 .. prev_tok.start + prev_tok.length - 1];
        const objString = parser.collector.copyString(str);
        parser.emitConst(objString.obj_val());
    }
    fn variable(parser: *Parser, canAssign: bool) void {
        parser.namedVariable(parser.previous, canAssign);
    }
    fn namedVariable(parser: *Parser, name: Token, canAssign: bool) void {
        var getOp: OpCode = undefined;
        var setOp: OpCode = undefined;
        var arg = parser.resolveLocal(name);
        if (arg != null) {
            getOp = .op_get_local;
            setOp = .op_set_local;
        } else {
            arg = parser.identifierConstant(name);
            getOp = .op_get_global;
            setOp = .op_set_global;
        }
        if (canAssign and parser.match(.tok_equal)) {
            parser.expression();
            parser.emitBytes(setOp, arg.?);
        } else {
            parser.emitBytes(getOp, arg.?);
        }
    }
    fn group(parser: *Parser) void {
        parser.expression();
        parser.consume(.tok_right_paren, "Expect ')' after expression.");
    }
    fn unary(parser: *Parser) void {
        const opeartorType = parser.previous.type;
        parser.expression();
        switch (opeartorType) {
            .tok_minus => parser.emitOp(.op_negate),
            .tok_bang => parser.emitOp(.op_not),
            else => unreachable,
        }
    }
    fn literal(parser: *Parser) void {
        const op: OpCode = switch (parser.previous.type) {
            .kw_false => .op_false,
            .kw_nil => .op_nil,
            .kw_true => .op_true,
            .kw_quit => .op_quit,
            else => unreachable,
        };
        parser.emitOp(op);
    }

    fn binary(parser: *Parser) void {
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
    pub fn declaration(parser: *Parser) void {
        if (parser.match(.kw_var)) {
            // std.debug.print("parser var\n",.{});
            parser.varDeclaration();
        } else {
            parser.statement();
        }
    }
    pub fn varDeclaration(parser: *Parser) void {
        const global = parser.parseVariable("Expect variable name.");
        if (parser.match(.tok_equal)) {
            parser.expression();
        } else {
            parser.emitOp(.op_nil);
        }
        parser.consume(.tok_semicolon, "Expect ';' after variable declaration.");
        parser.defineVariable(global);
    }
    fn parseVariable(parser: *Parser, errorMessage: []const u8) u8 {
        parser.consume(.tok_identifier, errorMessage);
        parser.declareVariable();
        if (parser.currentCompiler.scopeDepth > 0) {
            // std.debug.print("local variable\n",.{});
            return 0;
        }
        return parser.identifierConstant(parser.previous);
    }
    fn identifierConstant(parser: *Parser, name: Token) u8 {
        const objStr = parser.collector.copyString(parser.tokenString(name));
        return parser.makeConst(objStr.obj_val());
    }
    fn declareVariable(parser: *Parser) void {
        if (parser.currentCompiler.scopeDepth == 0) return;
        const name = parser.previous;

        var i = parser.currentCompiler.localCount;
        while (i > 0) {
            i -= 1;
            const local = &parser.currentCompiler.locals[i];
            if (local.depth != -1 and local.depth < parser.currentCompiler.scopeDepth) {
                break;
            }
            if (std.meta.eql(name, local.name)) {
                std.debug.panic("Alreay a variable with this name in this scope.", .{});
            }
        }
        parser.currentCompiler.addLocal(name);
    }
    fn defineVariable(parser: *Parser, global: u8) void {
        if (parser.currentCompiler.scopeDepth > 0) {
            parser.currentCompiler.markInitialized();
            return;
        }
        parser.emitBytes(.op_define_global, global);
    }
    pub fn statement(parser: *Parser) void {
        if (parser.match(.kw_print)) {
            parser.printStatement();
        } else if (parser.match(.tok_left_brace)) {
            parser.beginScope();
            parser.block();
            parser.endScope();
        } else {
            parser.expressionStatement();
        }
    }
    pub fn block(parser: *Parser) void {
        while (!parser.check(.tok_right_brace) and !parser.check(.tok_eof)) {
            parser.declaration();
        }
        parser.consume(.tok_right_brace, "Expect '}' after block.");
    }
    fn beginScope(parser: *Parser) void {
        parser.currentCompiler.scopeDepth += 1;
    }
    fn endScope(parser: *Parser) void {
        const current = parser.currentCompiler;
        current.scopeDepth -= 1;
        while (current.localCount > 0 and current.locals[current.localCount - 1].depth > current.scopeDepth) {
            parser.emitOp(.op_pop);
            current.localCount -= 1;
        }
    }
    pub fn printStatement(parser: *Parser) void {
        parser.expression();
        parser.consume(.tok_semicolon, "Expect ';' after value.");
        parser.emitOp(.op_print);
    }
    pub fn expressionStatement(parser: *Parser) void {
        parser.expression();
        parser.consume(.tok_semicolon, "Expect ';' after value.");
        parser.emitOp(.op_pop);
    }
    fn emitConst(parser: *Parser, value: Value) void {
        parser.emitBytes(.op_constant, parser.makeConst(value));
    }
    fn makeConst(parser: *Parser, value: Value) u8 {
        const constant = parser.currentChunk().addConstant(value);
        if (constant > std.math.maxInt(u8)) {
            std.debug.panic("Too many constants in on chunk", .{});
        }
        return @intCast(constant);
    }

    pub fn resolveLocal(parser: *Parser, name: Token) ?u8 {
        std.debug.print("resolveLocal\n",.{});
        const compiler = parser.currentCompiler;
        if (compiler.localCount <= 0) return null;
        var i = compiler.localCount - 1;
        while (i >= 0) : (i -= 1) {
            const local = compiler.locals[i];
            if (parser.identifiersEqual(name, local.name)) {
                if(local.depth == -1){
                    std.debug.panic("Can't read local variable in its own initializer.\n",.{});
                }
                return @intCast(i);
            }
            if (i == 0) break;
        }
        return null;
    }
    fn tokenString(parser: *const Parser, tok: Token) []const u8 {
        return parser.scanner.source[tok.start .. tok.start + tok.length];
    }
    fn identifiersEqual(parser: *const Parser, a: Token, b: Token) bool {
        const astr = parser.tokenString(a);
        const bstr = parser.tokenString(b);
        return std.mem.eql(u8, astr, bstr);
    }
    fn currentChunk(parser: *const Parser) *Chunk {
        return parser.compilingChunk;
    }
};

pub fn compile(collector: *Collector, compiler: *Compiler, source: []const u8, compilingchunk: *Chunk) void {
    var scanner = Scanner.init(source);
    var parser = Parser.init(collector, &scanner, compilingchunk, compiler);
    parser.advance();
    while (!parser.match(.tok_eof)) {
        parser.declaration();
    }
    parser.endCompiler();
}
