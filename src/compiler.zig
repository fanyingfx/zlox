const std = @import("std");
const config = @import("config");
const Scanner = @import("scanner.zig").Scanner;
const Chunk = @import("chunk.zig");
const OpCode = @import("opCode.zig").OpCode;
const Value = @import("value.zig").Value;
const Token = @import("scanner.zig").Token;
const TokenType = @import("scanner.zig").TokenType;
// const number_val = @import("value.zig").number_val;
// const nil_val = @import("value.zig").nil_val;
// const bool_val = @import("value.zig").bool_val;
const Obj = @import("object.zig").Obj;
const ObjString = @import("object.zig").ObjString;
const ObjFunction = @import("object.zig").ObjFunction;
const Collector = @import("collector.zig");
const Err = error{CompilerError};

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
const ParseFn = *const fn (*ParserContext) Err!void;
const UINT8_COUNT = std.math.maxInt(u8) + 1;
const Local = struct {
    name: Token,
    depth: isize,
};
const FunctionType = enum { function, script };
pub const Compiler = struct {
    enclosing: ?*Compiler,
    locals: [UINT8_COUNT]Local,
    localCount: usize,
    scopeDepth: isize,
    function: *ObjFunction,
    type_: FunctionType,
    pub fn addLocal(current: *Compiler, name: Token) void {
        if (current.localCount == UINT8_COUNT) {
            std.debug.panic("Too many local variables in function.\n", .{});
        }
        const local = &current.locals[current.localCount];
        local.name = name;
        local.depth = -1; //current.scopeDepth;
        current.localCount += 1;
    }
    pub fn markInitialized(current: *Compiler) void {
        if (current.scopeDepth == 0) return;
        current.locals[current.localCount - 1].depth = current.scopeDepth;
    }
};
pub const ParserContext = struct {
    current: Token,
    previous: Token,
    scanner: *Scanner,
    compilingChunk: *Chunk,
    collector: *Collector,
    currentCompiler: ?*Compiler,

    pub fn init(collector: *Collector, scanner: *Scanner, chunk: *Chunk) ParserContext {
        return .{
            .current = undefined,
            .previous = undefined,
            .scanner = scanner,
            .compilingChunk = chunk,
            .collector = collector,
            .currentCompiler = null,
        };
    }
    fn initCompiler(parser: *ParserContext, compiler: *Compiler, typ: FunctionType) void {
        compiler.* = .{
            .enclosing = parser.currentCompiler,
            .locals = undefined,
            .localCount = 0,
            .scopeDepth = 0,
            .type_ = typ,
            .function = undefined,
        };
        compiler.function = parser.collector.allocateFunction();
        parser.currentCompiler = compiler;

        if (compiler.type_ != .script) {
            compiler.function.name = parser.collector.copyString(parser.tokenString(parser.previous));
        }

        const local = &compiler.locals[parser.currentCompiler.?.localCount];
        compiler.localCount += 1;
        local.depth = 0;
        local.name.start = 0;
        local.name.length = 0;
    }
    pub fn advance(parser: *ParserContext) void {
        parser.previous = parser.current;
        parser.current = parser.scanner.scanToken();
    }
    pub fn consume(parser: *ParserContext, type_: TokenType, message: []const u8) Err!void {
        if (parser.current.type == type_) {
            parser.advance();
            return;
        }
        std.debug.print("CompilerError: {s}\n", .{message});
        return error.CompilerError;
    }
    fn emitByte(parser: *ParserContext, byte: u8) void {
        parser.currentChunk().write(byte, parser.previous.line);
    }
    fn emitBytes(parser: *ParserContext, op: OpCode, byte: u8) void {
        parser.emitOp(op);
        parser.emitByte(byte);
    }
    fn emitOp(parser: *ParserContext, op: OpCode) void {
        parser.currentChunk().write(op, parser.previous.line);
    }
    fn emitReturn(parser: *ParserContext) void {
        parser.emitOp(.op_nil);
        parser.emitOp(.op_return);
    }
    pub fn endCompiler(parser: *ParserContext) *ObjFunction {
        parser.emitReturn();
        const func = parser.currentCompiler.?.function;
        if (comptime config.enable_debug) {
            const debug = @import("debug.zig");
            const name = if (func.name) |name| name.chars else "<script>";
            debug.disassembleChunk(parser.currentChunk(), name);
        }
        parser.currentCompiler = parser.currentCompiler.?.enclosing;
        return func;
    }

    pub fn expression(parser: *ParserContext) Err!void {
        try parser.parsePrecedence(.prec_assignment);
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
            .kw_and => and_,
            .kw_or => or_,
            .tok_left_paren => call,
            else => unreachable,
        };
    }
    fn getPrecedence(token_type: TokenType) Prececdence {
        return switch (token_type) {
            .tok_equal => .prec_assignment,
            .tok_minus, .tok_plus, .tok_concat => .prec_term,
            .tok_slash, .tok_star => .prec_factor,
            .tok_bang_equal, .tok_equal_equal, .tok_greater, .tok_greater_equal, .tok_less, .tok_less_equal => .prec_comparison,
            .kw_and => .prec_and,
            .kw_or => .prec_or,
            .tok_left_paren => .prec_call,
            else => .prec_none,
        };
    }
    fn match(parser: *ParserContext, tokenType: TokenType) bool {
        if (!parser.check(tokenType)) return false;
        parser.advance();
        return true;
    }
    fn check(parser: *ParserContext, tokenType: TokenType) bool {
        return parser.current.type == tokenType;
    }
    fn parsePrecedence(parser: *ParserContext, prec: Prececdence) Err!void {
        parser.advance();
        const canAssign = prec.value() <= Prececdence.prec_assignment.value();
        switch (parser.previous.type) {
            .tok_identifier => {
                try parser.variable(canAssign);
            },
            else => |tok| {
                const prefixFn = getPrefixFn(tok);
                try prefixFn(parser);
            },
        }
        while (prec.value() <= getPrecedence(parser.current.type).value()) {
            parser.advance();
            const infixFn = getInfixFn(parser.previous.type);
            try infixFn(parser);
            if (canAssign and parser.match(.tok_equal)) {
                std.debug.panic("Invalid assignment target.\n", .{});
            }
        }
    }
    fn number(parser: *ParserContext) !void {
        const num = std.fmt.parseFloat(f64, parser.tokenString(parser.previous)) catch {
            return error.CompilerError;
        };
        parser.emitConst(Value.number_val(num));
    }
    fn string(parser: *ParserContext) !void {
        const prev_tok = parser.previous;
        const str = parser.scanner.source[prev_tok.start + 1 .. prev_tok.start + prev_tok.length - 1];
        const objString = parser.collector.copyString(str);
        parser.emitConst(objString.toValue());
    }
    fn variable(parser: *ParserContext, canAssign: bool) Err!void {
        try parser.namedVariable(parser.previous, canAssign);
    }
    fn namedVariable(parser: *ParserContext, name: Token, canAssign: bool) Err!void {
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
            try parser.expression();
            parser.emitBytes(setOp, arg.?);
        } else {
            parser.emitBytes(getOp, arg.?);
        }
    }
    fn group(parser: *ParserContext) Err!void {
        try parser.expression();
        try parser.consume(.tok_right_paren, "Expect ')' after expression.");
    }
    fn unary(parser: *ParserContext) Err!void {
        const opeartorType = parser.previous.type;
        try parser.expression();
        switch (opeartorType) {
            .tok_minus => parser.emitOp(.op_negate),
            .tok_bang => parser.emitOp(.op_not),
            else => return error.CompilerError,
        }
    }
    fn literal(parser: *ParserContext) Err!void {
        const op: OpCode = switch (parser.previous.type) {
            .kw_false => .op_false,
            .kw_nil => .op_nil,
            .kw_true => .op_true,
            .kw_quit => .op_quit,
            else => return error.CompilerError,
        };
        parser.emitOp(op);
    }

    fn binary(parser: *ParserContext) Err!void {
        const operatorType = parser.previous.type;
        const prec = getPrecedence(operatorType);
        try parser.parsePrecedence(prec.inc());
        switch (operatorType) {
            .tok_bang_equal => parser.emitBytes(.op_equal, OpCode.op_not.toU8()),
            .tok_equal_equal => parser.emitOp(.op_equal),
            .tok_greater => parser.emitOp(.op_greater),
            .tok_greater_equal => parser.emitBytes(.op_less, OpCode.op_not.toU8()),
            .tok_less => parser.emitOp(.op_less),
            .tok_less_equal => parser.emitBytes(.op_greater, OpCode.op_not.toU8()),
            .tok_plus => parser.emitOp(.op_add),
            .tok_minus => parser.emitOp(.op_substract),
            .tok_star => parser.emitOp(.op_multiply),
            .tok_slash => parser.emitOp(.op_divide),
            .tok_concat => parser.emitOp(.op_concat),
            else => return error.CompilerError,
        }
    }
    fn call(parser: *ParserContext) Err!void {
        const argCount = try parser.argumentList();
        parser.emitBytes(.op_call, argCount);
    }
    fn argumentList(parser: *ParserContext) Err!u8 {
        var argCount: u8 = 0;
        if (!parser.check(.tok_right_paren)) {
            while (true) {
                try parser.expression();
                argCount += 1;
                if (argCount == 255) {
                    std.debug.panic("Can't have more than 255 arguments.", .{});
                }
                if (!parser.match(.tok_comma)) {
                    break;
                }
            }
        }
        try parser.consume(.tok_right_paren, "Expect ')' after arguments.");
        return argCount;
    }
    fn and_(parser: *ParserContext) Err!void {
        const endJump = parser.emitJump(.op_jump_if_false);
        parser.emitOp(.op_pop);
        try parser.parsePrecedence(.prec_and);
        parser.patchJump(endJump);
    }
    fn or_(parser: *ParserContext) Err!void {
        const elseJump = parser.emitJump(.op_jump_if_false);
        const endJump = parser.emitJump(.op_jump);
        parser.patchJump(elseJump);
        parser.emitOp(.op_pop);
        try parser.parsePrecedence(.prec_or);
        parser.patchJump(endJump);
    }
    fn declaration(parser: *ParserContext) Err!void {
        if (parser.match(.kw_fun)) {
            try parser.funDeclaration();
        } else if (parser.match(.kw_var)) {
            try parser.varDeclaration();
        } else {
            try parser.statement();
        }
    }
    fn funDeclaration(parser: *ParserContext) Err!void {
        const global = try parser.parseVariable("Expect function name.");
        parser.currentCompiler.?.markInitialized();
        try parser.function(.function);
        parser.defineVariable(global);
    }
    fn function(parser: *ParserContext, typ: FunctionType) Err!void {
        // parser.currentCompiler = Compiler.init(parser.currentCompiler, typ);
        var compiler: Compiler = undefined;
        parser.initCompiler(&compiler, typ);
        parser.beginScope();
        try parser.consume(.tok_left_paren, "Expect '('");
        // function paramaters
        if (!parser.check(.tok_right_paren)) {
            while (true) {
                compiler.function.arity += 1;
                if (compiler.function.arity > 255) {
                    std.debug.panic("Can't have more than 255 paramaters\n", .{});
                }
                const constant_ = try parser.parseVariable("Expect paramater name");
                parser.defineVariable(constant_);
                if (!parser.match(.tok_comma)) {
                    break;
                }
            }
        }
        try parser.consume(.tok_right_paren, "Expect ')'");
        try parser.consume(.tok_left_brace, "Expect '}'");
        try parser.block();
        const func = parser.endCompiler();
        parser.emitBytes(.op_constant, parser.makeConst(func.toValue()));
    }

    fn varDeclaration(parser: *ParserContext) Err!void {
        const global = try parser.parseVariable("Expect variable name.");
        if (parser.match(.tok_equal)) {
            try parser.expression();
        } else {
            parser.emitOp(.op_nil);
        }
        try parser.consume(.tok_semicolon, "Expect ';' after variable declaration.");
        parser.defineVariable(global);
    }
    fn parseVariable(parser: *ParserContext, errorMessage: []const u8) Err!u8 {
        try parser.consume(.tok_identifier, errorMessage);
        parser.declareVariable();
        if (parser.currentCompiler.?.scopeDepth > 0) {
            return 0;
        }
        return parser.identifierConstant(parser.previous);
    }
    fn identifierConstant(parser: *ParserContext, name: Token) u8 {
        const objStr = parser.collector.copyString(parser.tokenString(name));
        return parser.makeConst(objStr.toValue());
    }
    fn declareVariable(parser: *ParserContext) void {
        const current = parser.currentCompiler.?;
        if (current.scopeDepth == 0) return;
        const name = parser.previous;

        var i = current.localCount;
        while (i > 0) {
            i -= 1;
            const local = &current.locals[i];
            if (local.depth != -1 and local.depth < current.scopeDepth) {
                break;
            }
            if (std.meta.eql(name, local.name)) {
                std.debug.panic("Alreay a variable with this name in this scope.", .{});
            }
        }
        current.addLocal(name);
    }
    fn defineVariable(parser: *ParserContext, global: u8) void {
        if (parser.currentCompiler.?.scopeDepth > 0) {
            parser.currentCompiler.?.markInitialized();
            return;
        }
        parser.emitBytes(.op_define_global, global);
    }
    pub fn block(parser: *ParserContext) Err!void {
        while (!parser.check(.tok_right_brace) and !parser.check(.tok_eof)) {
            try parser.declaration();
        }
        try parser.consume(.tok_right_brace, "Expect '}' after block.");
    }
    fn beginScope(parser: *ParserContext) void {
        parser.currentCompiler.?.scopeDepth += 1;
    }
    fn endScope(parser: *ParserContext) void {
        const current = parser.currentCompiler.?;
        current.scopeDepth -= 1;
        while (current.localCount > 0 and current.locals[current.localCount - 1].depth > current.scopeDepth) {
            parser.emitOp(.op_pop);
            current.localCount -= 1;
        }
    }

    pub fn statement(parser: *ParserContext) Err!void {
        if (parser.match(.kw_print)) {
            try parser.printStatement();
        } else if (parser.match(.tok_left_brace)) {
            parser.beginScope();
            try parser.block();
            parser.endScope();
        } else if (parser.match(.kw_if)) {
            try parser.ifStatement();
        } else if (parser.match(.kw_while)) {
            try parser.whileStatement();
        } else if (parser.match(.kw_return)) {
            try parser.returnStatement();
        } else {
            try parser.expressionStatement();
        }
    }
    fn printStatement(parser: *ParserContext) Err!void {
        try parser.expression();
        try parser.consume(.tok_semicolon, "Expect ';' after value.");
        parser.emitOp(.op_print);
    }
    fn returnStatement(parser: *ParserContext) Err!void {
        if (parser.currentCompiler.?.type_ == .script) {
            std.debug.print("Can't return from top-level code.", .{});
            return error.CompilerError;
        }
        if (parser.match(.tok_semicolon)) {
            parser.emitReturn();
        } else {
            try parser.expression();
            try parser.consume(.tok_semicolon, "Expect ';' after return value");
            parser.emitOp(.op_return);
        }
    }
    fn ifStatement(parser: *ParserContext) Err!void {
        try parser.consume(.tok_left_paren, "Expect '(' after 'if'.");
        try parser.expression();
        try parser.consume(.tok_right_paren, "Expect ')' after condition.");
        const thenJump = parser.emitJump(.op_jump_if_false);
        parser.emitOp(.op_pop);
        try parser.statement();
        // in byte code, if always has else branch.
        const elseJump = parser.emitJump(.op_jump);
        parser.patchJump(thenJump);
        parser.emitOp(.op_pop);
        if (parser.match(.kw_else)) try parser.statement();
        parser.patchJump(elseJump);
    }
    fn whileStatement(parser: *ParserContext) Err!void {
        const loopStart = parser.currentChunk().code.items.len;
        try parser.consume(.tok_left_paren, "Expect '(' after 'while'.");
        try parser.expression();
        try parser.consume(.tok_right_paren, "Expect ')' after condition.");
        const exitJump = parser.emitJump(.op_jump_if_false);
        parser.emitOp(.op_pop);
        try parser.statement();
        parser.emitLoop(loopStart);
        parser.patchJump(exitJump);
        parser.emitOp(.op_pop);
    }
    fn emitLoop(parser: *ParserContext, loopStart: usize) void {
        parser.emitOp(.op_loop);
        const offset = parser.currentChunk().code.items.len - loopStart + 2;
        if (offset > std.math.maxInt(u16)) {
            std.debug.panic("Loop body too large.", .{});
        }
        parser.emitByte(@truncate((offset >> 8) & 0xff));
        parser.emitByte(@truncate(offset & 0xff));
    }
    fn emitJump(parser: *ParserContext, instruction: OpCode) u8 {
        parser.emitOp(instruction);
        parser.emitByte(0xff);
        parser.emitByte(0xff);
        return @intCast(parser.currentChunk().code.items.len - 2);
    }
    fn patchJump(parser: *ParserContext, offset: usize) void {
        // std.debug.assert(offset <= std.math.maxInt(u8));
        const jump = parser.currentChunk().code.items.len - offset - 2;
        if (jump > std.math.maxInt(u16)) {
            std.debug.panic("Too much code to jump over.\n", .{});
        }
        parser.currentChunk().code.items[offset] = @intCast((jump >> 8) & 0xff);
        parser.currentChunk().code.items[offset + 1] = @intCast(jump & 0xff);
    }
    pub fn expressionStatement(parser: *ParserContext) Err!void {
        try parser.expression();
        try parser.consume(.tok_semicolon, "Expect ';' after value.");
        parser.emitOp(.op_pop);
    }
    fn emitConst(parser: *ParserContext, value: Value) void {
        parser.emitBytes(.op_constant, parser.makeConst(value));
    }
    fn makeConst(parser: *ParserContext, value: Value) u8 {
        const constant = parser.currentChunk().addConstant(value);
        if (constant > std.math.maxInt(u8)) {
            std.debug.panic("Too many constants in on chunk", .{});
        }
        return @intCast(constant);
    }

    pub fn resolveLocal(parser: *ParserContext, name: Token) ?u8 {
        const compiler = parser.currentCompiler.?;
        if (compiler.localCount <= 0) return null;
        var i = compiler.localCount - 1;
        while (i >= 0) : (i -= 1) {
            const local = compiler.locals[i];
            if (parser.identifiersEqual(name, local.name)) {
                if (local.depth == -1) {
                    std.debug.panic("Can't read local variable in its own initializer.\n", .{});
                }
                return @intCast(i);
            }
            if (i == 0) break;
        }
        return null;
    }
    fn tokenString(parser: *const ParserContext, tok: Token) []const u8 {
        return parser.scanner.source[tok.start .. tok.start + tok.length];
    }
    fn identifiersEqual(parser: *const ParserContext, a: Token, b: Token) bool {
        const astr = parser.tokenString(a);
        const bstr = parser.tokenString(b);
        return std.mem.eql(u8, astr, bstr);
    }
    fn currentChunk(parser: *const ParserContext) *Chunk {
        return &parser.currentCompiler.?.function.chunk;
    }
};

pub fn compile(collector: *Collector, source: []const u8, compilingchunk: *Chunk) Err!*ObjFunction {
    var scanner = Scanner.init(source);

    var compiler: Compiler = undefined;
    var parser = ParserContext.init(collector, &scanner, compilingchunk);
    parser.initCompiler(&compiler, .script);
    parser.advance();
    while (!parser.match(.tok_eof)) {
        try parser.declaration();
    }
    return parser.endCompiler();
}
