const std = @import("std");
const Scanner = @import("scanner.zig").Scanner;
const Chunk = @import("common.zig").Chunk;
const OpCode = @import("common.zig").OpCode;
const Value = @import("value.zig").Value;
const Token = @import("scanner.zig").Token;
const TokenType = @import("scanner.zig").TokenType;

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
fn getPrecedence(token_type: TokenType) Prececdence {
    return switch (token_type) {
        .tok_equal => .prec_assignment,
        .tok_minus, .tok_plus => .prec_term,
        .tok_slash, .tok_star => .prec_factor,
        else => .prec_none,
    };
}
const ParseFn = *const fn ( *Parser) void;

pub const Parser = struct {
    current: Token,
    previous: Token,
    scanner: *Scanner,
    compilingChunk: *Chunk,

    pub fn init(scanner: *Scanner, chunk: *Chunk) Parser {
        return .{
            .current = undefined,
            .previous = undefined,
            .scanner = scanner,
            .compilingChunk = chunk,
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
        // parser.chunk.write(byte, parser.previous.line);
        parser.emitOp(op);
        parser.emitByte(byte);
    }
    fn emitOp(parser: *Parser, op: OpCode) void {
        parser.currentChunk().writeOp(op, parser.previous.line);
    }
    pub fn endCompiler(parser: *Parser) void {
        parser.emitOp(.op_return);
    }

    pub fn parseExpression(parser: *Parser) void {
        parser.parsePrecedence(.prec_assignment);
    }
    fn getPrefixFn(token_type: TokenType) ParseFn {
        return switch (token_type) {
            .tok_minus => parseUnary,
            .tok_number => parseNumber,
            .tok_left_paren => parseGroup,
            else => undefined,
        };
    }
    fn getInfixFn(token_type: TokenType) ParseFn {
        return switch (token_type) {
            .tok_plus, .tok_minus, .tok_slash, .tok_star =>  parseBinary,
            else => undefined,
        };
    }
    fn parsePrecedence(parser: *Parser, prec: Prececdence) void {
        _ = parser.advance();
        const prefixFn = getPrefixFn(parser.previous.type);
        prefixFn(parser);
        while (prec.value() <= getPrecedence(parser.current.type).value()) {
            _ = parser.advance();
            const infixFn = getInfixFn(parser.previous.type);
            infixFn(parser);
        }
    }
    fn parseNumber(parser: *Parser) void {
        const number: Value = std.fmt.parseFloat(f64, parser.tokenString(parser.previous)) catch unreachable;
        parser.emitConst(number);
    }
    fn parseGroup(parser: *Parser) void {
        parser.parseExpression();
        parser.consume(.tok_right_paren, "Expect ')' after expression.");
    }
    fn parseUnary(parser: *Parser) void {
        parser.parsePrecedence(.prec_unary);
    }
    fn parseBinary(parser: *Parser) void {
        const operatorType = parser.previous.type;
        const prec = getPrecedence(operatorType);
        parser.parsePrecedence(prec.inc());
        switch (operatorType) {
            .tok_plus => parser.emitOp(.op_add),
            .tok_minus => parser.emitOp(.op_substract),
            .tok_star => parser.emitOp(.op_multiply),
            .tok_slash => parser.emitOp(.op_divide),
            else => unreachable,
        }
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
    fn tokenString(parser: *const Parser, tok: Token) []const u8 {
        return parser.scanner.source[tok.start .. tok.start + tok.length];
    }
    fn currentChunk(parser: *const Parser) *Chunk {
        return parser.compilingChunk;
    }
};

pub fn compile(source: []const u8, chunk: *Chunk) void {
    var scanner = Scanner.init(source);
    var parser = Parser.init(&scanner, chunk);
    parser.advance();
    // parser.advance();
    // std.debug.print("{any}\n", .{parser.current.type});
    parser.parseExpression();
    parser.consume(.tok_eof, "Expected end of expression.");
    parser.endCompiler();
}
