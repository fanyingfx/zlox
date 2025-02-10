const std = @import("std");
const keywords = std.StaticStringMap(TokenType).initComptime(.{
    .{ "and", .kw_and },
    .{ "class", .kw_class },
    .{ "else", .kw_else },
    .{ "false", .kw_false },
    .{ "for", .kw_for },
    .{ "fun", .kw_fun },
    .{ "if", .kw_if },
    .{ "nil", .kw_nil },
    .{ "or", .kw_or },
    .{ "print", .kw_print },
    .{ "return", .kw_return },
    .{ "super", .kw_super },
    .{ "this", .kw_this },
    .{ "true", .kw_true },
    .{ "var", .kw_var },
    .{ "while", .kw_while },
    .{ "quit", .kw_quit },
    .{ "exit", .kw_quit },
});
pub const Scanner = struct {
    source: []const u8,
    start: usize,
    current: usize,
    line: usize,
    pub fn init(source: []const u8) Scanner {
        return .{
            .source = source,
            .start = 0,
            .current = 0,
            .line = 1,
        };
    }
    pub fn isAtEnd(self: *const Scanner) bool {
        return self.current >= self.source.len - 1;
    }
    pub fn makeToken(self: *const Scanner, type_: TokenType) Token {
        return .{
            .type = type_,
            .start = self.start,
            .length = self.current - self.start,
            .line = self.line,
        };
    }
    pub fn scanToken(self: *Scanner) Token {
        self.skipWhitespace();
        self.start = self.current;
        if (self.isAtEnd()) return self.makeToken(.tok_eof);
        const c = self.advance();
        switch (c) {
            '(' => return self.makeToken(.tok_left_paren),
            ')' => return self.makeToken(.tok_right_paren),
            '{' => return self.makeToken(.tok_left_brace),
            '}' => return self.makeToken(.tok_right_brace),
            ';' => return self.makeToken(.tok_semicolon),
            ',' => return self.makeToken(.tok_comma),
            '.' => return self.makeToken(.tok_dot),
            '-' => return self.makeToken(.tok_minus),
            '+' => return self.makeToken(if (self.match('+')) .tok_concat else .tok_plus),
            '/' => return self.makeToken(.tok_slash),
            '*' => return self.makeToken(.tok_star),
            '!' => return self.makeToken(if (self.match('=')) .tok_bang_equal else .tok_bang),
            '=' => return self.makeToken(if (self.match('=')) .tok_equal_equal else .tok_equal),
            '<' => return self.makeToken(if (self.match('=')) .tok_less_equal else .tok_less),
            '>' => return self.makeToken(if (self.match('=')) .tok_greater_equal else .tok_greater),
            '0'...'9' => return self.number(),
            'a'...'z', '_', 'A'...'Z' => return self.identifier(),
            '"' => return self.string(),
            else => {
                // std.debug.print("source = {s}\n", .{self.source[self.start..self.current]});
                std.debug.panic("'{c}' is unknown token!", .{c});
            },
        }
    }
    fn advance(self: *Scanner) u8 {
        self.current += 1;
        return self.source[self.current - 1];
    }
    fn match(self: *Scanner, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.current] != expected) return false;
        self.current += 1;
        return true;
    }
    fn peek(self: *Scanner) u8 {
        return self.source[self.current];
    }
    fn peekNext(self: *Scanner) u8 {
        if (self.current + 1 < self.source.len) {
            return self.source[self.current + 1];
        }
        std.debug.panic("can not peek next!", .{});
    }
    fn skipWhitespace(self: *Scanner) void {
        if (self.isAtEnd()) return;
        while (true) {
            const c = self.peek();
            // std.debug.print("{c}\n",.{c});
            switch (c) {
                ' ', '\r', '\t' => _ = self.advance(),
                '\n' => {
                    self.line += 1;
                    _ = self.advance();
                },
                '/' => {
                    if (self.peekNext() == '/') {
                        while (self.peek() != '\n' and !self.isAtEnd()) _ = self.advance();
                    } else {
                        return;
                    }
                },
                else => return,
            }
        }
    }
    fn string(self: *Scanner) Token {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') self.line += 1;
            _ = self.advance();
        }
        if (self.isAtEnd()) std.debug.panic("Unterminated string.", .{});
        _ = self.advance();
        return self.makeToken(.tok_string);
    }

    fn number(self: *Scanner) Token {
        while (isDigit(self.peek())) _ = self.advance();
        if (self.peek() == '.' and isDigit(self.peekNext())) {
            _ = self.advance();
            while (isDigit(self.peek())) _ = self.advance();
        }
        return self.makeToken(.tok_number);
    }
    fn identifier(self: *Scanner) Token {
        while (isAlpha(self.peek()) or isDigit(self.peek())) _ = self.advance();
        const idstr = self.source[self.start..self.current];
        const tag = if (keywords.get(idstr)) |tag| tag else .tok_identifier;
        return self.makeToken(tag);
    }
    fn isAlpha(c: u8) bool {
        return switch (c) {
            'a'...'z', 'A'...'Z', '_' => true,
            else => false,
        };
    }
    fn isDigit(c: u8) bool {
        return switch (c) {
            '0'...'9' => true,
            else => false,
        };
    }
};

pub const TokenType = enum {
    // Single-character tokens.
    tok_left_paren,
    tok_right_paren,
    tok_left_brace,
    tok_right_brace,
    tok_comma,
    tok_dot,
    tok_minus,
    tok_plus,
    tok_semicolon,
    tok_slash,
    tok_star,
    tok_concat,
    // One or two character tokens.
    tok_bang,
    tok_bang_equal,
    tok_equal,
    tok_equal_equal,
    tok_greater,
    tok_greater_equal,
    tok_less,
    tok_less_equal,
    // Literals.
    tok_identifier,
    tok_string,
    tok_number,
    // Keywords.
    kw_and,
    kw_class,
    kw_else,
    kw_false,
    kw_for,
    kw_fun,
    kw_if,
    kw_nil,
    kw_or,
    kw_print,
    kw_return,
    kw_super,
    kw_this,
    kw_true,
    kw_var,
    kw_while,
    kw_quit,

    tok_error,
    tok_eof,
};
pub const Token = struct {
    type: TokenType,
    start: usize,
    length: usize,
    line: usize,
};
