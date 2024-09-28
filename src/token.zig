pub const TokenTag = enum {
    ILLEGAL,
    EOF,

    IDENT,
    INT,

    ASSIGN, // =
    PLUS, // +

    COMMA, // ,
    SEMICOLON, // ;

    LPAREN, // (
    RPAREN, // )
    LBRACE, // {
    RBRACE, // }

    FUNCTION,
    LET,
};

pub const Token = union(TokenTag) {
    ILLEGAL: [:0]const u8,
    EOF: void,

    IDENT: void,
    INT: void,

    ASSIGN: void, // =
    PLUS: void, // +

    COMMA: void, // ,
    SEMICOLON: void, // ;

    LPAREN: void, // (
    RPAREN: void, // )
    LBRACE: void, // {
    RBRACE: void, // }

    FUNCTION: void,
    LET: void,
};
