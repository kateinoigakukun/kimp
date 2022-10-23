
enum Token {
    case identifier(String), skip, assign, semicolon, `if`, then, `else`, `while`, `do`,
         leftParen, rightParen,
         intLiteral(Int), boolLiteral(Bool), intBinOp(IntBinOp), boolBinOp(BoolBinOp)
}

enum LexError: Error {
    case unexpected(Character, expected: Character, index: String.Index)
}

func lex(source: String) throws -> [Token] {
    var cursor = source.startIndex
    var current: Character { source[cursor] }
    var isEOS: Bool { cursor >= source.endIndex }
    func peekNext() -> Character {
        return source[source.index(after: cursor)]
    }
    func advance(offsetBy offset: String.IndexDistance = 1) {
        cursor = source.index(cursor, offsetBy: offset)
    }

    /// Match [a-zA-Z]+
    func lexIdentifier() -> Token {
        let start = cursor
        func isValid(_ c: Character) -> Bool {
            return (c >= "a" && c <= "z") || (c >= "A" && c <= "Z") || c == "'"
        }
        while (!isEOS && isValid(current)) {
            advance()
        }
        let end = cursor
        let ident = String(source[start..<end])

        if let bool = Bool(ident) {
            return .boolLiteral(bool)
        } else if ident == "skip" {
            return .skip
        } else if ident == "if" {
            return .`if`
        } else if ident == "then" {
            return .then
        } else if ident == "else" {
            return .else
        } else if ident == "while" {
            return .`while`
        } else if ident == "do" {
            return .do
        }
        return .identifier(ident)
    }

    func lexNumber() -> Int {
        let start = cursor
        func isValid(_ c: Character) -> Bool {
            return c >= "0" && c <= "9"
        }
        while (!isEOS && isValid(current)) {
            advance()
        }
        let end = cursor
        return Int(source[start..<end])!
    }

    var tokens: [Token] = []
    while !isEOS {
        switch current {
        case ":":
            guard peekNext() == "=" else {
                throw LexError.unexpected(peekNext(), expected: "=", index: cursor)
            }
            tokens.append(.assign)
            advance(offsetBy: 2)
        case ";":
            tokens.append(.semicolon)
            advance()
        case "(":
            tokens.append(.leftParen)
            advance()
        case ")":
            tokens.append(.rightParen)
            advance()
        case "=":
            tokens.append(.boolBinOp(.equal))
            advance()
        case ">":
            if peekNext() == "=" {
                tokens.append(.boolBinOp(.greaterThanOrEqual))
                advance(offsetBy: 2)
            } else {
                tokens.append(.boolBinOp(.greaterThan))
                advance()
            }
        case "<":
            if peekNext() == "=" {
                tokens.append(.boolBinOp(.lessThanOrEqual))
                advance(offsetBy: 2)
            } else {
                tokens.append(.boolBinOp(.lessThan))
                advance()
            }
        case " ", "\t", "\n":
            advance()
            break
        case "0"..."9":
            tokens.append(.intLiteral(lexNumber()))
        default:
            if let op = IntBinOp(rawValue: current) {
                tokens.append(.intBinOp(op))
                advance()
            } else {
                tokens.append(lexIdentifier())
            }
        }
        guard !isEOS else { break }
    }
    return tokens
}

/// Recursive-descent parser
struct Parser {
    enum StackValue {
        case term(Token)
        case phrase(Phrase)
    }
    struct Error: Swift.Error {
        let message: String
        init(_ message: String) {
            self.message = message
        }
    }
    
    let tokens: [Token]
    var cursorIndex: Int
    var currentToken: Token {
        tokens[cursorIndex]
    }

    var isEOF: Bool {
        cursorIndex == tokens.endIndex
    }
    
    init(tokens: [Token]) {
        self.tokens = tokens
        self.cursorIndex = tokens.startIndex
    }

    @discardableResult
    mutating func consumeToken() throws -> Token {
        guard cursorIndex < tokens.endIndex else {
            throw Error("expected next token")
        }
        let nextIndex = tokens.index(after: self.cursorIndex)
        let current = tokens[self.cursorIndex]
        self.cursorIndex = nextIndex
        return current
    }

    // C ::= skip | x := E | C ; C | if B then C else C | while B do C | (C)
    // ->
    // C  ::= skip C2 | x := E C2 | if B then C else C C2 | while B do C C2
    // C2 ::= ε | ; C
    mutating func parseCommand() throws -> Command {
        func parseCommand2() throws -> (_ c1: Command) -> (Command) {
            if isEOF { return { $0 } }
            switch currentToken {
            case .semicolon:
                try consumeToken()
                let c2 = try parseCommand()
                return { c1 in
                    return .sequence(c1, c2)
                }
            default:
                return { $0 }
            }
        }

        let c1: Command
        switch currentToken {
        case .leftParen:
            try consumeToken()
            let c = try parseCommand()
            guard case .rightParen = try consumeToken() else { throw Error("expected )") }
            c1 = c
        case .skip:
            try consumeToken()
            c1 = .skip
        case .if:
            try consumeToken()
            let cond = try parseBoolExpr()
            guard case .then = try consumeToken() else { throw Error("expected then") }
            let thenBody = try parseCommand()
            guard case .`else` = try consumeToken() else { throw Error("expected else") }
            let elseBody = try parseCommand()
            c1 = .ifThenElse(condition: cond, then: thenBody, else: elseBody)
        case .while:
            try consumeToken()
            let cond = try parseBoolExpr()
            guard case .`do` = try consumeToken() else { throw Error("expected do") }
            let body = try parseCommand()
            c1 = .whileDo(condition: cond, body: body)
        case .identifier(let x):
            // for `x := E`
            try consumeToken()
            guard case .assign = try consumeToken() else {
                throw Error("expected :=")
            }
            let lvalue = try parseIntExpr()
            c1 = .assign(x: x, expr: lvalue)
        default:
            throw Error("unexpected '\(currentToken)' expecting command")
        }
        return try parseCommand2()(c1)
    }
    

    // E ::= n | -n | x | E iop E | (E)
    // ->
    // E  ::= n E2 | -n E2 | x E2 | (E) E2
    // E2 ::= ε | iop E
    mutating func parseIntExpr() throws -> IntExpr {
        func parseIntExpr2() throws -> (_ lhs: IntExpr) -> (IntExpr) {
            if isEOF { return { $0 } }
            switch currentToken {
            case .intBinOp(let intBinOp):
                try consumeToken()
                let rhs = try parseIntExpr()
                return { lhs in
                    return .binop(op: intBinOp, lhs: lhs, rhs: rhs)
                }
            default:
                return { $0 }
            }
        }

        let lhs: IntExpr
        switch currentToken {
        case .identifier(let name):
            try consumeToken()
            lhs = .variable(name)
        case .intLiteral(let v):
            try consumeToken()
            lhs = .literal(v)
        case .leftParen:
            try consumeToken()
            lhs = try parseIntExpr()
            guard case .rightParen = try consumeToken() else { throw Error("expected )") }
        case .intBinOp(.minus):
            try consumeToken()
            guard case let .intLiteral(v) = try consumeToken() else { throw Error("expected int literal") }
            lhs = .literal(-v)
        default:
            throw Error("unexpected '\(currentToken)' expecting expression")
        }
        return try parseIntExpr2()(lhs)
    }

    // B ::= b | E bop E
    mutating func parseBoolExpr() throws -> BoolExpr {
        switch currentToken {
        case .boolLiteral(let v):
            try consumeToken()
            return .literal(v)
        default:
            let lhs = try parseIntExpr()
            guard case let .boolBinOp(op) = try consumeToken() else {
                throw Error("unexpected '\(currentToken)' expecting bool bin op")
            }
            let rhs = try parseIntExpr()
            return .binop(op: op, lhs: lhs, rhs: rhs)
        }
    }
}
