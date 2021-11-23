
enum Token {
    case identifier(String), skip, assign, semicolon, `if`, then, `else`, `while`, `do`,
         intLiteral(Int), boolLiteral(Bool), intBinOp(IntBinOp), boolBinOp(BoolBinOp)
}


func lex(source: String) -> [Token] {
    source.split(separator: " ").map { rawToken in
        switch rawToken {
        case "skip": return .skip
        case ":=": return .assign
        case ";": return .semicolon
        case "if": return .`if`
        case "then": return .then
        case "else": return .else
        case "while": return .`while`
        case "do": return .`do`
        case let raw:
            let raw = String(raw)
            if let bool = Bool(raw) {
                return .boolLiteral(bool)
            }
            if let int = Int(raw) {
                return .intLiteral(int)
            }
            if let op = IntBinOp(rawValue: raw) {
                return .intBinOp(op)
            }
            if let op = BoolBinOp(rawValue: raw) {
                return .boolBinOp(op)
            }
            return .identifier(raw)
        }
    }
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

    // C ::= skip | x := E | C ; C | if B then C else C | while B do C
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
    

    // E ::= n | x | E iop E
    // ->
    // E  ::= n E2 | x E2
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
