typealias B = Bool
typealias Z = Int
typealias Var = String

enum Phrase: Equatable, CustomStringConvertible {
    case command(Command)
    case intExpr(IntExpr)
    case boolExpr(BoolExpr)
    var description: String {
        switch self {
        case let .command(c): return c.description
        case let .intExpr(e): return e.description
        case let .boolExpr(b): return b.description
        }
    }
}

indirect enum Command: Equatable, CustomStringConvertible {
    case skip
    case assign(x: Var, expr: IntExpr)
    case sequence(Command, Command)
    case ifThenElse(condition: BoolExpr, then: Command, `else`: Command)
    case whileDo(condition: BoolExpr, body: Command)

    var description: String {
        switch self {
        case .skip: return "skip"
        case .assign(x: let x, expr: let expr):
            return "\(x) := \(expr)"
        case .sequence(let c1, let c2):
            return "\(c1); \(c2)"
        case let .ifThenElse(condition, c1, c2):
            return "if \(condition) then \(c1) else \(c2)"
        case let .whileDo(condition, body):
            return "while \(condition) do \(body)"
        }
    }
}

enum IntBinOp: String, Equatable, CustomStringConvertible {
    case plus  = "+",
         minus = "-",
         times = "×"
    var description: String {
        self.rawValue
    }
}

indirect enum IntExpr: Equatable, CustomStringConvertible {
    case literal(Z)
    case variable(Var)
    case binop(op: IntBinOp, lhs: IntExpr, rhs: IntExpr)
    
    var description: String {
        switch self {
        case .literal(let n): return n.description
        case .variable(let v): return v
        case .binop(let op, let lhs, let rhs):
            return "\(lhs) \(op) \(rhs)"
        }
    }
}

enum BoolBinOp: String, Equatable, CustomStringConvertible {
    case equal              = "=",
         greaterThan        = ">",
         lessThan           = "<",
         greaterThanOrEqual = ">=",
         lessThanOrEqual    = "<="
    var description: String {
        self.rawValue
    }
}

enum BoolExpr: Equatable, CustomStringConvertible {
    case literal(B)
    case binop(op: BoolBinOp, lhs: IntExpr, rhs: IntExpr)
    var description: String {
        switch self {
        case .literal(let b): return b.description
        case .binop(let op, let lhs, let rhs):
            return "\(lhs) \(op) \(rhs)"
        }
    }
}

typealias State = [Var: Z]

struct Config {
    var phrase: Phrase
    var state: State
}

extension Config: CustomStringConvertible {
    var description: String {
        "〈 \(phrase), \(state) 〉"
    }
}

protocol Rule {
    func apply<T>(config: inout Config, rules: [Rule], returning: (Config) -> T?) -> T?
}

func expectIntExpr(config: Config) -> (IntExpr, State)? {
    if case let .intExpr(expr) = config.phrase {
        return (expr, config.state)
    } else {
        return nil
    }
}

func expectBoolExpr(config: Config) -> (BoolExpr, State)? {
    if case let .boolExpr(expr) = config.phrase {
        return (expr, config.state)
    } else {
        return nil
    }
}

func expectCommand(config: Config) -> (Command, State)? {
    if case let .command(c) = config.phrase {
        return (c, config.state)
    } else {
        return nil
    }
}

func trySimplify<T>(rules: [Rule], config: Config, returning: (Config) -> T?) -> T? {
    for rule in rules {
        var config = config
        let result: T? = rule.apply(config: &config, rules: rules, returning: returning)
        if let result = result {
            return result
        }
    }
    return nil
}

let rules: [(String, (inout Config, [Rule]) -> Bool)] = [
    ("var", { config, _ in
        guard case let .intExpr(.variable(x)) = config.phrase else {
            return false
        }
        guard let v = config.state[x] else {
            return false
        }
        config.phrase = .intExpr(.literal(v))
        return true
    }),
    ("op1", { config, rules in
        switch config.phrase {
        case let .boolExpr(.binop(op, lhs, rhs)):
            let subConfig = Config(phrase: .intExpr(lhs), state: config.state)
            guard let (newLhs, newState) = trySimplify(rules: rules, config: subConfig, returning: expectIntExpr) else {
                return false
            }
            config.phrase = .boolExpr(.binop(op: op, lhs: newLhs, rhs: rhs))
            config.state = newState
        case let .intExpr(.binop(op, lhs, rhs)):
            let subConfig = Config(phrase: .intExpr(lhs), state: config.state)
            guard let (newLhs, newState) = trySimplify(rules: rules, config: subConfig, returning: expectIntExpr) else {
                return false
            }
            config.phrase = .intExpr(.binop(op: op, lhs: newLhs, rhs: rhs))
            config.state = newState
        default:
            return false
        }
        return true
    }),
    ("op2", { config, rules in
        switch config.phrase {
        case let .boolExpr(.binop(op, .literal(n1), rhs)):
            let subConfig = Config(phrase: .intExpr(rhs), state: config.state)
            guard let (newRhs, newState) = trySimplify(rules: rules, config: subConfig, returning: expectIntExpr) else {
                return false
            }
            config.phrase = .boolExpr(.binop(op: op, lhs: .literal(n1), rhs: newRhs))
            config.state = newState
        case let .intExpr(.binop(op, .literal(n1), rhs)):
            let subConfig = Config(phrase: .intExpr(rhs), state: config.state)
            guard let (newRhs, newState) = trySimplify(rules: rules, config: subConfig, returning: expectIntExpr) else {
                return false
            }
            config.phrase = .intExpr(.binop(op: op, lhs: .literal(n1), rhs: newRhs))
            config.state = newState
        default:
            return false
        }
        return true
    }),
    ("op3", { config, rules in
        switch config.phrase {
        case let .intExpr(.binop(op, .literal(n1), .literal(n2))):
            let v: Z
            switch op {
            case .minus:
                v = n1 - n2
            case .plus:
                v = n1 + n2
            case .times:
                v = n1 * n2
            }
            config.phrase = .intExpr(.literal(v))
        case let .boolExpr(.binop(op, .literal(n1), .literal(n2))):
            let v: B
            switch op {
            case .equal:
                v = n1 == n2
            case .greaterThan:
                v = n1 > n2
            case .lessThan:
                v = n1 < n2
            case .greaterThanOrEqual:
                v = n1 >= n2
            case .lessThanOrEqual:
                v = n1 <= n2
            }
            config.phrase = .boolExpr(.literal(v))
        default:
            return false
        }
        return true
    }),
    ("set1", { config, rules in
        guard case let .command(.assign(x, expr)) = config.phrase else {
            return false
        }
        let subConfig = Config(phrase: .intExpr(expr), state: config.state)
        guard let (newExpr, newState) = trySimplify(rules: rules, config: subConfig, returning: expectIntExpr) else {
            return false
        }
        config.phrase = .command(.assign(x: x, expr: newExpr))
        config.state = newState
        return true
    }),
    ("set2", { config, rules in
        guard case let .command(.assign(x, .literal(n))) = config.phrase else {
            return false
        }
        config.phrase = .command(.skip)
        config.state[x] = n
        return true
    }),
    ("seq1", { config, rules in
        guard case let .command(.sequence(c1, c2)) = config.phrase else {
            return false
        }
        let subConfig = Config(phrase: .command(c1), state: config.state)
        guard let (newCommand, newState) = trySimplify(rules: rules, config: subConfig, returning: expectCommand) else {
            return false
        }
        config.phrase = .command(.sequence(newCommand, c2))
        config.state = newState
        return true
    }),
    ("seq2", { config, rules in
        guard case let .command(.sequence(.skip, c)) = config.phrase else {
            return false
        }
        config.phrase = .command(c)
        return true
    }),
    ("if1", { config, rules in
        guard case let .command(.ifThenElse(condition: b, then: c1, else: c2)) = config.phrase else {
            return false
        }
        let subConfig = Config(phrase: .boolExpr(b), state: config.state)
        guard let (newExpr, newState) = trySimplify(rules: rules, config: subConfig, returning: expectBoolExpr) else {
            return false
        }
        config.phrase = .command(.ifThenElse(condition: newExpr, then: c1, else: c2))
        config.state = newState
        return true
    }),
    ("if2", { config, rules in
        guard case let .command(.ifThenElse(condition: .literal(true), then: c1, else: _)) = config.phrase else {
            return false
        }
        config.phrase = .command(c1)
        return true
    }),
    ("if3", { config, rules in
        guard case let .command(.ifThenElse(condition: .literal(false), then: _, else: c2)) = config.phrase else {
            return false
        }
        config.phrase = .command(c2)
        return true
    }),
    ("whl", { config, rules in
        guard case let .command(.whileDo(b, c)) = config.phrase else {
            return false
        }
        config.phrase = .command(
            .ifThenElse(
                condition: b,
                then: .sequence(
                    c,
                    .whileDo(condition: b, body: c)
                ),
                else: .skip
            )
        )
        return true
    }),
]

struct Step: CustomStringConvertible {
    let ruleName: String
    let before: Config
    let after: Config

    var description: String {
        "(\(ruleName)) \(before) → \(after)"
    }
}

func evalSteps(config: Config) -> AnyIterator<[Step]> {
    class Tracer {
        var steps: [Step] = []
    }
    struct StepTracingRule: Rule {
        let name: String
        let f: (inout Config, [Rule]) -> Bool
        let tracer: Tracer

        func apply<T>(config: inout Config, rules: [Rule], returning: (Config) -> T?) -> T? {
            let oldConfig = config
            let applied = f(&config, rules)
            guard applied else {
                return nil
            }
            guard let result = returning(config) else {
                return nil
            }
            tracer.steps.append(Step(ruleName: name, before: oldConfig, after: config))
            return result
        }
    }

    var config = config
    return AnyIterator<[Step]> {
        let tracer = Tracer()
        let wrappedRules = rules.map { StepTracingRule(name: $0, f: $1, tracer: tracer) }

        for rule in wrappedRules {
            if rule.apply(config: &config, rules: wrappedRules, returning: { _ in () }) != nil {
                return tracer.steps
            }
        }
        return nil
    }
}

func eval(config: inout Config) {
    struct FRule: Rule {
        let name: String
        let f: (inout Config, [Rule]) -> Bool
        func apply<T>(config: inout Config, rules: [Rule], returning: (Config) -> T?) -> T? {
            let applied = f(&config, rules)
            guard applied else {
                return nil
            }
            guard let result = returning(config) else {
                return nil
            }
            return result
        }
    }
    let wrappedRules = rules.map { FRule(name: $0, f: $1) }
evalLoop:
    while true {
        for rule in wrappedRules {
            if rule.apply(config: &config, rules: wrappedRules, returning: { _ in () }) != nil {
                continue evalLoop
            }
        }
        break
    }
}
