import Foundation
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
            if case .sequence = c1 {
                return "(\(c1)); \(c2)"
            } else if case .sequence = c2 {
                return "\(c1); (\(c2))"
            } else {
                return "\(c1); \(c2)"
            }
        case let .ifThenElse(condition, c1, c2):
            return "if \(condition) then \(c1) else \(c2)"
        case let .whileDo(condition, body):
            return "while \(condition) do \(body)"
        }
    }
}

enum IntBinOp: Character, Equatable, CustomStringConvertible {
    case plus  = "+",
         minus = "-",
         times = "×"
    var description: String {
        String(self.rawValue)
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

class PrettyPrinter {
    private(set) var buffer: String
    private var indentDepth: Int
    private let indentWidth: Int
    private var isFirstTextBlock: Bool

    init(indentDepth: Int = 0) {
        self.buffer = ""
        self.indentDepth = indentDepth
        self.indentWidth = 2
        self.isFirstTextBlock = true
    }

    func indent(_ fn: () -> Void) {
        self.indentDepth += self.indentWidth
        fn()
        self.indentDepth -= self.indentWidth
    }

    func text(_ str: String) {
        if (self.isFirstTextBlock) {
            self.buffer += String(repeating: " ", count: self.indentDepth)
            self.isFirstTextBlock = false
        }
        self.buffer += str
    }

    func line(_ str: String) {
        self.buffer += String(repeating: " ", count: self.indentDepth) + str + "\n"
        self.isFirstTextBlock = true
    }

    func breakLine() {
        self.buffer += "\n"
        self.isFirstTextBlock = true
    }

    private(set) var stateSubsts: [State: String] = [:]
    func printState(_ state: State) -> String {
        if let subst = stateSubsts[state] {
            return subst
        }
        let newSubst = "\\sigma_{\(stateSubsts.count)}"
        stateSubsts[state] = newSubst
        return newSubst
    }

    func printStateSubst(state: State, subst: String) -> String {
        var out = "\(subst) = \\{"
        out += state.map { (key, value) in
            "\(key) \\mapsto \(value)"
        }.joined(separator: ", ")
        out += "\\}"
        return out
    }

    func printIntExpr(_ expr: IntExpr) -> String {
        switch expr {
        case .literal(let z): return z.description
        case .variable(let `var`): return `var`
        case .binop(let op, let lhs, let rhs):
            return "\(printIntExpr(lhs)) \(op) \(printIntExpr(rhs))"
        }
    }

    func printBoolExpr(_ expr: BoolExpr) -> String {
        switch expr {
        case .literal(let b): return b.description
        case .binop(let op, let lhs, let rhs):
            return "\(printIntExpr(lhs)) \(printBoolBinOp(op)) \(printIntExpr(rhs))"
        }
    }

    func printBoolBinOp(_ op: BoolBinOp) -> String {
        switch op {
        case .equal: return #"="#
        case .greaterThan: return #">"#
        case .lessThan: return #"<"#
        case .greaterThanOrEqual: return #"\geq"#
        case .lessThanOrEqual: return #"\leq"#
        }
    }

    func printCommand(_ command: Command) -> String {
        switch command {
        case .skip: return "skip"
        case .assign(x: let x, expr: let expr):
            return "\(x) := \(printIntExpr(expr))"
        case .sequence(let c1, let c2):
            if case .sequence = c1 {
                return "(\(printCommand(c1)); \(printCommand(c2))"
            } else if case .sequence = c2 {
                return "\(printCommand(c1)); (\(printCommand(c2))"
            } else {
                return "\(printCommand(c1)); \(printCommand(c2))"
            }
        case let .ifThenElse(condition, c1, c2):
            return "if \(printBoolExpr(condition)) then \(printCommand(c1)) else \(printCommand(c2))"
        case let .whileDo(condition, body):
            return "while \(printBoolExpr(condition)) do \(printCommand(body))"
        }
    }

    func printTree(deriviation: Deriviation) {
        let base: String
        var needsBar = true
        switch deriviation.content {
        case .arithmeticExpr(let source, let state, let int):
            base = "\\langle \\mathrm{\(printIntExpr(source))}, \(printState(state)) \\rangle \\rightarrow \(int)"
        case .booleanExpr(let source, let state, let bool):
            base = "\\langle \\mathrm{\(printBoolExpr(source))}, \(printState(state)) \\rangle \\rightarrow \\mathrm{\(bool)}"
        case .command(let source, let state, let state2):
            let source = printCommand(source).replacingOccurrences(of: " ", with: "\\,")
            base = "\\langle \\mathrm{\(source)}, \(printState(state)) \\rangle \\rightarrow \(printState(state2))"
        case .label(let string):
            base = string
            needsBar = false
        }
        if needsBar {
            self.line("\\infer[\(deriviation.name)]{\(base)}{")
            if !deriviation.parents.isEmpty {
                self.indent {
                    for (idx, parent) in deriviation.parents.enumerated() {
                        printTree(deriviation: parent)
                        if idx != deriviation.parents.count - 1 {
                            self.line("&")
                        }
                    }
                }
            }
            self.line("}")
        } else {
            self.line(base)
        }
    }
}

struct Deriviation {
    enum Content {
        case arithmeticExpr(source: IntExpr, state: State, Int)
        case booleanExpr(source: BoolExpr, state: State, Bool)
        case command(source: Command, state: State, State)
        case label(String)
    }
    var name: String
    var content: Content
    var parents: [Deriviation]

    init(parents: [Deriviation], name: String, content: Content) {
        self.name = name
        self.content = content
        self.parents = parents
    }
}

func arithmeticRule(expr: IntExpr, state: State) -> (Z, Deriviation) {
    let v: Int
    let name: String
    let parents: [Deriviation]
    switch expr {
    case .literal(let x):
        v = x
        name = "num"
        parents = []
    case .variable(let x):
        v = state[x]!
        name = "var"
        parents = []
    case .binop(op: let op, lhs: let lhs, rhs: let rhs):
        let (lhs, lhsDeriv) = arithmeticRule(expr: lhs, state: state)
        let (rhs, rhsDeriv) = arithmeticRule(expr: rhs, state: state)
        switch op {
        case .plus:
            v = lhs + rhs
            name = "sum"
        case .minus:
            v = lhs - rhs
            name = "sub"
        case .times:
            v = lhs * rhs
            name = "mul"
        }
        let binopDeriv = Deriviation(parents: [], name: "", content: .label("\(lhs) \(op) \(rhs) = \(v)"))
        parents = [lhsDeriv, rhsDeriv, binopDeriv]
    }
    return (v, Deriviation(parents: parents, name: name, content: .arithmeticExpr(source: expr, state: state, v)))
}

func booleanRule(expr: BoolExpr, state: State) -> (B, Deriviation) {
    let v: Bool
    let name: String
    let parents: [Deriviation]
    switch expr {
    case .literal(let x):
        v = x
        name = "\(x)"
        parents = []
    case .binop(op: let op, lhs: let lhs, rhs: let rhs):
        let (lhs, lhsDeriv) = arithmeticRule(expr: lhs, state: state)
        let (rhs, rhsDeriv) = arithmeticRule(expr: rhs, state: state)
        let label: String
        let baseName: String
        switch op {
        case .equal:
            v = lhs == rhs
            label = v ? "\(lhs) = \(rhs)" : "\(lhs) != \(rhs)"
            baseName = "eq"
        case .greaterThan:
            v = lhs > rhs
            label = v ? "\(lhs) > \(rhs)" : "\(lhs) \\leq \(rhs)"
            baseName = "gt"
        case .lessThan:
            v = lhs < rhs
            label = v ? "\(lhs) < \(rhs)" : "\(lhs) \\geq \(rhs)"
            baseName = "lt"
        case .greaterThanOrEqual:
            v = lhs >= rhs
            label = v ? "\(lhs) \\geq \(rhs)" : "\(lhs) < \(rhs)"
            baseName = "ge"
        case .lessThanOrEqual:
            v = lhs <= rhs
            label = v ? "\(lhs) \\leq \(rhs)" : "\(lhs) > \(rhs)"
            baseName = "le"
        }
        let binopDeriv = Deriviation(parents: [], name: "", content: .label(label))
        parents = [lhsDeriv, rhsDeriv, binopDeriv]
        name = "\(baseName)_\(v ? "t" : "f")"
    }
    return (v, Deriviation(parents: parents, name: name, content: .booleanExpr(source: expr, state: state, v)))
}

func commandRule(command: Command, state: State) -> (State, Deriviation) {
    var newState = state
    let name: String
    var parents: [Deriviation] = []
    switch command {
    case .skip:
        name = "skip"
    case .assign(x: let x, expr: let expr):
        name = "upd"
        let (v, exprDeriv) = arithmeticRule(expr: expr, state: state)
        newState[x] = v
        parents = [exprDeriv]
    case .sequence(let c0, let c1):
        name = "seq"
        let (state1, c0Deriv) = commandRule(command: c0, state: state)
        let (state2, c1Deriv) = commandRule(command: c1, state: state1)
        newState = state2
        parents = [c0Deriv, c1Deriv]
    case .ifThenElse(condition: let condition, then: let c0, else: let c1):
        let (cond, condDeriv) = booleanRule(expr: condition, state: state)
        let bodyDeriv: Deriviation
        if cond {
            name = "if_t"
            (newState, bodyDeriv) = commandRule(command: c0, state: state)
        } else {
            name = "if_f"
            (newState, bodyDeriv) = commandRule(command: c1, state: state)
        }
        parents = [condDeriv, bodyDeriv]
    case .whileDo(condition: let condition, body: let body):
        let (cond, condDeriv) = booleanRule(expr: condition, state: state)
        if cond {
            name = "wh_t"
            let (state1, bodyDeriv) = commandRule(command: body, state: state)
            let (state2, nextIterDeriv) = commandRule(command: command, state: state1)
            parents = [condDeriv, bodyDeriv, nextIterDeriv]
            newState = state2
        } else {
            name = "wh_f"
            parents = [condDeriv]
            newState = state
        }
    }
    return (newState, Deriviation(parents: parents, name: name, content: .command(source: command, state: state, newState)))
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

func buildDeriviation(config: Config) -> Deriviation {
    let deriv: Deriviation
    switch config.phrase {
    case .boolExpr(let bExpr):
        (_, deriv) = booleanRule(expr: bExpr, state: config.state)
    case .intExpr(let aExpr):
        (_, deriv) = arithmeticRule(expr: aExpr, state: config.state)
    case .command(let command):
        (_, deriv) = commandRule(command: command, state: config.state)
    }
    return deriv
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
