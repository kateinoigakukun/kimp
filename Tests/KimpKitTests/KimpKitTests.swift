import XCTest
@testable import KimpKit

final class KimpKitTests: XCTestCase {
    func testParse() throws {
        let program = "while x > 0 do x' := x × x' ; x := x - 1"
        var parser = Parser(tokens: lex(source: program))
        let command = try parser.parseCommand()
        XCTAssertEqual(command, .whileDo(
            condition: .binop(op: .greaterThan, lhs: .variable("x"), rhs: .literal(0)),
            body: .sequence(
                .assign(
                    x: "x'",
                    expr: .binop(
                        op: .times, lhs: .variable("x"), rhs: .variable("x'")
                    )
                ),
                .assign(
                    x: "x",
                    expr: .binop(op: .minus, lhs: .variable("x"), rhs: .literal(1))
                )
            )
        ))
        XCTAssertEqual(parser.cursorIndex, parser.tokens.endIndex)
    }

    func testEval() throws {
        var config = Config(
            phrase: .command(
                .whileDo(
                    condition: .binop(op: .greaterThan, lhs: .variable("x"), rhs: .literal(0)),
                    body: .sequence(
                        .assign(
                            x: "x'",
                            expr: .binop(
                                op: .times, lhs: .variable("x"), rhs: .variable("x'")
                            )
                        ),
                        .assign(
                            x: "x",
                            expr: .binop(op: .minus, lhs: .variable("x"), rhs: .literal(1))
                        )
                    )
                )
            ),
            state: [
                "x": 4,
                "x'": 1
            ]
        )

        eval(config: &config)
        XCTAssertEqual(config.phrase, .command(.skip))
        XCTAssertEqual(config.state["x"], 0)
        XCTAssertEqual(config.state["x'"], 24)
    }

    func testSteps() throws {
        let program = "( if x <= y then z := y ; ( y := x ; x := z ) else skip ) ; ( z := 3 ; while 0 <= x - y do ( z := z × z ; y := y + y ) )"
        var parser = Parser(tokens: lex(source: program))
        let command = try parser.parseCommand()
        XCTAssertEqual(parser.cursorIndex, parser.tokens.endIndex)
        let state = [
            "x": 1, "y": 10, "z": 0
        ]
        let config = Config(phrase: .command(command), state: state)
        let steps = evalSteps(config: config)
        let singleStep = steps.next()!
        for step in singleStep {
            print(step)
        }
//        XCTAssertEqual(steps.next()?.ruleName, "var")
    }
}
