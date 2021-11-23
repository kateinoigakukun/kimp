# KimpKit

kateinoigakukun's IMP language implementation.

```swift
import KimpKit

let program = "while x > 0 do x' := x × x' ; x := x - 1"
var parser = Parser(tokens: lex(source: program))
let command = try parser.parseCommand()
var config = Config(phrase: .command(command), state: [
    "x": 4,
    "x'": 1,
])

eval(config: &config)

XCTAssertEqual(config.phrase, .command(.skip))
XCTAssertEqual(config.state["x"], 0)
XCTAssertEqual(config.state["x'"], 24)
```

