// Keywords

package Foo // EXPECTED: "package" swift-mode:keyword-face "Foo" swift-mode:type-face

import Foo // EXPECTED: "import" swift-mode:keyword-face "Foo" swift-mode:type-face

@objc
protocol Foo<T>: Foo where T: Foo { // EXPECTED: "protocol" swift-mode:keyword-face "Foo" swift-mode:type-face "<" swift-mode:operator-face "T" swift-mode:type-face ">" swift-mode:operator-face ":" swift-mode:delimiter-face "Foo" swift-mode:type-face "where" swift-mode:keyword-face "T" swift-mode:type-face ":" swift-mode:delimiter-face "Foo" swift-mode:type-face "{" swift-mode:bracket-face
    associatedtype Foo: Foo // EXPECTED: "associatedtype" swift-mode:keyword-face "Foo" swift-mode:type-face ":" swift-mode:delimiter-face "Foo" swift-mode:type-face
    typealias Foo: Foo // EXPECTED: "typealias" swift-mode:keyword-face "Foo" swift-mode:type-face ":" swift-mode:delimiter-face "Foo" swift-mode:type-face
    optional static var foo: Int { get; set; } // EXPECTED: "optional" swift-mode:keyword-face "static" swift-mode:keyword-face "var" swift-mode:keyword-face ":" swift-mode:delimiter-face "Int" swift-mode:builtin-type-face "{" swift-mode:bracket-face "get" swift-mode:keyword-face ";" swift-mode:delimiter-face "set" swift-mode:keyword-face ";" swift-mode:delimiter-face "}" swift-mode:bracket-face
}

class Foo: Foo { // EXPECTED: "class" swift-mode:keyword-face "Foo" swift-mode:type-face ":" swift-mode:delimiter-face "Foo" swift-mode:type-face "{" swift-mode:bracket-face
    lazy var foo = Foo() // EXPECTED: "lazy" swift-mode:keyword-face "var" swift-mode:keyword-face "=" swift-mode:operator-face "Foo" swift-mode:type-face "()" swift-mode:bracket-face
    weak var foo = Foo() // EXPECTED: "weak" swift-mode:keyword-face "var" swift-mode:keyword-face "=" swift-mode:operator-face "Foo" swift-mode:type-face "()" swift-mode:bracket-face
    unowned var foo = Foo() // EXPECTED: "unowned" swift-mode:keyword-face "var" swift-mode:keyword-face "=" swift-mode:operator-face "Foo" swift-mode:type-face "()" swift-mode:bracket-face

    required init(x: Int) { // EXPECTED: "required" swift-mode:keyword-face "init" swift-mode:keyword-face "(" swift-mode:bracket-face ":" swift-mode:delimiter-face "Int" swift-mode:builtin-type-face ")" swift-mode:bracket-face "{" swift-mode:bracket-face
        super.init(x) // EXPECTED: "super" swift-mode:keyword-face "." swift-mode:operator-face "init" swift-mode:keyword-face "(" swift-mode:bracket-face ")" swift-mode:bracket-face
    }

    convenience init?() { // EXPECTED: "convenience" swift-mode:keyword-face "init" swift-mode:keyword-face "?" swift-mode:operator-face "()" swift-mode:bracket-face "{" swift-mode:bracket-face
        self.init(100) // EXPECTED: "self" swift-mode:keyword-face "." swift-mode:operator-face "init" swift-mode:keyword-face "(" swift-mode:bracket-face "100" swift-mode:number-face ")" swift-mode:bracket-face
    }

    deinit { // EXPECTED: "deinit" swift-mode:keyword-face "{" swift-mode:bracket-face
    }

    let x: Int = 1 // EXPECTED: "let" swift-mode:keyword-face ":" swift-mode:delimiter-face "Int" swift-mode:builtin-type-face "=" swift-mode:operator-face "1" swift-mode:number-face
    var x: Int { // EXPECTED: "var" swift-mode:keyword-face ":" swift-mode:delimiter-face "Int" swift-mode:builtin-type-face "{" swift-mode:bracket-face
        get { // EXPECTED: "get" swift-mode:keyword-face "{" swift-mode:bracket-face
            x
        }

        set(x: Int) { // EXPECTED: "set" swift-mode:keyword-face "(" swift-mode:bracket-face ":" swift-mode:delimiter-face "Int" swift-mode:builtin-type-face ")" swift-mode:bracket-face "{" swift-mode:bracket-face
            x = x
        }
    }
    var x: Int = 1 { // EXPECTED: "var" swift-mode:keyword-face ":" swift-mode:delimiter-face "Int" swift-mode:builtin-type-face "=" swift-mode:operator-face "1" swift-mode:number-face "{" swift-mode:bracket-face
        willSet { // EXPECTED: "willSet" swift-mode:keyword-face "{" swift-mode:bracket-face
        }

        didSet { // EXPECTED: "didSet" swift-mode:keyword-face "{" swift-mode:bracket-face
        }
    }

    subscript(x: Int) { // EXPECTED: "subscript" swift-mode:keyword-face "(" swift-mode:bracket-face ":" swift-mode:delimiter-face "Int" swift-mode:builtin-type-face ")" swift-mode:bracket-face "{" swift-mode:bracket-face
        get { // EXPECTED: "get" swift-mode:keyword-face "{" swift-mode:bracket-face
            x
        }

        set(x: Int) { // EXPECTED: "set" swift-mode:keyword-face "(" swift-mode:bracket-face ":" swift-mode:delimiter-face "Int" swift-mode:builtin-type-face ")" swift-mode:bracket-face "{" swift-mode:bracket-face
            x = x
        }
    }

    func foo(a: borrowing Int, b: consuming Int, c: inout Int) { // EXPECTED: "func" swift-mode:keyword-face "foo" swift-mode:function-name-face "(" swift-mode:bracket-face ":" swift-mode:delimiter-face "borrowing" swift-mode:keyword-face "Int" swift-mode:builtin-type-face "," swift-mode:delimiter-face ":" swift-mode:delimiter-face "consuming" swift-mode:keyword-face "Int" swift-mode:builtin-type-face "," swift-mode:delimiter-face ":" swift-mode:delimiter-face "inout" swift-mode:keyword-face "Int" swift-mode:builtin-type-face ")" swift-mode:bracket-face "{" swift-mode:bracket-face
        do { // EXPECTED: "do" swift-mode:keyword-face "{" swift-mode:bracket-face
            defer { // EXPECTED: "defer" swift-mode:keyword-face "{" swift-mode:bracket-face
                foo()
            }

            guard foo(copy a) // EXPECTED: "guard" swift-mode:keyword-face "foo" swift-mode:function-call-face "(" swift-mode:bracket-face "copy" swift-mode:keyword-face ")" swift-mode:bracket-face
            else { // EXPECTED: "else" swift-mode:keyword-face "{" swift-mode:bracket-face
                foo()
            }

            try foo() // EXPECTED: "try" swift-mode:keyword-face "foo" swift-mode:function-call-face "()" swift-mode:bracket-face

            while true { // EXPECTED: "while" swift-mode:keyword-face "true" swift-mode:constant-keyword-face "{" swift-mode:bracket-face
                if true { // EXPECTED: "if" swift-mode:keyword-face "true" swift-mode:constant-keyword-face "{" swift-mode:bracket-face
                    break // EXPECTED: "break" swift-mode:keyword-face
                } else { // EXPECTED: "}" swift-mode:bracket-face "else" swift-mode:keyword-face "{" swift-mode:bracket-face
                    continue // EXPECTED: "continue" swift-mode:keyword-face
                }
            }

            for x in xs { // EXPECTED: "for" swift-mode:keyword-face "in" swift-mode:keyword-face "xs" swift-mode:function-call-face "{" swift-mode:bracket-face
            }

            repeat { // EXPECTED: "repeat" swift-mode:keyword-face "{" swift-mode:bracket-face
            } while false // EXPECTED: "}" swift-mode:bracket-face "while" swift-mode:keyword-face "false" swift-mode:constant-keyword-face

            while false { // EXPECTED: "while" swift-mode:keyword-face "false" swift-mode:constant-keyword-face "{" swift-mode:bracket-face
                throw Foo() // EXPECTED: "throw" swift-mode:keyword-face "Foo" swift-mode:type-face "()" swift-mode:bracket-face
            }

            switch e { // EXPECTED: "switch" swift-mode:keyword-face "e" swift-mode:function-call-face "{" swift-mode:bracket-face
            case Foo: // EXPECTED: "case" swift-mode:keyword-face "Foo" swift-mode:type-face ":" swift-mode:delimiter-face
                fallthrough // EXPECTED: "fallthrough" swift-mode:keyword-face
            case Foo:
                foo()
            default: // EXPECTED: "default" swift-mode:keyword-face ":" swift-mode:delimiter-face
                foo()
            }

            return // EXPECTED: "return" swift-mode:keyword-face
        } catch e { // EXPECTED: "}" swift-mode:bracket-face "catch" swift-mode:keyword-face "e" swift-mode:function-call-face "{" swift-mode:bracket-face
        }
    }

    fileprivate func foo(f: () throws -> Int) -> async rethrows some Foo { // EXPECTED: "fileprivate" swift-mode:keyword-face "func" swift-mode:keyword-face "foo" swift-mode:function-name-face "(" swift-mode:bracket-face ":" swift-mode:delimiter-face "()" swift-mode:bracket-face "throws" swift-mode:keyword-face "->" swift-mode:operator-face "Int" swift-mode:builtin-type-face ")" swift-mode:bracket-face "->" swift-mode:operator-face "async" swift-mode:keyword-face "rethrows" swift-mode:keyword-face "some" swift-mode:keyword-face "Foo" swift-mode:type-face "{" swift-mode:bracket-face
        await foo() as Foo // EXPECTED: "await" swift-mode:keyword-face "foo" swift-mode:function-call-face "()" swift-mode:bracket-face "as" swift-mode:keyword-face "Foo" swift-mode:type-face
        foo() is Foo // EXPECTED: "foo" swift-mode:function-call-face "()" swift-mode:bracket-face "is" swift-mode:keyword-face "Foo" swift-mode:type-face
        Foo.Protocol // EXPECTED: "Foo" swift-mode:type-face "." swift-mode:operator-face "Protocol" swift-mode:keyword-face
        Foo.Type // EXPECTED: "Foo" swift-mode:type-face "." swift-mode:operator-face "Type" swift-mode:keyword-face
        Self.foo() // EXPECTED: "Self" swift-mode:keyword-face "." swift-mode:operator-face "foo" swift-mode:function-call-face "()" swift-mode:bracket-face
        Foo()
    }

    internal func foo() { // EXPECTED: "internal" swift-mode:keyword-face "func" swift-mode:keyword-face "foo" swift-mode:function-name-face "()" swift-mode:bracket-face "{" swift-mode:bracket-face
    }

    open func foo() { // EXPECTED: "open" swift-mode:keyword-face "func" swift-mode:keyword-face "foo" swift-mode:function-name-face "()" swift-mode:bracket-face "{" swift-mode:bracket-face
    }

    private func foo() { // EXPECTED: "private" swift-mode:keyword-face "func" swift-mode:keyword-face "foo" swift-mode:function-name-face "()" swift-mode:bracket-face "{" swift-mode:bracket-face
    }

    @objc
    public dynamic override func foo() { // EXPECTED: "public" swift-mode:keyword-face "dynamic" swift-mode:keyword-face "override" swift-mode:keyword-face "func" swift-mode:keyword-face "foo" swift-mode:function-name-face "()" swift-mode:bracket-face "{" swift-mode:bracket-face
    }

    func foo<each T: Foo>(foo: (repeat each Foo)) { // EXPECTED: "func" swift-mode:keyword-face "foo" swift-mode:function-name-face "<" swift-mode:operator-face "each" swift-mode:keyword-face "T" swift-mode:type-face ":" swift-mode:delimiter-face "Foo" swift-mode:type-face ">" swift-mode:operator-face "(" swift-mode:bracket-face ":" swift-mode:delimiter-face "(" swift-mode:bracket-face "repeat" swift-mode:keyword-face "each" swift-mode:keyword-face "Foo" swift-mode:type-face "))" swift-mode:bracket-face "{" swift-mode:bracket-face
    }

    // reserved but not used keyword
    precedence // EXPECTED: "precedence" swift-mode:keyword-face
}

enum Foo { // EXPECTED: "enum" swift-mode:keyword-face "Foo" swift-mode:type-face "{" swift-mode:bracket-face
    case foo // EXPECTED: "case" swift-mode:keyword-face
    indirect case foo(foo: Foo) // EXPECTED: "indirect" swift-mode:keyword-face "case" swift-mode:keyword-face "foo" swift-mode:function-call-face "(" swift-mode:bracket-face ":" swift-mode:delimiter-face "Foo" swift-mode:type-face ")" swift-mode:bracket-face
}

infix operator +++: AdditionPrecedence // EXPECTED: "infix" swift-mode:keyword-face "operator" swift-mode:keyword-face "+++" swift-mode:operator-face ":" swift-mode:delimiter-face "AdditionPrecedence" swift-mode:builtin-precedence-group-face
postfix operator +++: AdditionPrecedence // EXPECTED: "postfix" swift-mode:keyword-face "operator" swift-mode:keyword-face "+++" swift-mode:operator-face ":" swift-mode:delimiter-face "AdditionPrecedence" swift-mode:builtin-precedence-group-face
prefix operator +++: AdditionPrecedence // EXPECTED: "prefix" swift-mode:keyword-face "operator" swift-mode:keyword-face "+++" swift-mode:operator-face ":" swift-mode:delimiter-face "AdditionPrecedence" swift-mode:builtin-precedence-group-face

precedencegroup Foo { // EXPECTED: "precedencegroup" swift-mode:keyword-face "Foo" swift-mode:type-face "{" swift-mode:bracket-face
    higherThan: Foo // EXPECTED: "higherThan" swift-mode:keyword-face ":" swift-mode:delimiter-face "Foo" swift-mode:type-face
    lowerThan: Foo // EXPECTED: "lowerThan" swift-mode:keyword-face ":" swift-mode:delimiter-face "Foo" swift-mode:type-face
    associativity: left // EXPECTED: "associativity" swift-mode:keyword-face ":" swift-mode:delimiter-face "left" swift-mode:keyword-face
    assignment: true // EXPECTED: "assignment" swift-mode:keyword-face ":" swift-mode:delimiter-face "true" swift-mode:constant-keyword-face
}

precedencegroup Foo { // EXPECTED: "precedencegroup" swift-mode:keyword-face "Foo" swift-mode:type-face "{" swift-mode:bracket-face
    higherThan: Foo // EXPECTED: "higherThan" swift-mode:keyword-face ":" swift-mode:delimiter-face "Foo" swift-mode:type-face
    lowerThan: Foo // EXPECTED: "lowerThan" swift-mode:keyword-face ":" swift-mode:delimiter-face "Foo" swift-mode:type-face
    associativity: right // EXPECTED: "associativity" swift-mode:keyword-face ":" swift-mode:delimiter-face "right" swift-mode:keyword-face
    assignment: true // EXPECTED: "assignment" swift-mode:keyword-face ":" swift-mode:delimiter-face "true" swift-mode:constant-keyword-face
}

precedencegroup Foo { // EXPECTED: "precedencegroup" swift-mode:keyword-face "Foo" swift-mode:type-face "{" swift-mode:bracket-face
    higherThan: Foo // EXPECTED: "higherThan" swift-mode:keyword-face ":" swift-mode:delimiter-face "Foo" swift-mode:type-face
    lowerThan: Foo // EXPECTED: "lowerThan" swift-mode:keyword-face ":" swift-mode:delimiter-face "Foo" swift-mode:type-face
    associativity: none // EXPECTED: "associativity" swift-mode:keyword-face ":" swift-mode:delimiter-face "none" swift-mode:keyword-face
    assignment: true // EXPECTED: "assignment" swift-mode:keyword-face ":" swift-mode:delimiter-face "true" swift-mode:constant-keyword-face
}

struct Foo { // EXPECTED: "struct" swift-mode:keyword-face "Foo" swift-mode:type-face "{" swift-mode:bracket-face
    mutating func foo(a: sending Foo) { // EXPECTED: "mutating" swift-mode:keyword-face "func" swift-mode:keyword-face "foo" swift-mode:function-name-face "(" swift-mode:bracket-face ":" swift-mode:delimiter-face "sending" swift-mode:keyword-face "Foo" swift-mode:type-face ")" swift-mode:bracket-face "{" swift-mode:bracket-face
    }

    nonmutating func foo() { // EXPECTED: "nonmutating" swift-mode:keyword-face "func" swift-mode:keyword-face "foo" swift-mode:function-name-face "()" swift-mode:bracket-face "{" swift-mode:bracket-face
    }

    consuming func foo() { // EXPECTED: "consuming" swift-mode:keyword-face "func" swift-mode:keyword-face "foo" swift-mode:function-name-face "()" swift-mode:bracket-face "{" swift-mode:bracket-face
        discard self // EXPECTED: "discard" swift-mode:keyword-face "self" swift-mode:keyword-face
    }
}

extension Foo { // EXPECTED: "extension" swift-mode:keyword-face "Foo" swift-mode:type-face "{" swift-mode:bracket-face
}

distributed actor Foo { // EXPECTED: "distributed" swift-mode:keyword-face "actor" swift-mode:keyword-face "Foo" swift-mode:type-face "{" swift-mode:bracket-face
    nonisolated func foo() { // EXPECTED: "nonisolated" swift-mode:keyword-face "func" swift-mode:keyword-face "foo" swift-mode:function-name-face "()" swift-mode:bracket-face "{" swift-mode:bracket-face
    }
}

macro Foo() = #externalMacro(module: "Foo", type: "Foo") // EXPECTED: "macro" swift-mode:keyword-face "Foo" swift-mode:type-face "()" swift-mode:bracket-face "=" swift-mode:operator-face "#externalMacro" swift-mode:preprocessor-keyword-face "(" swift-mode:bracket-face ":" swift-mode:delimiter-face "\"Foo\"" swift-mode:string-face "," swift-mode:delimiter-face ":" swift-mode:delimiter-face "\"Foo\"" swift-mode:string-face ")" swift-mode:bracket-face
