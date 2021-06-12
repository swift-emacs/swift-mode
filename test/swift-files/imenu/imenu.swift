@AAA import FooModule1
@BBB(1, 2, 3) import FooModule2.SubModule1
import typealias FooModule3.ImportedTypeAlias
import struct FooModule3.ImportedStruct
import class FooModule3.ImportedClass
import enum FooModule3.ImportedEnum
import protocol FooModule3.ImportedProtocol
import let FooModule3.importedConstant
import var FooModule3.importedVariable
import func FooModule3.importedFunction

// Note that `import actor` is invalid (at least with Swift 5.5).

@AAA let globalConstant1 = 1,
         globalConstant2: Int = 2
@AAA var globalVariable1 = 1,
         globalVariable2: Int = 2

@AAA class FooClass<A, B, C>: AAA {
    @AAA private unowned(safe) lazy class final var classVariable1: AAA = AAA()
    @AAA public class final {
    }

    @AAA var compuatedProperty: Int {
        get {
            return 1
        }

        set {
            let a = 1
            var b = 2
        }
    }

    @AAA var observedProperty Int {
        willSet(a) {
            let a = 1
            var b = 2
        }

        didSet(a) {
        }
    }

    @AAA internal typealias TypeAlias<A: AA, B: BB, C: CC> = AAA

    @AAA class final func function1<A: AA, B: BB, C: CC>(aaa bbb: Int, _ ccc: Int, ddd: Int = 1, eee: inout Int = 2) async throws -> AAA {
        let a = 1
        var b = 2

        return AAA()
    }
    @AAA class final func function2() rethrows -> AAA {
        return AAA()
    }
    @AAA class final func function3(a: Int...) rethrows -> AAA {
        return AAA()
    }

    // Argument labels of operators are always "_"
    @AAA class final func + <A: AA, B: BB, C: CC>(lhs: FooClass1<A, B, C>, rhs: Int) rethrows -> AAA {
        return AAA()
    }

    // Default argument labels of subscript are "_".
    subscript<A: AA, B: BB, C: CC>(aaa: Int, bbb ccc: Int) -> AAA {
        get {
            return AAA()
        }
        set {
        }
    }

    init?(a: Int) {
    }

    init!(b: Int) {
    }

    deinit {
    }
}

@AAA enum FooEnum1<A, B, C>: FooProtocol {
    @AAA case case1(aaa: Int, bbb ccc: Int), case2
    @AAA indirect case case3(eee: FooEnum)
}

enum FooEnum2: Int {
    case case1 = 1,
         case2 = 2
}

struct FooStruct: AAA {
}

actor FooActor: AAA {
}

protocol FooProtocol: AAA {
    var protocolProperty: Int { get set}
    func protocolMethod(a: Int, b: Int) -> Int
    init(a: Int, b: Int, c d: Int)
    subscript<A: AA, B: BB, C: CC>(aaa: Int, bbb ccc: Int) -> AAA
    associatedtype AssociatedType: AAA
}

extension FooClass: BBB where A = Int {
    class NestedClass1 {
        class NestedClass2 {
            class NestedClass3 {
            }
        }
    }
}

infix operator +++++: precedenceGroup
prefix operator -----: precedenceGroup
postfix operator *****: precedenceGroup

precedencegroup precedenceGroup {
    higherThan: a
    lowerThan: a
    associativity: left
    assignment: true
}
