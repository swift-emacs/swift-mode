;;; swift-mode-font-lock.el --- Major-mode for Apple's Swift programming language, Font Locks. -*- lexical-binding: t -*-

;; Copyright (C) 2014-2017 taku0, Chris Barrett, Bozhidar Batsov,
;;                         Arthur Evstifeev, Michael Sanders

;; Authors: taku0 (http://github.com/taku0)
;;       Chris Barrett <chris.d.barrett@me.com>
;;       Bozhidar Batsov <bozhidar@batsov.com>
;;       Arthur Evstifeev <lod@pisem.net>
;;       Michael Sanders <michael.sanders@fastmail.com>
;;
;; Version: 5.0.0
;; Package-Requires: ((emacs "24.4") (seq "2.3"))
;; Keywords: languages swift

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Routines for Font Locks

;;; Code:

;;; Customizations

;;;###autoload
(defgroup swift-mode:faces nil
  "Font faces."
  :group 'swift)

(defface swift-mode:constant-keyword-face
  '((t . (:inherit font-lock-constant-face)))
  "Face for highlighting constant keywords

That is, true, false, and nil."
  :group 'swift-mode:faces)

(defface swift-mode:preprocessor-keyword-face
  '((t . (:inherit font-lock-preprocessor-face)))
  "Face for highlighting preprocessor keywords.

Exmpale: #if, #endif, and #selector."
  :group 'swift-mode:faces)

(defface swift-mode:keyword-face
  '((t . (:inherit font-lock-keyword-face)))
  "Face for highlighting keywords."
  :group 'swift-mode:faces)

(defface swift-mode:builtin-method-trailing-closure-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting builtin methods with trailing closure."
  :group 'swift-mode:faces)

(defface swift-mode:builtin-method-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting builtin methods."
  :group 'swift-mode:faces)

(defface swift-mode:builtin-function-trailing-closure-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting builtin functions with trailing closure."
  :group 'swift-mode:faces)

(defface swift-mode:builtin-function-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting builtin functions."
  :group 'swift-mode:faces)

(defface swift-mode:builtin-propertie-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting builtin properties."
  :group 'swift-mode:faces)

(defface swift-mode:builtin-enum-case-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting builtin enum cases."
  :group 'swift-mode:faces)

(defface swift-mode:build-config-keyword-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting build configuration keywords."
  :group 'swift-mode:faces)

(defface swift-mode:builtin-type-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting builtin types."
  :group 'swift-mode:faces)

(defface swift-mode:builtin-precedence-group-face
  '((t . (:inherit font-lock-builtin-face)))
  "Face for highlighting builtin precedence groups."
  :group 'swift-mode:faces)

(defface swift-mode:function-call-face
  '((t . (:inherit font-lock-function-name-face)))
  "Face for highlighting function calls."
  :group 'swift-mode:faces)

(defface swift-mode:function-name-face
  '((t . (:inherit font-lock-function-name-face)))
  "Face for highlighting function names."
  :group 'swift-mode:faces)

(defface swift-mode:property-access-face
  '((t . (:inherit font-lock-variable-name-face)))
  "Face for highlighting property accesses."
  :group 'swift-mode:faces)


;;; Supporting functions

(defun swift-mode:function-name-pos-p (pos limit)
  "Return t if POS is just before the name of a function declaration.

This function does not search beyond LIMIT."
  (goto-char pos)
  (forward-comment (- (point)))
  (skip-syntax-backward "w_")
  (looking-at
   "\\<\\(func\\|enum\\|struct\\|class\\|protocol\\|extension\\)\\>"))

(defun swift-mode:property-access-pos-p (pos limit)
  "Return t if POS is just before the property name of a member expression.

This function does not search beyond LIMIT."
  ;; foo.bar    // property access
  ;; foo .bar   // property access
  ;; foo . bar  // INVALID
  ;; foo. bar   // INVALID
  ;; foo?.bar   // property access
  ;; foo?. bar  // INVALID
  ;; foo ?.bar  // INVALID, but highlight as a property access anyway
  ;; foo? .bar  // property access
  ;; foo.bar()  // NOT property access
  ;; foo.bar () // NOT property access
  ;; foo.1      // property access
  ;; foo1.1     // property access
  ;; 1.1        // NOT property access
  ;; .1         // NOT property access
  ;; $1.1       // property access
  ;; .x         // property access
  (and
   ;; Just after dot
   (progn
     (goto-char pos)
     (eq (char-before) ?.))

   ;; Not floating-point literal
   (progn
     (goto-char pos)
     (backward-char)
     (skip-syntax-backward "w_")
     (not (looking-at "[0-9]*\\.[0-9]+\\>")))

   ;; Not method/function call
   (progn
     (goto-char pos)
     (skip-syntax-forward "w_" limit)
     (skip-chars-forward "?")
     ;; I don't sure we can use `forward-comment' beyond limit, so assuming
     ;; no comments here.
     (skip-syntax-forward " " limit)
     (not (eq (char-after) ?\()))))

(defun swift-mode:font-lock-match-expr (limit match-p)
  "Move the cursor just after an identifier that satisfy given predicate.

Set `match-data', and return t if the identifier found before position LIMIT.
Return nil otherwise.

The predicate MATCH-P is called with two arguments:
- the position of the identifier, and
- the limit of search functions."
  (and
   (< (point) limit)
   (re-search-forward "\\<\\(\\sw\\|\\s_\\)+\\>" limit t)
   (or
    (save-excursion
      (save-match-data
        (funcall match-p (match-beginning 0) limit)))
    (swift-mode:font-lock-match-expr limit match-p))))

(defun swift-mode:font-lock-match-function-names (limit)
  "Move the cursor just after a function name or others.

Others includes enum, struct, class, protocol, and extension name.
Set `match-data', and return t if a function name or others found before
position LIMIT.
Return nil otherwise."
  (swift-mode:font-lock-match-expr limit #'swift-mode:function-name-pos-p))

(defun swift-mode:font-lock-match-property-accesss (limit)
  "Move the cursor just after a property access.
Set `match-data', and return t if a property access found before position LIMIT.
Return nil otherwise."
  (swift-mode:font-lock-match-expr limit #'swift-mode:property-access-pos-p))

;;; Keywords and standard identifiers

;; Keywords
;; https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/LexicalStructure.html#//apple_ref/doc/uid/TP40014097-CH30-ID410

(defconst swift-mode:standard-member-functions-trailing-closure
  '("sort" "sorted" "split" "contains" "index" "partition" "filter" "first"
    "forEach" "flatMap" "withMutableCharacters" "withCString"
    "withUnsafeMutableBufferPointer" "withUnsafeMutablePointers"
    "withUnsafeMutablePointerToElements" "withUnsafeMutablePointerToHeader"
    "withUnsafeBufferPointer" "withUTF8Buffer" "min" "map" "max" "compactMap")
  "Member functions with closures in the standard library in Swift 3.

They may be used with trailing closures and no parentheses.")

(defconst swift-mode:standard-member-functions
  '("symmetricDifference" "storeBytes" "starts" "stride" "sortInPlace"
    "successor" "suffix" "subtract" "subtracting" "subtractInPlace"
    "subtractWithOverflow" "squareRoot" "samePosition" "holdsUniqueReference"
    "holdsUniqueOrPinnedReference" "hasSuffix" "hasPrefix" "negate" "negated"
    "next" "countByEnumerating" "copy" "copyBytes" "clamp" "clamped" "create"
    "toIntMax" "toOpaque" "toUIntMax" "takeRetainedValue" "takeUnretainedValue"
    "truncatingRemainder" "transcodedLength" "trailSurrogate"
    "isMutableAndUniquelyReferenced" "isMutableAndUniquelyReferencedOrPinned"
    "isStrictSuperset" "isStrictSupersetOf" "isStrictSubset" "isStrictSubsetOf"
    "isSuperset" "isSupersetOf" "isSubset" "isSubsetOf" "isContinuation"
    "isTotallyOrdered" "isTrailSurrogate" "isDisjoint" "isDisjointWith"
    "isUniqueReference" "isUniquelyReferenced" "isUniquelyReferencedOrPinned"
    "isEqual" "isLess" "isLessThanOrEqualTo" "isLeadSurrogate" "insert"
    "insertContentsOf" "intersect" "intersection" "intersectInPlace"
    "initialize" "initializeMemory" "initializeFrom" "indexOf" "indexForKey"
    "overlaps" "objectAt" "distance" "distanceTo" "divide" "divided"
    "divideWithOverflow" "descendant" "destroy" "decode" "decodeCString"
    "deinitialize" "dealloc" "deallocate" "deallocateCapacity" "dropFirst"
    "dropLast" "union" "unionInPlace" "underestimateCount" "unwrappedOrError"
    "update" "updateValue" "uppercased" "joined" "joinWithSeparator" "popFirst"
    "popLast" "passRetained" "passUnretained" "predecessor" "prefix" "escape"
    "escaped" "encode" "enumerate" "enumerated" "elementsEqual" "exclusiveOr"
    "exclusiveOrInPlace" "formRemainder" "formSymmetricDifference"
    "formSquareRoot" "formTruncatingRemainder" "formIntersection" "formIndex"
    "formUnion" "flatten" "fromCString" "fromCStringRepairingIllFormedUTF8"
    "fromOpaque" "withMemoryRebound" "width" "write" "writeTo" "lowercased"
    "load" "leadSurrogate" "lexicographicalCompare" "lexicographicallyPrecedes"
    "assign" "assignBackwardFrom" "assignFrom" "assumingMemoryBound" "add"
    "adding" "addingProduct" "addProduct" "addWithOverflow" "advanced"
    "advancedBy" "autorelease" "append" "appendContentsOf" "alloc" "allocate"
    "abs" "round" "rounded" "reserveCapacity" "retain" "reduce" "replace"
    "replaceRange" "replaceSubrange" "reverse" "reversed" "requestNativeBuffer"
    "requestUniqueMutableBackingBuffer" "release" "remove" "removeRange"
    "removeSubrange" "removeValue" "removeValueForKey" "removeFirst"
    "removeLast" "removeAtIndex" "removeAll" "remainder" "remainderWithOverflow"
    "generate" "getObjects" "getElement" "minimum" "minimumMagnitude"
    "minElement" "move" "moveInitialize" "moveInitializeMemory"
    "moveInitializeBackwardFrom" "moveInitializeFrom" "moveAssign"
    "moveAssignFrom" "multiply" "multiplyWithOverflow" "multiplied" "measure"
    "makeIterator" "makeDescription" "maximum" "maximumMagnitude" "maxElement"
    "bindMemory")
  "Member functions in the standard library in Swift 3.")

(defconst swift-mode:standard-global-functions-trailing-closure
  '("anyGenerator" "autoreleasepool")
  "Global functions with closures available in Swift 3.

They may be used with trailing closures and no parentheses.")

(defconst swift-mode:standard-global-functions
  '("stride" "strideof" "strideofValue" "sizeof" "sizeofValue" "sequence" "swap"
    "numericCast" "transcode" "isUniquelyReferenced"
    "isUniquelyReferencedNonObjC" "isKnownUniquelyReferenced" "zip" "dump"
    "debugPrint" "unsafeBitCast" "unsafeDowncast" "unsafeUnwrap" "unsafeAddress"
    "unsafeAddressOf" "print" "precondition" "preconditionFailure" "fatalError"
    "withUnsafeMutablePointer" "withUnsafePointer" "withExtendedLifetime"
    "withVaList" "assert" "assertionFailure" "alignof" "alignofValue" "abs"
    "repeatElement" "readLine" "getVaList" "min" "max")
  "Global functions available in Swift 3.")

(defconst swift-mode:standard-properties
  '("startIndex" "stringValue" "stride" "size" "sign" "signBitIndex"
    "significand" "significandBitCount" "significandBitPattern"
    "significandWidth" "signalingNaN" "superclassMirror" "summary"
    "subscriptBaseAddress" "header" "hashValue" "hasPointerRepresentation"
    "nulTerminatedUTF8" "nextDown" "nextUp" "nan" "nativeOwner" "characters"
    "count" "countTrailingZeros" "customMirror" "customPlaygroundQuickLook"
    "capacity" "isSignMinus" "isSignaling" "isSignalingNaN" "isSubnormal"
    "isNormal" "isNaN" "isCanonical" "isInfinite" "isZero" "isEmpty" "isFinite"
    "isASCII" "indices" "infinity" "identity" "owner" "description"
    "debugDescription" "unsafelyUnwrapped" "unicodeScalar" "unicodeScalars"
    "underestimatedCount" "utf16" "utf8" "utf8Start" "utf8CString"
    "utf8CodeUnitCount" "uintValue" "uppercaseString" "ulp" "ulpOfOne" "pi"
    "pointee" "endIndex" "elements" "exponent" "exponentBitCount"
    "exponentBitPattern" "value" "values" "keys" "quietNaN" "first"
    "firstElementAddress" "firstElementAddressIfContiguous" "floatingPointClass"
    "littleEndian" "lowercaseString" "leastNonzeroMagnitude"
    "leastNormalMagnitude" "last" "lazy" "alignment" "allocatedElementCount"
    "allZeros" "array" "arrayPropertyIsNativeTypeChecked" "radix" "rawValue"
    "greatestFiniteMagnitude" "min" "memory" "max" "byteSize" "byteSwapped"
    "binade" "bitPattern" "bigEndian" "buffer" "base" "baseAddress" "arguments"
    "argc" "unsafeArgv")
  "Properties in the standard library in Swift 3.")

(defconst swift-mode:standard-enum-cases
  '("scalarValue" "size" "signalingNaN" "sound" "some" "suppressed" "sprite"
    "set" "none" "negativeSubnormal" "negativeNormal" "negativeInfinity"
    "negativeZero" "color" "collection" "customized" "toNearestOrEven"
    "toNearestOrAwayFromZero" "towardZero" "tuple" "text" "int" "image"
    "optional" "dictionary" "double" "down" "uInt" "up" "url"
    "positiveSubnormal" "positiveNormal" "positiveInfinity" "positiveZero"
    "point" "plus" "error" "emptyInput" "view" "quietNaN" "float"
    "attributedString" "awayFromZero" "rectangle" "range" "generated" "minus"
    "bool" "bezierPath")
  "Enum cases in the standard library.

Note that there is some overlap between these and the properties.")

(defconst swift-mode:standard-enum-types
  '("ImplicitlyUnwrappedOptional" "Representation" "MemoryLayout"
    "FloatingPointClassification" "SetIndexRepresentation"
    "SetIteratorRepresentation" "FloatingPointRoundingRule"
    "UnicodeDecodingResult" "Optional" "DictionaryIndexRepresentation"
    "AncestorRepresentation" "DisplayStyle" "PlaygroundQuickLook" "Never"
    "FloatingPointSign" "Bit" "DictionaryIteratorRepresentation")
  "Enum types in the standard library in Swift 3.")

(defconst swift-mode:standard-protocols
  '("RandomAccessCollection" "RandomAccessIndexable"
    "RangeReplaceableCollection" "RangeReplaceableIndexable" "RawRepresentable"
    "MirrorPath" "MutableCollection" "MutableIndexable" "BinaryFloatingPoint"
    "BitwiseOperations" "BidirectionalCollection" "BidirectionalIndexable"
    "Strideable" "Streamable" "SignedNumber" "SignedInteger" "SetAlgebra"
    "Sequence" "Hashable" "Collection" "Comparable" "CustomReflectable"
    "CustomStringConvertible" "CustomDebugStringConvertible"
    "CustomPlaygroundQuickLookable" "CustomLeafReflectable" "CVarArg"
    "TextOutputStream" "Integer" "IntegerArithmetic" "Indexable" "IndexableBase"
    "IteratorProtocol" "OptionSet" "UnsignedInteger" "UnicodeCodec" "Equatable"
    "Error" "ExpressibleByBooleanLiteral" "ExpressibleByStringInterpolation"
    "ExpressibleByStringLiteral" "ExpressibleByNilLiteral"
    "ExpressibleByIntegerLiteral" "ExpressibleByDictionaryLiteral"
    "ExpressibleByUnicodeScalarLiteral"
    "ExpressibleByExtendedGraphemeClusterLiteral" "ExpressibleByFloatLiteral"
    "ExpressibleByArrayLiteral" "FloatingPoint" "LosslessStringConvertible"
    "LazySequenceProtocol" "LazyCollectionProtocol" "AnyObject"
    "AbsoluteValuable")
  "Protocols in the standard library in Swift 3.")

(defconst swift-mode:foundation-protocols
  '("CustomNSError" "LocalizedError" "NSKeyValueObservingCustomization"
    "RecoverableError" "ReferenceConvertible")
  "Protocols in Foundation.")

(defconst swift-mode:standard-structs
  '("Repeat" "Repeated" "ReversedRandomAccessCollection"
    "ReversedRandomAccessIndex" "ReversedCollection" "ReversedIndex"
    "RandomAccessSlice" "Range" "RangeReplaceableRandomAccessSlice"
    "RangeReplaceableBidirectionalSlice" "RangeReplaceableSlice"
    "RangeGenerator" "GeneratorSequence" "GeneratorOfOne" "Mirror"
    "MutableRandomAccessSlice" "MutableRangeReplaceableRandomAccessSlice"
    "MutableRangeReplaceableBidirectionalSlice" "MutableRangeReplaceableSlice"
    "MutableBidirectionalSlice" "MutableSlice" "ManagedBufferPointer"
    "BidirectionalSlice" "Bool" "StaticString" "String" "StrideThrough"
    "StrideThroughGenerator" "StrideThroughIterator" "StrideTo"
    "StrideToGenerator" "StrideToIterator" "Set" "SetIndex" "SetIterator"
    "Slice" "HalfOpenInterval" "Character" "CharacterView" "ContiguousArray"
    "CountableRange" "CountableClosedRange" "CollectionOfOne" "COpaquePointer"
    "ClosedRange" "ClosedRangeIndex" "ClosedRangeIterator" "ClosedInterval"
    "CVaListPointer" "Int" "Int16" "Int8" "Int32" "Int64" "Indices" "Index"
    "IndexingGenerator" "IndexingIterator" "Iterator" "IteratorSequence"
    "IteratorOverOne" "Zip2Sequence" "Zip2Iterator" "OpaquePointer"
    "ObjectIdentifier" "Dictionary" "DictionaryIndex" "DictionaryIterator"
    "DictionaryLiteral" "Double" "DefaultRandomAccessIndices"
    "DefaultBidirectionalIndices" "DefaultIndices" "UnsafeRawPointer"
    "UnsafeMutableRawPointer" "UnsafeMutableBufferPointer"
    "UnsafeMutablePointer" "UnsafeBufferPointer" "UnsafeBufferPointerGenerator"
    "UnsafeBufferPointerIterator" "UnsafePointer" "UnicodeScalar"
    "UnicodeScalarView" "UnfoldSequence" "Unmanaged" "UTF16" "UTF16View" "UTF8"
    "UTF8View" "UTF32" "UInt" "UInt16" "UInt8" "UInt32" "UInt64" "JoinGenerator"
    "JoinedSequence" "JoinedIterator" "PermutationGenerator"
    "EnumerateGenerator" "EnumerateSequence" "EnumeratedSequence"
    "EnumeratedIterator" "EmptyGenerator" "EmptyCollection" "EmptyIterator"
    "Float" "Float80" "FlattenGenerator" "FlattenBidirectionalCollection"
    "FlattenBidirectionalCollectionIndex" "FlattenSequence" "FlattenCollection"
    "FlattenCollectionIndex" "FlattenIterator" "LegacyChildren"
    "LazyRandomAccessCollection" "LazyMapRandomAccessCollection"
    "LazyMapGenerator" "LazyMapBidirectionalCollection" "LazyMapSequence"
    "LazyMapCollection" "LazyMapIterator" "LazyBidirectionalCollection"
    "LazySequence" "LazyCollection" "LazyFilterGenerator"
    "LazyFilterBidirectionalCollection" "LazyFilterSequence"
    "LazyFilterCollection" "LazyFilterIndex" "LazyFilterIterator"
    "AnyRandomAccessCollection" "AnyGenerator" "AnyBidirectionalCollection"
    "AnySequence" "AnyHashable" "AnyCollection" "AnyIndex" "AnyIterator"
    "AutoreleasingUnsafeMutablePointer" "Array" "ArraySlice")
  "Structs in the standard library in Swift 3.")

(defconst swift-mode:foundation-structs
  '("Calendar" "CharacterSet" "CocoaError" "Data" "Date" "DateComponents"
    "DateInterval" "ErrorUserInfoKey" "IndexPath" "IndexSet" "Locale"
    "MachError" "Measurement" "NSFastEnumerationIterator" "NSIndexSetIterator"
    "NSKeyValueObservedChange" "Notification" "POSIXError"
    "PersonNameComponents" "TimeZone" "URL" "URLComponents" "URLError"
    "URLQueryItem" "URLRequest" "URLResourceValues" "UUID")
  "Structs in Foundation.")

(defconst swift-mode:standard-typealiases
  '("RawSignificand" "RawExponent" "RawValue" "BooleanLiteralType" "Buffer"
    "Base" "Storage" "StringLiteralType" "Stride" "Stream1" "Stream2"
    "SubSequence" "NativeBuffer" "Child" "Children" "CBool" "CShort"
    "CSignedChar" "CodeUnit" "CChar" "CChar16" "CChar32" "CInt" "CDouble"
    "CUnsignedShort" "CUnsignedChar" "CUnsignedInt" "CUnsignedLong"
    "CUnsignedLongLong" "CFloat" "CWideChar" "CLong" "CLongLong" "IntMax"
    "IntegerLiteralType" "Indices" "Index" "IndexDistance" "Iterator" "Distance"
    "UnicodeScalarType" "UnicodeScalarIndex" "UnicodeScalarView"
    "UnicodeScalarLiteralType" "UnfoldFirstSequence" "UTF16Index" "UTF16View"
    "UTF8Index" "UIntMax" "Element" "Elements" "ExtendedGraphemeClusterType"
    "ExtendedGraphemeClusterLiteralType" "Exponent" "Void" "Value" "Key"
    "Float32" "FloatLiteralType" "Float64" "AnyClass" "Any")
  "Typealiases in the standard library in Swift 3.")

(defconst swift-mode:standard-class-types
  '("ManagedBuffer" "ManagedProtoBuffer" "NonObjectiveCBase" "AnyGenerator")
  "Built-in class types.")

(defconst swift-mode:standard-precedence-groups
  '("BitwiseShift" "Assignment" "RangeFormation" "Casting" "Addition"
    "NilCoalescing" "Comparison" "LogicalConjunction" "LogicalDisjunction"
    "Default" "Ternary" "Multiplication" "FunctionArrow")
  "Precedence groups in the standard library.")

(defconst swift-mode:constant-keywords
  '("true" "false" "nil"
    ;; CommandLine is an enum, but it acts like a constant.
    "CommandLine" "Process"
    ;; The return type of a function that never returns.
    "Never")
  "Keywords used as constants.")

(defconst swift-mode:preprocessor-keywords
  '("#available" "#colorLiteral" "#column" "#else" "#elseif" "#endif"
    "#fileLiteral" "#file" "#function" "#if" "#imageLiteral" "#keypath" "#line"
    "#selector" "#sourceLocation")
  "Keywords that begin with a number sign (#).")

(defconst swift-mode:declaration-keywords
  '("associatedtype" "class" "deinit" "enum" "extension" "fileprivate" "func"
    "import" "init" "inout" "internal" "let" "open" "operator" "private"
    "protocol" "public" "static" "struct" "subscript" "typealias" "var")
  "Keywords used in declarations.")

(defconst swift-mode:statement-keywords
  '("break" "case" "continue" "default" "defer" "do" "else" "fallthrough" "for"
    "guard" "if" "in" "repeat" "return" "switch" "where" "while")
  "Keywords used in statements.")

(defconst swift-mode:expression-keywords
  '("as" "catch" "dynamicType" "is" "rethrows" "super" "self" "Self" "throws"
    "throw" "try")
  "Keywords used in expressions and types.

Excludes true, false, and keywords begin with a number sign.")

(defconst swift-mode:context-keywords
  '("Protocol" "Type" "and" "assignment" "associativity" "convenience" "didSet"
    "dynamic" "final" "get" "higherThan" "indirect" "infix" "lazy" "left"
    "lowerThan" "mutating" "none" "nonmutating" "optional" "override" "postfix"
    "precedence" "precedencegroup" "prefix" "required" "right" "set" "unowned"
    "weak" "willSet")
  "Keywords reserved in particular contexts.")

(defconst swift-mode:build-config-keywords
  '("os" "arch" "swift" "OSX" "iOS" "watchOS" "tvOS" "i386" "x86_64" "arm"
    "arm64" "iOSApplicationExtension" "OSXApplicationExtension")
  "Keywords for build configuration statements.")

;;; font-lock definition

(defconst swift-mode:font-lock-keywords
  `(
    ;; Attributes
    "@\\(\\sw\\|\\s_\\)*"

    (,(regexp-opt swift-mode:constant-keywords 'words)
     .
     'swift-mode:constant-keyword-face)

    (,(regexp-opt swift-mode:preprocessor-keywords 'symbols)
     .
     'swift-mode:preprocessor-keyword-face)

    (,(regexp-opt (append swift-mode:declaration-keywords
                          swift-mode:statement-keywords
                          swift-mode:expression-keywords
                          swift-mode:context-keywords)
                  'words)
     .
     'swift-mode:keyword-face)

    (,(concat "\\."
              (regexp-opt swift-mode:standard-member-functions-trailing-closure
                          'words)
              "\\s-*[({]")
     1
     'swift-mode:builtin-method-trailing-closure-face)

    (,(concat "\\."
              (regexp-opt swift-mode:standard-member-functions 'words)
              "\\s-*(")
     1
     'swift-mode:builtin-method-face)

    (,(concat (regexp-opt swift-mode:standard-global-functions-trailing-closure
                          'words)
              "\\s-*[({]")
     1
     'swift-mode:builtin-function-trailing-closure-face)

    (,(concat (regexp-opt swift-mode:standard-global-functions 'words)
              "\\s-*(")
     1
     'swift-mode:builtin-function-face)

    (,(concat "\\." (regexp-opt swift-mode:standard-properties 'words))
     1
     'swift-mode:builtin-propertie-face)

    (,(concat "\\." (regexp-opt swift-mode:standard-enum-cases 'words))
     1
     'swift-mode:builtin-enum-case-face)

    (,(regexp-opt swift-mode:build-config-keywords 'words)
     .
     'swift-mode:build-config-keyword-face)

    (,(regexp-opt (append swift-mode:standard-class-types
                          swift-mode:standard-enum-types
                          swift-mode:foundation-protocols
                          swift-mode:foundation-structs
                          swift-mode:standard-protocols
                          swift-mode:standard-structs
                          swift-mode:standard-typealiases)
                  'words)
     .
     'swift-mode:builtin-type-face)

    (,(concat "\\<"
              (regexp-opt swift-mode:standard-precedence-groups 'non-nil)
              "Precedence\\>")
     .
     'swift-mode:builtin-precedence-group-face)

    ;; Method/function calls
    ("\\<\\(\\(\\sw\\|\\s_\\)+\\)\\>\\??\\s-*("
     1
     'swift-mode:function-call-face)

    ;; Function declarations
    (swift-mode:font-lock-match-function-names
     .
     'swift-mode:function-name-face)

    ;; Property accesses
    (swift-mode:font-lock-match-property-accesss
     .
     'swift-mode:property-access-face))
  "Swift mode keywords for Font Lock.")


(provide 'swift-mode-font-lock)

;;; swift-mode-font-lock.el ends here
