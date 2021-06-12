// swift-mode:test:eval (setq-local swift-mode:basic-offset 4)
// swift-mode:test:eval (setq-local swift-mode:parenthesized-expression-offset 2)
// swift-mode:test:eval (setq-local swift-mode:multiline-statement-offset 2)
// swift-mode:test:eval (setq-local swift-mode:switch-case-offset 0)

// For-in statements

for x in xs {
    foo()
    foo()
}

for x
    in xs {
    foo()
    foo()
}

for x in
    xs {
    foo()
    foo()
}

for x
    in
    xs {
    foo()
    foo()
}

for
  x
  in
  xs {
    foo()
    foo()
}

for
  x
  in xs
    .foo() {
    foo()
    foo()
}

for
  (
    x,
    y
  )
  in
  xs
    .foo() +++ { z in
        bar()
    } {
    foo()
    foo()
}

for
  case
    (
      x,
      y
    )
  in
  xs
    .foo() +++ { z in
        bar()
        bar()
    } {
    foo()
    foo()
}

for case
      (
        x,
        y
      )
    in
    xs
      .foo() +++ { z in
          bar()
          bar()
      } {
    foo()
    foo()
}


for Foo
      .Bar(x)
    in
    xs {
    foo()
    foo()
}

for
  Foo
    .Bar(x)
  in
  xs {
    foo()
    foo()
}

for case
      Foo
      .Bar(x)
    in
    xs {
    foo()
    foo()
}

for
  case
    Foo
    .Bar(x)
  in
  xs {
    foo()
    foo()
}



for x as
      Foo
    in
    xs {
    foo()
    foo()
}


for x
    in
    xs
      .foo
    where
      aaa
        .bbb(x) {
    foo()
    foo()
}

for x
    in
    xs where
      aaa
        .bbb(x) {
    foo()
    foo()
}

for x
    in
    xs
    where aaa
            .bbb(x) {
    foo()
    foo()
}

for x
    in
    xs where aaa
               .bbb(x) {
    foo()
    foo()
}

for
  x in xs
  where
    aaa.bbb(x) {
    foo()
    foo()
}

for case .P(x)
    in
    xs
    where
      aaa.bbb(x) {
    foo()
    foo()
}

for case .P(x) in xs where
      aaa
        .bbb(x) {
    foo()
    foo()
}

for .P(x) in xs where
      aaa
        .bbb(x) {
    foo()
    foo()
}

for case .P(x) in xs
    where
      aaa
        .bbb(x) {
    foo()
    foo()
}

for .P(x) in xs
    where
      aaa
        .bbb(x) {
    foo()
    foo()
}

for case .P(x)
    in xs
    where
      aaa
        .bbb(x) {
    foo()
    foo()
}

for .P(x)
    in xs
    where
      aaa
        .bbb(x) {
    foo()
    foo()
}

for
  case .P(x)
  in
  xs
  where
    aaa.bbb(x) {
    foo()
    foo()
}

for
  case .P(x) in xs where
    aaa
      .bbb(x) {
    foo()
    foo()
}

for .P(x) in xs where
      aaa
        .bbb(x) {
    foo()
    foo()
}

for
  case .P(x) in xs
  where
    aaa
      .bbb(x) {
    foo()
    foo()
}

for
  .P(x) in xs
  where
    aaa
      .bbb(x) {
    foo()
    foo()
}

for
  case .P(x)
  in xs
  where
    aaa
      .bbb(x) {
    foo()
    foo()
}

for
  .P(x)
  in xs
  where
    aaa
      .bbb(x) {
    foo()
    foo()
}


for
  x in foo(
    a: {
        b()
    }
  )
{
}

for
  x in [
    a {
        b()
    }
  ]
{
}

for
  try
    await
    x
  in
  xs() {
    foo()
    foo()
}


// While statements

while foo
        .bar() +++ { x in
            foo()
            foo()
        } {
    foo()
    foo()
}

while
  foo
    .bar() +++ { x in
        foo()
        foo()
    } {
    foo()
    foo()
}

while
  let
    x
    =
    xx,
  var
    y
    =
    yy,
  x
    ==
    y,
  case
    (
      a,
      b
    )
    =
    ab {
    foo()
    foo()
}

while let
        x
        =
        xx,
      var
        y
        =
        yy,
      x
        ==
        y,
      case
        (
          a,
          b
        )
        =
        ab {
    foo()
    foo()
}


while let
        x
        =
        xx
    , var
        y
        =
        yy
    , x
        ==
        y
    , case
        (
          a,
          b
        )
        =
        ab
{
    foo()
    foo()
}


while
  let foo = bar(
    a: {
        b()
    }
  )
{
}

while
  foo(
    a: {
        b()
    }
  ),
  a == [
    b {
        c()
    }
  ]
{
}

// Repeat-while statements

repeat {
    foo()
    foo()
} while foo
          .bar()
          .baz()

repeat {
    foo()
    foo()
} while
  foo
    .bar()
    .baz()

repeat {
    foo()
    foo()
}
  while
  foo
    .bar()
    .baz()

repeat {
    foo()
    foo()
}
  while foo
          .bar()
          .baz()

repeat {
} while foo(
          a: {
              b()
          }
        )

repeat {
} while a == [
          b {
              c()
          }
        ]

// If statement

if x
     .foo()
     .bar() {
    foo()
    foo()
}

if
  x
    .foo()
    .bar() {
    foo()
    foo()
}

if
  let
    x
    =
    xx,
  var
    y
    =
    yy,
  x
    ==
    y,
  case
    (
      a,
      b
    )
    =
    ab {
    foo()
    foo()
}

if foo() {
    foo()
    foo()
    foo()
} else if foo() {
    foo()
    foo()
    foo()
} else if foo
            .bar()
            .baz() +++ { x in
                return x
            },
          foo
            .bar()
            .baz() +++ { x in
                return x
            } {
    foo()
    foo()
    foo()
} else if
  foo
    .bar()
    .baz() +++ { x in
        return x
    },
  foo
    .bar()
    .baz() +++ { x in
        return x
    } {
    foo()
    foo()
    foo()
}

if foo.bar +++ {
       foo()
   },
   foo.bar +++ {
       foo()
   } {
    foo()
}

if
  foo(
    a: {
        b()
    }
  ),
  a == [
    b {
        c()
    }
  ]
{
}

if
  let foo = bar(
    a: {
        b()
    }
  ),
  let foo = bar(
    a: {
        b()
    }
  )
{
}

// Guard statement

guard
  foo
    .foo() else {
    bar()
    bar()
}

guard
  foo
    .foo()
else {
    bar()
    bar()
}

guard foo
        .foo()
        .foo() +++ { x in
            foo()
        } else {
    bar()
    bar()
}

guard
  foo
    .foo()
    .foo() +++ { x in
        foo()
    } else {
    bar()
    bar()
}

guard
  let
    x
    =
    xx,
  var
    y
    =
    yy,
  x
    ==
    y,
  case
    (
      a,
      b
    )
    =
    ab
else {
    foo()
    foo()
}

guard
  let
    x
    =
    xx,
  var
    y
    =
    yy,
  x
    ==
    y,
  case
    (
      a,
      b
    )
    =
    ab else {
    foo()
    foo()
}

guard
  let foo = bar(
    a: {
        b()
    }
  )
else {
}

guard
  foo(
    a: {
        b()
    }
  ),
  a == [
    b {
        c()
    }
  ]
else {
}

// Switch statement

switch foo
  .bar {
case foo:
    foo()
    foo()
default:
    foo()
    foo()
}

switch
  foo
  .bar {
case foo:
    foo()
    foo()
default:
    foo()
    foo()
}


switch foo {
case foo:
    foo()
      .bar()
    foo()
default:
    foo()
    foo()
}

switch foo {
case .P(let x)
       where
         foo
           .bar(),
     .Q(let x)
       where
         foo
           .bar(),
     .R(let x)
       where
         foo
           .bar():
    foo()
    foo()
default:
    foo()
    foo()
}

switch foo {
case let .P(x)
       where
         foo
           .bar(),
     let .Q(x)
       where
         foo
           .bar(),
     let .R(x)
       where
         foo
           .bar():
    foo()
    foo()
default:
    foo()
    foo()
}

switch foo {
case
  let .P(x)
    where
      foo
        .bar(),
  let .Q(x)
    where
      foo
        .bar(),
  let .R(x)
    where
      foo
        .bar():
    foo()
    foo()
default:
    foo()
    foo()
}

switch foo {
case
  let .P(x)
    where
      foo
        .bar(),
  let .Q(x)
    where
      foo
        .bar(),
  let .R(x)
    where
      foo
        .bar():
    foo()
    foo()
default:
    foo()
    foo()
}

switch foo {
case let
       .P(x)
         where
           foo
             .bar(),
     let
       .Q(x)
         where
           foo
             .bar(),
     let
       .R(x)
         where
           foo
             .bar():
    foo()
    foo()
default:
    foo()
    foo()
}

switch foo {
case
  let
    .P(x)
      where
        foo
          .bar(),
  let
    .Q(x)
      where
        foo
          .bar(),
  let
    .R(x)
      where
        foo
          .bar():
    foo()
    foo()
default:
    foo()
    foo()
}

switch foo {
case
  let Foo
    .P(x)
      where
        foo
          .bar(),
  let Foo
    .Q(x)
      where
        foo
          .bar(),
  let Foo
    .R(x)
      where
        foo
          .bar():
    foo()
    foo()
case
  Foo
    .P,
  Foo
    .Q,
  Foo
    .R:
default:
    foo()
    foo()
}

switch foo {
case
  let
    Foo
    .P(x)
      where
        foo
          .bar(),
  let
    Foo
    .Q(x)
      where
        foo
          .bar(),
  let
    Foo
    .R(x)
      where
        foo
          .bar():
    foo()
    foo()
default:
    foo()
    foo()
}

switch foo {
case
  is
    Foo
      where
        foo
          .bar(),
  is
    Foo
      where
        foo
          .bar(),
  let Foo
    .Bar
    .Baz
      where
        foo
          .bar():
    foo()
    foo()
default:
    foo()
    foo()
}

switch foo {
case let .P(x) where x is Foo<Int>:
    foo() // swift-mode:test:known-bug
    foo() // swift-mode:test:known-bug
default:
    foo()
    foo()
}


switch foo {
case let .P(x)
       where
         foo
           .bar:
case let AAA
       .P(x)
         where
           foo
             .bar:
case let .P(x) where
       foo
         .bar:
case let AAA
       .P(x) where
         foo
           .bar:
case let .P(x)
       where foo
               .bar:
case let AAA
       .P(x)
         where foo
                 .bar:
case let .P(x) where foo
                       .bar:
case let AAA
       .P(x) where foo
                     .bar:
}

switch foo {
case 1:
    if aaa {
    }
case 2:
    if bbb {
    }
default:
    if ccc {
    }
}

switch foo {
case 1:
    return
case 2:
    bar()
}

switch foo {
case 1:
    var x: Int = 1
    var y: Int = 2
}

// swift-mode:test:eval (setq-local swift-mode:switch-case-offset 2)

switch foo {
  case foo:
    foo()
    foo()
  default:
    foo()
    foo()
}

// swift-mode:test:eval (setq-local swift-mode:switch-case-offset 0)

// swift-mode:test:eval (setq-local swift-mode:switch-case-offset 4)

switch foo {
    case foo:
        foo()
        if let x = y {
            bar()
        }
        foo()
    default:
        foo()
        foo()
}

// swift-mode:test:eval (setq-local swift-mode:switch-case-offset 0)

switch
  foo(
    a: {
        b()
    }
  )
{
}

switch
  [
    a {
        b()
    }
  ]
{
}

// Labeled statements


foo:
  if foo
       .bar == baz {
}

foo:
  if
  foo
    .bar == baz {
}


foo:
  for
  x
  in
  xs {
    foo()
    foo()
}

// Control transfer statements

while foo() {
    break
    continue
    return
      foo()
    throw
      foo()

    switch foo() {
    case A:
        foo()
        fallthrough
    case B:
        foo()
        fallthrough
    default:
        foo()
    }
}

// Defer statements

defer {
    foo()
    bar()
    baz()
}

// Do statements

do {
} catch Foo
          .Bar(x)
            where
              foo()
                .bar() {
    foo()
    foo()
} catch Foo
          .Bar(x)
            where
              foo()
                .bar() {
    foo()
    foo()
} catch
  Foo
    .Bar(x)
      where
        foo()
          .bar() {
    foo()
    foo()
} catch
  where
    foo()
      .bar() {
    foo()
    foo()
}
catch Foo
        .Bar(x)
          where
            foo()
              .bar() {
    foo()
    foo()
}
catch
  Foo
    .Bar(x)
      where
        foo()
          .bar() {
    foo()
    foo()
}
catch
  where
    foo()
      .bar() {
    foo()
    foo()
}
catch
{
    foo()
    foo()
}

// Multi-Pattern Catch Clauses
// https://github.com/apple/swift-evolution/blob/master/proposals/0276-multi-pattern-catch-clauses.md

do {
} catch Foo(let a)
          where
            foo
              .bar(),
        Bar(let a)
          where
            foo
              .bar(),
        Baz(let a) where
          foo
            .bar() {
    foo(a)
} catch Foo(let a)
          where
            foo
              .bar()
      , Bar(let a)
          where
            foo
              .bar()
      , Baz(let a) where
          foo
            .bar() {
    foo(a)
} catch
  Foo(let a)
    where
      foo
        .bar(),
  Bar(let a)
    where
      foo
        .bar(),
  Baz(let a) where
    foo
      .bar() {
    foo(a)
}


// Conditional control statements

func foo() {
    #if foo
    foo()
    foo()
    #elseif foo
    foo()
    foo()
    #else
    foo()
    foo()
    #endif
}
