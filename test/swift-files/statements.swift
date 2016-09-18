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
    .foo() { // swift-mode:test:known-bug
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
    .foo() +++ { z in // swift-mode:test:known-bug
        bar()
    } {
    foo()
    foo()
}

for
  case
    ( // swift-mode:test:known-bug
      x,
      y
    )
  in
  xs
    .foo() +++ { z in // swift-mode:test:known-bug
        bar()
        bar()
    } {
    foo()
    foo()
}

for case
      ( // swift-mode:test:known-bug
        x,
        y
      )
    in
    xs
      .foo() +++ { z in // swift-mode:test:known-bug
          bar()
          bar()
      } {
    foo()
    foo()
}


for Foo
      .Bar(x) // swift-mode:test:known-bug
    in
    xs {
    foo()
    foo()
}

for
  Foo
    .Bar(x) // swift-mode:test:known-bug
  in
  xs {
    foo()
    foo()
}



for x as
      Foo // swift-mode:test:known-bug
    in
    xs {
    foo()
    foo()
}


for x
    in
    xs
      .foo // swift-mode:test:known-bug
    where // swift-mode:test:known-bug
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
        .bbb(x) { // swift-mode:test:known-bug
    foo()
    foo()
}

for x
    in
    xs where aaa
      .bbb(x) { // swift-mode:test:known-bug
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

// Repeat-while statements

repeat {
    foo()
    foo()
} while foo
  .bar() // swift-mode:test:known-bug
  .baz()

repeat {
    foo()
    foo()
} while
  foo
  .bar() // swift-mode:test:known-bug
  .baz()

repeat {
    foo()
    foo()
}
  while
  foo
  .bar() // swift-mode:test:known-bug
  .baz()

repeat {
    foo()
    foo()
}
  while foo
  .bar() // swift-mode:test:known-bug
  .baz()

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
    foo() // swift-mode:test:known-bug
    foo()
} // swift-mode:test:known-bug

// Switch statement

switch foo
  .bar {
case foo:
    foo()
    foo()
default:
    foo()
    foo()
} // swift-mode:test:known-bug

switch
  foo
  .bar {
case foo:
    foo()
    foo()
default:
    foo()
    foo()
} // swift-mode:test:known-bug


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
       .P(x) // swift-mode:test:known-bug
         where // swift-mode:test:known-bug
           foo
             .bar(),
     let
       .Q(x)
         where // swift-mode:test:known-bug
           foo
             .bar(),
     let
       .R(x)
         where // swift-mode:test:known-bug
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
    .P(x) // swift-mode:test:known-bug
      where // swift-mode:test:known-bug
        foo
          .bar(),
  let
    .Q(x)
      where // swift-mode:test:known-bug
        foo
          .bar(),
  let
    .R(x)
      where // swift-mode:test:known-bug
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
    .P(x) // swift-mode:test:known-bug
      where // swift-mode:test:known-bug
        foo
          .bar(),
  let Foo
    .Q(x)
      where // swift-mode:test:known-bug
        foo
          .bar(),
  let Foo
    .R(x)
      where // swift-mode:test:known-bug
        foo
          .bar():
    foo()
    foo()
case
  Foo
    .P, // swift-mode:test:known-bug
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
    Foo // swift-mode:test:known-bug
    .P(x)
      where // swift-mode:test:known-bug
        foo
          .bar(),
  let
    Foo
    .Q(x)
      where // swift-mode:test:known-bug
        foo
          .bar(),
  let
    Foo
    .R(x)
      where // swift-mode:test:known-bug
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
    Foo // swift-mode:test:known-bug
      where // swift-mode:test:known-bug
        foo
          .bar(),
  is
    Foo
      where // swift-mode:test:known-bug
        foo
          .bar(),
  let Foo
    .Bar
    .Baz
      where // swift-mode:test:known-bug
        foo
          .bar():
    foo()
    foo()
default:
    foo()
    foo()
}

// swift-mode:test:eval (setq-local swift-mode:switch-case-offset 2)

switch foo {
  case foo:
    foo() // swift-mode:test:known-bug
    foo()
  default:
    foo() // swift-mode:test:known-bug
    foo()
}

// swift-mode:test:eval (setq-local swift-mode:switch-case-offset 0)



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
    where // swift-mode:test:known-bug
      foo()
        .bar() {
    foo()
    foo()
} catch
  Foo // swift-mode:test:known-bug
  .Bar(x)
    where
      foo()
        .bar() {
    foo()
    foo()
} catch
    where // swift-mode:test:known-bug
      foo()
        .bar() {
    foo()
    foo()
}

// Conditional control statements

func foo() {
    #if foo
    foo()
    foo()
    #elsif foo
    foo()
    foo()
    #else
    foo()
    foo()
    #end
}
