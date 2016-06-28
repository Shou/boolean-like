
# Introduction
A set of typeclasses `Falsifier`, `Andlike`, `Orlike`, and `Xorlike`,
that define operations dealing with boolean-representable structures such
as `Maybe` which has true-like `Just` and false-like `Nothing`, or `[]` by
true-like non-empty list and false-like empty list.

# Typeclasses

## Falsifier
The false-representing constructor value.

## Andlike
 Boolean-like logic operation '>&>' that acts like AND for any
boolean-representable datatypes, e.g. '[]' or 'Maybe'.

__Associativity__

```haskell
(a >&> b) >&> c == a >&> (b >&> c)
```

__Absorbing element / truth table__

```haskell
false >&> false == false
false >&> b == false
a >&> false == false
a >&> b == b
```

## Orlike
Boolean-like logic operation `<|<` that acts like OR for any
boolean-representable datatypes, e.g. `[]` or `Maybe`. It is basically
`Control.Applicative.(<|>)` with a list instance that doesn't append.

__Associativity__

```haskell
(a <|< b) <|< c == a <|< (b <|< c)
```

__Absorbing element / truth table__

```haskell
false <|< false == false
false <|< b == b
a <|< false == a
a <|< b == a
```

## Xorlike
Boolean-like logic operation `<^>` that acts like XOR for any
boolean-representable datatypes, e.g. `[]` or `Maybe`.

__Absorbing element / truth table__

```haskell
false <^> false == false
false <^> b == b
a <^> false == a
a <^> b == false
```

