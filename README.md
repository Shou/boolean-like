
This package first-and-foremost provides a set of logical combinators,
under the typeclasses `Andlike`, `Orlike`, and `Xorlike`,
that define operations dealing with boolean-representable structures such
as `Maybe` which has true-like `Just` and false-like `Nothing`, or `[a]` by
true-like non-empty list and false-like empty list. It also introduces
the optional type constraint `Falsifier` which is for types containing a
unary false-like constructor, such as `Nothing`.

```haskell
import Combinator.Booly
```

## Operator precedence

The operators specify argument precedence where necessary, indicating which
argument is to be returned when there is a conflict. Xorlike does not have this
issue because to succeed both cannot be true-like, and when both are false-like
only unary false-like constructors are allowed, hence the argument precedence
is irrelevant.

```haskell
-- Both true-like
Just 1 <|< Just 2 == Just 1 == Just 2 >|> Just 1
-- Both false-like
Left 1 <&< Left 2 == Left 1 == Left 2 >&> Left 1
```

## Default typeclass signatures

`Andlike` and `Orlike` provide default instances for `Applicative` and
`Alternative` respectively for convenience because there is a major overlap.
However, not all `Andlike` and `Orlike` are also `Applicative` and `Applicative`
respectively, and vice versa. `Map` from Data.Map is valid as `Andlike`,
`Orlike`, and `Xorlike`, but does not have an `Applicative` instance, nor
`Alternative`. There is an `Applicative` instance for `((->) a)`, but functions
don't have an empty constructor, which make them invalid instances of all three
boolean-like typeclasses.

## Isabelle proofs

Proofs are provided in `proofs/Booly.thy` over the Isabelle `Maybe` datatype
equivalent.

# Typeclasses and constraints

## Falsifier

```haskell
type Falsifier a = (Eq a, Monoid a)

false :: Falsifier a => a
false = mempty
```

The false-representing constructor value. Examples:

* `Maybe`: `Nothing`
* `[a]`: `[]`
* `Text`: `empty`


## Andlike

```haskell
class Andlike a where
    (<&<) :: a -> a -> a
```

Boolean-like logic operation `<&<` that acts like AND for any
boolean-representable datatypes, e.g. `[]` or `Maybe`.

__Associativity__

```haskell
(a <&< b) <&< c == a <&< (b <&< c)
```

__Structural commutativity__

`a <&< b` is structurally equivalent to `b <&< a`.

__Absorbing element / truth table__

```haskell
false <&< false == false
false <&< b == false
a <&< false == false
a <&< b == b
```

## Orlike

```haskell
class Orlike a where
    (<|<) :: a -> a -> a
```

Boolean-like logic operation `<|<` that acts like OR for any
boolean-representable datatypes, e.g. `[a]` or `Maybe`. It is basically
`Control.Applicative.(<|>)` with a list instance that doesn't append, and the
argument precedence indicated by the direction of the LT/GT symbols.

__Associativity__

```haskell
(a <|< b) <|< c == a <|< (b <|< c)
```

__Structural commutativity__

`a <|< b` is structurally equivalent to `b <|< a`.

__Identity element / Truth table__

```haskell
false <|< false == false
false <|< b == b
a <|< false == a
a <|< b == a
```

## Xorlike

```haskell
class Xorlike a where
    (<^>) :: a -> a -> a
```

Boolean-like logic operation `<^>` that acts like XOR for any
boolean-representable datatypes, e.g. `[]` or `Maybe`.

__Structural associativity__

`a <^> (b <^> c)` is structurally equivalent to `(a <^> b) <^> c`.

__Structural commutativity__

`a <^> b` is structurally equivalent to `b <^> a`.

__Truth table__

```haskell
false <^> false == false
false <^> b == b
a <^> false == a
a <^> b == false
```

# Helpful functions

# Examples

## Andlike

```haskell
dealWithThis json = do
    let maybeMessage = decode json :: Maybe Message

    case maybeMessage of
        Just message -> do
            let maybeUser = user message
                maybeText = text message
            -- maybeText relies on maybeUser being a Just value.
            maybe (return ()) handleMsg (maybeUser >&> maybeText)

        Nothing -> return ()
```

## Orlike

```haskell
-- Attoparsec
bbcode :: Parser _
bbcode = do
    tagName <- openingTag
    -- Prioritize parsing bbcode, otherwise try text.
    contents <- text >|> bbcode
    closingTag tagName
    return contents
```

## Xorlike

```haskell
runMaybeT $ do
    -- Only one should succeed!
    msg <- maybeSuccess <^> maybeError
    liftIO $ sendToClient msg
```

