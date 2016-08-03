
# Introduction
This package first-and-foremost provides a set of logical combinators,
under the typeclasses Andlike`, `Orlike`, and `Xorlike`,
that define operations dealing with boolean-representable structures such
as `Maybe` which has true-like `Just` and false-like `Nothing`, or `[a]` by
true-like non-empty list and false-like empty list. It also introduces
the optional typeclass `Falsifier` which is defined to be the false-like value
provided it's composed of a unary constructor.

```haskell
import Control.BoolLike
```

# Typeclasses

## Falsifier
The false-representing constructor value. Examples:

* `Maybe`: `Nothing`
* `[a]`: `[]`
* `Text`: `empty`


## Andlike
Boolean-like logic operation `>&>` that acts like AND for any
boolean-representable datatypes, e.g. `[]` or `Maybe`.

__Associativity__

```haskell
(a >&> b) >&> c == a >&> (b >&> c)
```

__Structural commutativity__

`a >&> b` is structurally equivalent to `b >&> a`.

__Absorbing element / truth table__

```haskell
false >&> false == false
false >&> b == false
a >&> false == false
a >&> b == b
```

## Orlike
Boolean-like logic operation `<|<` that acts like OR for any
boolean-representable datatypes, e.g. `[a]` or `Maybe`. It is basically
`Control.Applicative.(<|>)` with a list instance that doesn't append.

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

## Batteries included



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

