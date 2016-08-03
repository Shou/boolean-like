
{-# LANGUAGE ConstraintKinds, DefaultSignatures, GADTs #-}


-- | A set of typeclasses 'Falsifier', 'Andlike', 'Orlike', and 'Xorlike',
-- that define operations dealing with boolean-representable structures such
-- as 'Maybe' which has true-like 'Just' and false-like 'Nothing', or '[]' by
-- true-like non-empty list and false-like empty list.
module Combinator.Booly
    ( Andlike(..)
    , Orlike(..)
    , Xorlike(..)
    , Falsifier(..)
    , (>&>)
    , (>|>)
    , andLast
    , andHead
    , andMappend
    , andMconcat
    , isFalse
    , isTrue
    , boolF
    , voidF
    , whenF
    , unlessF
    )
    where


import Control.Applicative (Alternative(..))

-- FIXME both strict and lazy structures when necessary
import qualified Data.Attoparsec.Internal.Types as Atto
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Semigroup (Semigroup(..), Option(..))
import qualified Data.Text as T
import qualified Data.Vector as Vec


infixr 7 >&>
infixl 7 <&<
infixr 5 >|>
infixl 5 <|<
infixl 6 <^>


-- | Boolean-like logic operation '>&>' that acts like AND for any
-- boolean-representable datatypes, e.g. '[]' or 'Maybe'.
--
-- __Associativity__
--
-- prop> (a >&> b) >&> c == a >&> (b >&> c)
--
-- __Absorbing element / truth table__
--
-- prop> false >&> false == false
-- prop> false >&> b == false
-- prop> a >&> false == false
-- prop> a >&> b == b
--
class Andlike a where
    -- | Andlike operator, returns the rightmost argument on success, i.e.
    --   if no 'false' are present.
    (<&<) :: a -> a -> a

    -- | '<*' often shares behaviour with '<&<'.
    default (<&<) :: (Applicative f, f b ~ a) => a -> a -> a
    (<&<) = (<*)

-- | Boolean-like logic operation '<|<' that acts like OR for any
-- boolean-representable datatypes, e.g. '[]' or 'Maybe'. It is basically
-- 'Control.Applicative.(<|>)' with a list instance that doesn't append.
--
-- __Associativity__
--
-- prop> (a <|< b) <|< c == a <|< (b <|< c)
--
-- __Absorbing element / truth table__
--
-- prop> false <|< false == false
-- prop> false <|< b == b
-- prop> a <|< false == a
-- prop> a <|< b == a
--
class Orlike a where
    -- | Orlike operator, returns the leftmost true-like argument,
    -- otherwise the rightmost true-like argument, or finally 'false'.
    (<|<) :: a -> a -> a

    -- | All '<|>' instances except list-likes should share behaviour.
    default (<|<) :: (Alternative f, f b ~ a) => a -> a -> a
    (<|<) = (<|>)

-- | Boolean-like logic operation '<^>' that acts like XOR for any
-- boolean-representable datatypes, e.g. '[]' or 'Maybe'.
--
-- __Absorbing element / truth table__
--
-- prop> false <^> false == false
-- prop> false <^> b == b
-- prop> a <^> false == a
-- prop> a <^> b == false
--
class Xorlike a where
    -- | Xorlike operator, returns whichever argument is true-like as both
    -- cannot simultaneously be true-like values, or 'false'.
    (<^>) :: a -> a -> a

class Falsifier a where
    false :: a

    default false :: Monoid a => a
    false = mempty

-- {{{ Instances

instance Andlike () where
    _ <&< _ = ()

instance Orlike () where
    _ <|< _ = ()

instance Xorlike () where
    _ <^> _ = ()

instance Falsifier ()


instance Andlike (Maybe a) where
    Nothing <&< _ = Nothing
    _ <&< Nothing = Nothing
    a <&< _ = a

instance Orlike (Maybe a) where
    (Just a) <|< _ = Just a
    _ <|< (Just a) = Just a
    _ <|< _ = Nothing

instance Xorlike (Maybe a) where
    (Just a) <^> Nothing = Just a
    Nothing <^> (Just a) = Just a
    _ <^> _ = Nothing

instance Falsifier (Maybe a) where
    false = Nothing


instance Andlike (Option a) where
    (Option Nothing) <&< _ = Option Nothing
    _ <&< (Option Nothing) = Option Nothing
    a <&< _ = a

instance Orlike (Option a) where
    (Option (Just a)) <|< _ = Option (Just a)
    _ <|< (Option (Just a)) = Option (Just a)
    _ <|< _ = Option Nothing

instance Xorlike (Option a) where
    (Option (Just a)) <^> (Option Nothing) = Option (Just a)
    (Option Nothing) <^> (Option (Just a)) = Option (Just a)
    _ <^> _ = Option Nothing

instance Falsifier (Option a) where
    false = Option Nothing


instance Andlike (Either a b) where
    (Left a) <&< _ = Left a
    _ <&< (Left b) = Left b
    a <&< _ = a

instance Orlike (Either a b) where
    (Right a) <|< _ = Right a
    _ <|< (Right b) = Right b
    (Left a) <|< _ = Left a


instance Andlike ([] a) where
    [] <&< _ = []
    _ <&< [] = []
    a <&< _ = a

instance Orlike ([] a) where
    xs@(_:_) <|< _ = xs
    _ <|< xs@(_:_) = xs
    _ <|< _ = []

instance Xorlike ([] a) where
    xs@(_:_) <^> [] = xs
    [] <^> xs@(_:_) = xs
    _ <^> _ = []

instance Falsifier ([] a)


instance Andlike T.Text where
    ta <&< tb
        | T.null ta || T.null tb = T.empty
        | otherwise              = ta

instance Orlike T.Text where
    ta <|< tb
        | not (T.null ta) = ta
        | not (T.null tb) = tb
        | otherwise = T.empty

instance Xorlike T.Text where
    ta <^> tb
        | not (T.null ta) && T.null tb = ta
        | T.null ta && not (T.null tb) = tb
        | otherwise = T.empty

instance Falsifier T.Text


instance Andlike BS.ByteString where
    ba <&< bb
        | BS.null ba || BS.null bb = BS.empty
        | otherwise                = ba

instance Orlike BS.ByteString where
    ta <|< tb
        | not (BS.null ta) = ta
        | not (BS.null tb) = tb
        | otherwise = BS.empty

instance Xorlike BS.ByteString where
    ta <^> tb
        | not (BS.null ta) && BS.null tb = ta
        | BS.null ta && not (BS.null tb) = tb
        | otherwise = BS.empty

instance Falsifier BS.ByteString


instance Ord k => Andlike (Map.Map k v) where
    ma <&< mb
        | Map.null ma || Map.null mb = Map.empty
        | otherwise = ma

instance Ord k => Orlike (Map.Map k v) where
    ma <|< mb
        | not (Map.null ma) = ma
        | not (Map.null mb) = mb
        | otherwise       = Map.empty

instance Ord k => Xorlike (Map.Map k v) where
    ma <^> mb
        | not (Map.null ma) && Map.null mb = ma
        | Map.null ma && not (Map.null mb) = mb
        | otherwise = Map.empty


instance Andlike (Vec.Vector a)

instance Orlike (Vec.Vector a)

instance Xorlike (Vec.Vector a) where
    va <^> vb
        | not (Vec.null va) && Vec.null vb = va
        | Vec.null va && not (Vec.null vb) = vb
        | otherwise = Vec.empty


instance Andlike (Atto.Parser i a)

instance Orlike (Atto.Parser i a)


instance (Andlike a, Andlike b) => Andlike (a, b) where
    (a1, b1) <&< (a2, b2) = (a1 <&< a2, b1 <&< b2)

instance (Orlike a, Orlike b) => Orlike (a, b) where
    (a1, b1) <|< (a2, b2) = (a1 <|< a2, b1 <|< b2)

instance (Andlike a, Andlike b, Andlike c) => Andlike (a, b, c) where
    (a1, b1, c1) <&< (a2, b2, c2) = (a1 <&< a2, b1 <&< b2, c1 <&< c2)

instance (Orlike a, Orlike b, Orlike c) => Orlike (a, b, c) where
    (a1, b1, c1) <|< (a2, b2, c2) = (a1 <|< a2, b1 <|< b2, c1 <|< c2)

instance (Andlike a, Andlike b, Andlike c, Andlike d) => Andlike (a, b, c, d) where
    (a1, b1, c1, d1) <&< (a2, b2, c2, d2) = (a1 <&< a2, b1 <&< b2, c1 <&< c2, d1 <&< d2)

instance (Orlike a, Orlike b, Orlike c, Orlike d) => Orlike (a, b, c, d) where
    (a1, b1, c1, d1) <|< (a2, b2, c2, d2) = (a1 <|< a2, b1 <|< b2, c1 <|< c2, d1 <|< d2)


-- }}}


-- | Flipped version of '<&<'. Returns the leftmost argument on success.
(>&>) :: Andlike a => a -> a -> a
(>&>) = flip (<&<)

-- | Flipped version of '<|<'. Returns the leftmost argument on success.
(>|>) :: Orlike a => a -> a -> a
(>|>) = flip (<|<)

-- | Returns the first element on success of all values.
andHead :: (Andlike a, Falsifier a, Foldable t) => t a -> a
andHead as
    | null as   = false
    | otherwise = foldr1 (<&<) as

-- | Returns the last element on success of all values.
andLast :: (Andlike a, Falsifier a, Foldable t) => t a -> a
andLast as
    | null as   = false
    | otherwise = foldr1 (>&>) as

-- | Monadic append with the annihilating operator guarding each argument.
--   Returns the mappended result on success.
andMappend :: (Andlike a, Monoid a) => a -> a -> a
andMappend a b = (a <&< b) `mappend` (a >&> b)

-- | Monadic concatenation with the annihilating operator guarding each argument.
andMconcat :: (Andlike a, Falsifier a, Monoid a, Foldable t) => t a -> a
andMconcat as
    | null as   = false
    | otherwise = foldr1 andMappend as

isFalse :: (Eq a, Falsifier a) => a -> Bool
isFalse = (false ==)

isTrue :: (Eq a, Falsifier a) => a -> Bool
isTrue = not . isFalse

-- | Similar to 'Data.Bool.bool'
boolF :: (Eq a, Eq b, Falsifier a, Falsifier b) => a -> a -> b -> a
boolF a b f = if isTrue f then a else b

-- | Discard the argument and return 'false'.
voidF :: Falsifier a => a -> a
voidF = const false

whenF :: (Eq a, Eq b, Falsifier a, Falsifier b) => a -> b -> b
whenF fa fb = if isTrue fa then fb else false

unlessF :: (Falsifier a, Falsifier b) => a -> b -> b
unlessF fa fb = if isFalse fa then fb else false

