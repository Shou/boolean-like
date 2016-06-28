

-- | A set of typeclasses 'Falsifier', 'Andlike', 'Orlike', and 'Xorlike',
-- that define operations dealing with boolean-representable structures such
-- as 'Maybe' which has true-like 'Just' and false-like 'Nothing', or '[]' by
-- true-like non-empty list and false-like empty list.
module Control.BoolLike
    ( Falsifier(..)
    , Andlike(..)
    , Orlike(..)
    , Xorlike(..)
    , (<&<)
    , (>|>)
    , andlast
    , andhead
    , andmappend
    , andmconcat
    , isFalsifier
    , fvoid
    )
    where


import qualified Data.ByteString as B
import Data.Semigroup (Option(..))
import qualified Data.Text as T


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
    (>&>) :: a -> a -> a

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

-- | The false-representing unary constructor value.
class Falsifier a where
    -- | Element representing a false value, a failure.
    false :: a

-- {{{ Instances

instance Falsifier () where
    false = ()
    {-# INLINE false #-}

instance Andlike () where
    _ >&> _ = ()
    {-# INLINE (>&>) #-}

instance Orlike () where
    _ <|< _ = ()

instance Xorlike () where
    _ <^> _ = ()


instance Falsifier (Maybe a) where
    false = Nothing
    {-# INLINE false #-}

instance Andlike (Maybe a) where
    Nothing >&> _ = Nothing
    _ >&> Nothing = Nothing
    _ >&> a = a
    {-# INLINE (>&>) #-}

instance Orlike (Maybe a) where
    (Just a) <|< _ = Just a
    _ <|< (Just a) = Just a
    _ <|< _ = Nothing

instance Xorlike (Maybe a) where
    (Just a) <^> Nothing = Just a
    Nothing <^> (Just a) = Just a
    _ <^> _ = Nothing


instance Falsifier (Option a) where
    false = Option Nothing
    {-# INLINE false #-}

instance Andlike (Option a) where
    (Option Nothing) >&> _ = Option Nothing
    _ >&> (Option Nothing) = Option Nothing
    _ >&> a = a
    {-# INLINE (>&>) #-}

instance Orlike (Option a) where
    (Option (Just a)) <|< _ = Option (Just a)
    _ <|< (Option (Just a)) = Option (Just a)
    _ <|< _ = Option Nothing

instance Xorlike (Option a) where
    (Option (Just a)) <^> (Option Nothing) = Option (Just a)
    (Option Nothing) <^> (Option (Just a)) = Option (Just a)
    _ <^> _ = Option Nothing


instance Andlike (Either a b) where
    (Left a) >&> _ = Left a
    _ >&> (Left b) = Left b
    _ >&> b = b
    {-# INLINE (>&>) #-}

instance Orlike (Either a b) where
    (Right b) <|< _ = Right b
    _ <|< (Right b) = Right b
    (Left a) <|< _ = Left a

instance Falsifier a => Xorlike (Either a b) where
    (Right b) <^> (Left a) = Right b
    (Left a) <^> (Right b) = Right b
    _ <^> _ = Left false


instance Falsifier ([] a) where
    false = []
    {-# INLINE false #-}

instance Andlike ([] a) where
    [] >&> _ = []
    _ >&> [] = []
    _ >&> a = a
    {-# INLINE (>&>) #-}

instance Orlike ([] a) where
    xs@(_:_) <|< _ = xs
    _ <|< xs@(_:_) = xs
    _ <|< _ = []

instance Xorlike ([] a) where
    xs@(_:_) <^> [] = xs
    [] <^> xs@(_:_) = xs
    _ <^> _ = []


instance Falsifier T.Text where
    false = T.empty

instance Andlike T.Text where
    ta >&> tb
        | T.null ta || T.null tb = T.empty
        | otherwise              = tb

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


instance Falsifier B.ByteString where
    false = B.empty

instance Andlike B.ByteString where
    ba >&> bb
        | B.null ba || B.null bb = B.empty
        | otherwise              = bb

instance Orlike B.ByteString where
    ta <|< tb
        | not (B.null ta) = ta
        | not (B.null tb) = tb
        | otherwise = B.empty

instance Xorlike B.ByteString where
    ta <^> tb
        | not (B.null ta) && B.null tb = ta
        | B.null ta && not (B.null tb) = tb
        | otherwise = B.empty


instance (Falsifier a, Falsifier b) => Falsifier (a, b) where
    false = (false, false)
    {-# INLINE false #-}

instance (Andlike a, Andlike b) => Andlike (a, b) where
    (a1, b1) >&> (a2, b2) = (a1 >&> a2, b1 >&> b2)
    {-# INLINE (>&>) #-}

instance (Orlike a, Orlike b) => Orlike (a, b) where
    (a1, b1) <|< (a2, b2) = (a1 <|< a2, b1 <|< b2)
    {-# INLINE (<|<) #-}

instance (Falsifier a, Falsifier b, Falsifier c) => Falsifier (a, b, c) where
    false = (false, false, false)
    {-# INLINE false #-}

instance (Andlike a, Andlike b, Andlike c) => Andlike (a, b, c) where
    (a1, b1, c1) >&> (a2, b2, c2) = (a1 >&> a2, b1 >&> b2, c1 >&> c2)
    {-# INLINE (>&>) #-}

instance (Orlike a, Orlike b, Orlike c) => Orlike (a, b, c) where
    (a1, b1, c1) <|< (a2, b2, c2) = (a1 <|< a2, b1 <|< b2, c1 <|< c2)
    {-# INLINE (<|<) #-}

instance (Falsifier a, Falsifier b, Falsifier c, Falsifier d) => Falsifier (a, b, c, d) where
    false = (false, false, false, false)
    {-# INLINE false #-}

instance (Andlike a, Andlike b, Andlike c, Andlike d) => Andlike (a, b, c, d) where
    (a1, b1, c1, d1) >&> (a2, b2, c2, d2) = (a1 >&> a2, b1 >&> b2, c1 >&> c2, d1 >&> d2)
    {-# INLINE (>&>) #-}

instance (Orlike a, Orlike b, Orlike c, Orlike d) => Orlike (a, b, c, d) where
    (a1, b1, c1, d1) <|< (a2, b2, c2, d2) = (a1 <|< a2, b1 <|< b2, c1 <|< c2, d1 <|< d2)
    {-# INLINE (<|<) #-}


-- }}}


-- | Flipped version of '>&>'. Returns the leftmost argument on success.
(<&<) :: Andlike a => a -> a -> a
(<&<) = flip (>&>)
{-# INLINE (<&<) #-}

-- | Flipped version of '<|<'. Returns the leftmost argument on success.
(>|>) :: Orlike a => a -> a -> a
(>|>) = flip (<|<)
{-# INLINE (>|>) #-}

-- | Returns the last element on success of all values.
andlast :: (Andlike a, Falsifier a, Foldable t) => t a -> a
andlast as
    | null as   = false
    | otherwise = foldr1 (>&>) as
{-# INLINE andlast #-}

-- | Returns the first element on success of all values.
andhead :: (Andlike a, Falsifier a, Foldable t) => t a -> a
andhead as
    | null as   = false
    | otherwise = foldr1 (<&<) as
{-# INLINE andhead #-}

-- | Monadic append with the annihilating operator guarding each argument.
--   Returns the mappended result on success.
andmappend :: (Andlike a, Monoid a) => a -> a -> a
andmappend a b = (a <&< b) `mappend` (a >&> b)
{-# INLINE andmappend #-}

-- | Monadic concatenation with the annihilating operator guarding each argument.
andmconcat :: (Andlike a, Falsifier a, Monoid a, Foldable t) => t a -> a
andmconcat as
    | null as   = false
    | otherwise = foldr1 andmappend as
{-# INLINE andmconcat  #-}

isFalsifier :: (Falsifier a, Eq a) => a -> Bool
isFalsifier = (false ==)
{-# INLINE isFalsifier #-}

-- | Discard the argument and return 'false'.
fvoid :: Falsifier a => a -> a
fvoid = const false
{-# INLINE fvoid #-}

