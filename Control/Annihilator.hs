
-- | A class for Annihilators, which define a binary function '>|>' that follows
-- the mathematical properties of: absorbing element, associativity, and
-- commutativity.
module Control.Annihilator
    ( Annihilator(..)
    , (<|<)
    , aconcat
    , amappend
    , amconcat
    , avoid
    )
    where

-- | Annihilators are semigroups with annihilators, i.e. the
-- following laws should hold:
--
-- prop> ann >|> b = ann
-- prop> a >|> ann = ann
-- prop> a >|> b = b
class Annihilator a where
    -- | Annihilating element of '>|>'.
    ann :: a

    -- | Annihilating operator, returns the rightmost element if no
    -- annihilators 'ann' are present.
    (>|>) :: a -> a -> a


instance Annihilator () where
    ann = ()

    _ >|> _ = ()

instance Annihilator (Maybe a) where
    ann = Nothing

    Nothing >|> _ = Nothing
    _ >|> Nothing = Nothing
    _ >|> a = a

instance Annihilator ([] a) where
    ann = []

    [] >|> _ = []
    _ >|> [] = []
    _ >|> a = a

instance (Annihilator a, Annihilator b) => Annihilator (a, b) where
    ann = (ann, ann)

    (a1, b1) >|> (a2, b2) = (a1 >|> a2, b1 >|> b2)


-- | Flipped version of '>|>'.
(<|<) :: Annihilator a => a -> a -> a
(<|<) = flip (>|>)

-- | Annihilating concatenation.
aconcat :: (Annihilator a, Foldable t) => t a -> a
aconcat as
    | null as   = ann
    | otherwise = foldr1 (>|>) as

-- | Monadic append with the annihilating operator guarding each argument.
amappend :: (Annihilator a, Monoid a) => a -> a -> a
amappend a b = (a >|> b) `mappend` (a <|< b)

-- | Monadic concatenation with the annihilating operator guarding each argument.
amconcat :: (Annihilator a, Monoid a, Foldable t) => t a -> a
amconcat as
    | null as   = ann
    | otherwise = foldr1 amappend as

-- | Discard the argument and return 'ann'.
avoid :: Annihilator a => a -> a
avoid = const ann

