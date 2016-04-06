
Please see [Hackage](http://hackage.haskell.org/package/annihilator-0.1.0.0)

Documentation
=============

class Annihilator a where [Source](Control/Annihilator.hs#L24)

Annihilators are semigroups with annihilators, i.e. the following laws should hold:

    ann >|> b = ann

    a >|> ann = ann

    a >|> b = b

###Methods

    ann :: a

Annihilating element of `>|>`.

    (\>|\>) :: a -\> a -\> a

Annihilating operator, returns the rightmost element if no annihilators `ann` are present.

###Instances

* [Annihilator](Control/Annihilator.hs#L33) ()
* [Annihilator](Control/Annihilator.hs#L45) [a]
* [Annihilator](Control/Annihilator.hs#L38) ([Maybe](https://hackage.haskell.org/package//base-4.8.2.0/Data-Maybe.html#t:Maybe) a\)
* (Annihilator a, Annihilator b) =\> [Annihilator](Control/Annihilator.hs#L52) (a, b)

 (\<|\<) :: [Annihilator](Control-Annihilator.html#t:Annihilator) a =\> a -\> a -\> a

Flipped version of `>|>`.

aconcat :: [Annihilator](Control-Annihilator.html#t:Annihilator) a =\> [a] -\> a

Annihilating concatenation.

amappend :: ([Annihilator](Control-Annihilator.html#t:Annihilator) a, [Monoid](../base-4.8.2.0/Data-Monoid.html#t:Monoid) a) =\> a -\> a -\> a

Monadic append with the annihilating operator guarding each argument.

amconcat :: ([Annihilator](Control-Annihilator.html#t:Annihilator) a, [Monoid](../base-4.8.2.0/Data-Monoid.html#t:Monoid) a) =\> [a] -\> a

Monadic concatenation with the annihilating operator guarding each argument.

avoid :: [Annihilator](Control-Annihilator.html#t:Annihilator) a =\> a -\> a

Discard the argument and return `ann`.

Produced by [Haddock](http://www.haskell.org/haddock/) version 2.16.1
