
{-
    Semantics of this type.
    Especially: what is the monoid and what is the action.
-}

{-
-- 0.5 -> staccato/spicc, 1 -> detache/ord, 2 -> legato/bow
-- This may be a logarithm of the relative duration
type Separation = Double 

-- -1 -> start of phrase, -> 0 middle of phrase, -> 1 end of phrase
-- This may be a logarithm of the relative duration
type Weight     = Double

-- 0.5 -> under-accentuated, 1 -> normal accentuation, 2 -> over-accentuated
-- This may be a logarithm of the relative dynamic
type IndAcc     = Double
-- type Articulation = Time -> (Product Separation, Product Weight, Product IndAcc)
-}

