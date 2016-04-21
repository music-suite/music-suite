
{-# LANGUAGE FlexibleContexts #-}

module Music.Parts.Part
(
Part,
_solo,
subpart,
instrument,
divide,
containsPart,
smallestPart,
smallestSubpart,
largestPart,
largestSubpart,
distinctFrom,
allDistinct,
solo,
tutti,
)
where

import           Control.Applicative
import           Control.Lens                    (toListOf, Lens, Lens', (^.))
import           Data.Aeson                      (ToJSON (..), FromJSON(..))
import qualified Data.Aeson
import           Data.Default
-- import           Data.Monoid
-- import           Control.Lens (set)
import           Data.Functor.Adjunction         (unzipR)
import qualified Data.List
import           Data.Maybe
import           Data.Semigroup
import           Data.Semigroup.Option.Instances
import           Data.Traversable                (traverse)
import           Data.Typeable
import           Text.Numeral.Roman              (toRoman)

import           Music.Parts.Division
import           Music.Parts.Solo
import           Music.Parts.Instrument
import           Music.Parts.Subpart


-- | A part is a subdivided group of instruments of a given type.
data Part = Part
  Solo        -- Solo vs. tutti
  Instrument  -- Type of instrument
  Subpart     -- Subdivision within instrument chorus
  -- TODO Layer
    deriving (Eq, Ord)

instance Show Part where
    show (Part Solo instr subp) = "Solo " ++ show instr ++ addS (show subp)
        where
            addS "" = ""
            addS x = " " ++ x
    show (Part _ instr subp)    = show instr ++ addS (show subp)
        where
            addS "" = ""
            addS x = " " ++ x

-- Semantics: Monoid (Option . First)
instance Monoid Part where
  mempty = def
  mappend x y
    | x == mempty = y
    | otherwise   = x

instance Semigroup Part where
  (<>) = mappend

instance Default Part where
  def = Part def def def

instance ToJSON Part where
  toJSON p = Data.Aeson.object [
    ("instrument", toJSON $ p^.instrument),
    ("subpart",    toJSON $ p^.subpart),
    ("solo",       toJSON $ p^._solo)
    ]

instance FromJSON Part where
  parseJSON (Data.Aeson.Object v) = do
    s <- v Data.Aeson..: "solo"
    i <- v Data.Aeson..: "instrument"
    u <- v Data.Aeson..: "subpart"
    return $ Part s i u
  parseJSON _ = empty

-- |
-- @a \`containsPart\` b@ holds if the set of players represented by a is an improper subset of the
-- set of players represented by b.
containsPart :: Part -> Part -> Bool
Part solo1 instr1 subp1 `containsPart` Part solo2 instr2 subp2 =
        solo1 == solo2
        && instr1 == instr2
        && subp1 `containsSubpart` subp2

smallestPart :: Part -> Part -> Part
smallestPart p1@(Part _ _ sp1) p2@(Part _ _ sp2)
  | sp1 `smallestSubpart` sp2 == sp1 = p1
  | sp1 `smallestSubpart` sp2 == sp2 = p2

smallestSubpart :: Subpart -> Subpart -> Subpart
smallestSubpart x y
  | x `isProperSubpartOf` y = x
  | y `isProperSubpartOf` x = y
  -- arbitrarily:
  | otherwise               = x

largestPart :: Part -> Part -> Part
largestPart p1@(Part _ _ sp1) p2@(Part _ _ sp2)
  | sp1 `largestSubpart` sp2 == sp1 = p1
  | sp1 `largestSubpart` sp2 == sp2 = p2

largestSubpart :: Subpart -> Subpart -> Subpart
largestSubpart x y
  | x `isProperSubpartOf` y = y
  | y `isProperSubpartOf` x = x
  -- arbitrarily:
  | otherwise               = x


-- | Returns 'True' iff all given parts are distinct (as per 'distinctFrom').
allDistinct :: [Part] -> Bool
allDistinct []     = True
allDistinct (x:xs) = all (distinctFrom x) xs && allDistinct xs

-- | Returns 'True' iff x and y are completely distinct, i.e. neither contains the other.
--
-- >>> violins `distinctFrom` trumpets
-- True
-- >>> violins `distinctFrom` violins
-- False
-- >>> violins `distinctFrom` violins1
-- False
-- >>> violins1 `distinctFrom` violins
-- False
-- >>> violins1 `distinctFrom` violins2
-- True
--
distinctFrom :: Part -> Part -> Bool
distinctFrom (Part s1 i1 sp1) (Part s2 i2 sp2) = s1 /= s2 || i1 /= i2 || noneSubpart
  where
    -- Is this needed?
    noneSubpart = not (sp1 `isSubpartOf` sp2) && not (sp2 `isSubpartOf` sp1)



    -- if equal
    -- [pa',pb'] = divide 2 pa

_solo :: Lens' Part Solo
_solo f (Part s i u) = fmap (\s -> Part s i u) $ f s

subpart :: Lens' Part Subpart
subpart f (Part s i u) = fmap (\u -> Part s i u) $ f u

instrument :: Lens' Part Instrument
instrument f (Part s i u) = fmap (\i -> Part s i u) $ f i

-- | Divide a part into @n@ subparts.
divide :: Int -> Part -> [Part]
divide n (Part solo instr subp) = fmap (\x -> Part solo instr (subp <> Subpart [x])) $ divisions n

solo instr      = Part Solo instr def
tutti instr     = Part Tutti instr def
