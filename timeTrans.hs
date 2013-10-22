
{-# LANGUAGE     
    DeriveFunctor,
    DeriveFoldable,
    DeriveTraversable,
    NoMonomorphismRestriction,
    GeneralizedNewtypeDeriving,
    StandaloneDeriving,
    TypeFamilies,
    ViewPatterns,
    RankNTypes,
    PackageImports,
    MultiParamTypeClasses,
    
    OverloadedStrings,
    TypeOperators,
    FlexibleContexts,
    
    TemplateHaskell
    #-}

module TimeTrans where

import Prelude hiding (span) -- TODO

import Control.Compose ((~>))
import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Monad.Plus
import "mtl" Control.Monad.Writer

-- import Control.Lens
import Data.Key
import Data.Maybe
import Data.Tree
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup
import Data.Monoid.Action
import Data.Monoid.Coproduct
import Data.Foldable
import Data.Traversable
import qualified Data.Foldable as F
import qualified Data.Traversable as T

{-

cresc p f $ stretch 2 $ c 
    ==> crescendo has duration 1

stretch 2 $ cresc p f $ c 
    ==> crescendo has duration 2
    
-}

type Time = Double
newtype TFun a = TFun { getTFun :: Time -> a }
type Transformation a = Endo a

type TimedTransformation a = Transformation Time :+: Transformation a

instance Action (Transformation Time) (TFun a) where
-- instance Action (Transformation a) a where
-- instance Action (TimedTransformation a) a where