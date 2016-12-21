{-# LANGUAGE     
    DeriveFunctor,
    DeriveFoldable,
    DeriveTraversable,
    NoMonomorphismRestriction,
    GeneralizedNewtypeDeriving,
    StandaloneDeriving,
    TypeFamilies,
    ViewPatterns,

    MultiParamTypeClasses,
    TypeSynonymInstances, -- TODO remove
    FlexibleInstances,
    
    OverloadedStrings,
    TypeOperators
    #-}


module Trans3 where

import Data.Semigroup

data Tree a 
    = Tip a                 
    -- | Trans ts xs transforms all elements in xs that matches
    --   a predicate. The predicate is applied past *lower* transformations than this one.
    --   The first partial transformation to succeed is applied, if none succeeds no
    --   transformation takes place.
    | Trans [{-(a -> Maybe a)-}()] (Tree a) 
    | Bin (Tree a) (Tree a)
    deriving (Functor)
    
sem_Tree :: Tree a -> [a]
sem_Tree = go
    where
        go (Tip x)      = [x]
        go (Trans fs x) = undefined
        go (Bin x y)    = go x <> go y