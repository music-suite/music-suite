
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens

data Foo a = Foo { _bar :: Int, _baz :: Int, _quux :: a }
    deriving (Eq,Show)

-- bar, baz :: Simple Lens (Foo a) Int
-- quux :: Lens (Foo a) (Foo b) a b
makeLenses ''Foo