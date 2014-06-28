
module Control.Lens.Cons.Middle (
      _middle
  ) where

import Control.Lens

_middle :: (Snoc s s a a, Cons s s b b) => Traversal' s s
_middle = _tail._init
