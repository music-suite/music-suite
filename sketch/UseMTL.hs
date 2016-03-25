
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFoldable, DeriveTraversable #-}
import Control.Applicative
import Data.Functor.Identity
import Control.Monad.Writer.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
-- import Control.Monad.RWST
-- import Control.Monad.List.Class
import Control.Monad.Writer
import Control.Monad.List
import Control.Lens(Lens, Lens', lens, to, set, Getter, (^.), iso, Iso)
import Data.Bifunctor

-- type Writer  w   a = (a, w)
-- type WriterT w m a = m (a, w)
-- type ListT     m a = m [a]

newtype Meta = Meta () -- Note MUST be a commutative Monoid as per below
  deriving (Monoid, Show, Eq, Ord)
newtype Duration = Duration (Product Double)
  deriving (Monoid, Num, Show, Eq, Ord)
newtype Note a = Note (Writer Duration a)
  deriving (Functor, Applicative, Monad, MonadWriter Duration, Foldable, Traversable, Eq, Ord)

toNote :: (a, Duration) -> Note a
toNote = Note . WriterT . Identity
-- RUNTIME: id

fromNote :: Note a -> (a, Duration)
fromNote (Note x) = runWriter x
-- RUNTIME: id

note :: Iso (Duration, a) (Duration, b) (Note a) (Note b)
note = flip iso (swap . fromNote) (toNote . swap)
  where
    swap (x, y) = (y, x)

-- See
-- http://www.randomhacks.net/2007/03/05/three-things-i-dont-understand-about-monads/
-- https://hackage.haskell.org/package/arrow-list-0.7/docs/Control-Monad-Sequence.html
-- http://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-List.html

-- ListT (Writer Meta) is a monad iff (Writer Meta) is commutative
-- (Writer Meta) is a commutative monad iff Meta is a commutative monoid
newtype Voice a = Voice (WriterT Duration (ListT (Writer Meta)) a)
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus, Foldable, Traversable, Show, Eq, Ord)
-- MonadWriter Duration
  -- actually not needed
instance Monoid (Voice a) where
  mempty = empty
  mappend = (<|>)
-- TODO need to duplicate MonadWriter to get the double nesting
-- (or simply derive the meta lens that we need manually!)

-- type Voice a
--   = WriterT Duration (ListT (Writer Meta)) a
--   = (ListT (Writer Meta)) (a, Duration)
--   = ListT (Writer Meta) (a, Duration)
--   = Writer Meta [(a, Duration)]
--   = ([(a, Duration)], Meta)



-- TODO voice :: Getter [Note a] (Voice a)
-- TODO notes :: Lens (Voice a) (Voice b) [Note a] [Note b]

_Wrapped :: Lens
  (Voice a)
  (Voice b)
  (WriterT Duration (ListT (Writer Meta)) a)
  (WriterT Duration (ListT (Writer Meta)) b)
_Wrapped f (Voice x) = Voice <$> f x
getVoice (Voice x) = x


-- TODO lens laws!
meta :: Lens' (Voice a) Meta
meta = lens getMeta (\x m -> modifyMeta (const m) x)

notes :: Lens (Voice a) (Voice b) [Note a] [Note b]
notes = lens getNotes (\x m -> modifyNotes (const m) x)



getMeta :: Voice a -> Meta
getMeta = runIdentity . execWriterT . runListT . runWriterT . getVoice
-- getMeta = runIdentity . fmap snd . runWriterT . runListT . runWriterT . getVoice
-- RUNTIME
-- getMeta = snd


modifyMeta :: (Meta -> Meta) -> Voice a -> Voice a
modifyMeta f = Voice . mapWriterT (mapListT (mapWriter (second f))) . getVoice
-- RUNTIME
-- modifyMeta = second

getNotes :: Voice a -> [Note a]
getNotes = fmap toNote . fst . runWriter . runListT . runWriterT . getVoice
-- RUNTIME
-- getNotes = fst

modifyNotes :: ([Note a] -> [Note b]) -> Voice a -> Voice b
modifyNotes f = Voice . mapWriterT (mapListT (mapWriter (first (fmap fromNote . f . fmap toNote)))) . getVoice
-- RUNTIME
-- modifyNotes = first

voice :: Getter [Note a] (Voice a)
voice = to $ \ns -> modifyNotes (const ns) empty

-- TODO show that mplus/(<|>)/mzero/empty/join/bind/fmap are the same as in the prev implementation
-- Everything else arise from TCs + voice/notes/meta including
--  Show, JSON, Transformable, HasDuration, Reversible, Splittable, all Is..., all Has...


-- Monad
-- Functor




-- (Meta, [(Duration, a)])


-- ListT



-- >>> [(1,())^.note, (2,())^.note]^.voice
--
-- With newtype:
--  Voice (WriterT (ListT (WriterT (Identity ([(a,Duration (Product {getProduct = 1.0})),(b,Duration (Product {getProduct = 2.0}))],Meta m))))
-- Runtime:
--  ((((([((),(1.0)),((),(2.0))],())))))
--  ([ ((a,1.0) , ((b,2.0) ], m
