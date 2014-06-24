
-- Specifications of functions from the 'adjunctions' package

unit :: a -> Behavior (Delayed a)
counit :: Delayed (Behavior a) -> a
leftAdjunct :: (Delayed a -> b) -> a -> Behavior b
rightAdjunct :: (a -> Behavior b) -> Delayed a -> b

zipR :: (Behavior a, Behavior b) -> Behavior (a, b)
splitL :: Delayed a -> (a, Delayed ())

tabulateAdjunction :: (Delayed () -> b) -> Behavior b
tabulateAdjunction :: (Time       -> b) -> Behavior b

-- This is snapshot and snapshotWith from reactive!
indexAdjunction :: Behavior b -> Delayed a -> b
zapWithAdjunction :: (a -> b -> c) -> Behavior a -> Delayed b -> c

