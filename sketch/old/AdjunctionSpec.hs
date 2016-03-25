
-- Specifications of functions from the 'adjunctions' package

unit :: a -> Behavior (Future a)
counit :: Future (Behavior a) -> a
leftAdjunct :: (Future a -> b) -> a -> Behavior b
rightAdjunct :: (a -> Behavior b) -> Future a -> b

zipR :: (Behavior a, Behavior b) -> Behavior (a, b)
splitL :: Future a -> (a, Future ())

tabulateAdjunction :: (Future () -> b) -> Behavior b
tabulateAdjunction :: (Time      -> b) -> Behavior b

-- This is snapshot and snapshotWith from reactive!
indexAdjunction :: Behavior b -> Future a -> b
zapWithAdjunction :: (a -> b -> c) -> Behavior a -> Future b -> c

