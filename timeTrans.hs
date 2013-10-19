

cresc p f $ stretch 2 $ c 
    ==> crescendo has duration 1

stretch 2 $ cresc p f $ c 
    ==> crescendo has duration 2
    


type TimedTransformation a = Transformation Time :+: Transformation a

instance Action (Transformation a) a where
-- Time transformation act on time etc
instance Action (TimedTransformation a) a where