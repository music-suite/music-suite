{-  
        Semitones is the smallest musical unit (Semitones in Western music)
        
        The `semitones` function retrieves the number of Semitones in a pitch, for example
            semitones :: Interval -> Semitones
            semitones major third = 4

        Note that semitones is surjetive. We can define a non-deterministic function `spellings`
            spellings :: Semitones -> [Interval]
            spellings 4 = [majorThird, diminishedFourth]
        Law
            map semitones (spellings a) = replicate n a    for all n > 0
        Lemma
            map semitones (spellings a)
        

        isHemitonic   [1,2,2] = True
        isHemitonic   [2,2,2] = False
        isCohemitonic [1,1,2] = True
        isCohemitonic [1,2,1] = False
        isTritonic ...
        
        A Scale is a [Semitones], for example [2,2,1,2,2,2,1]
            From this we can derive       [2,4,5,7,9,11,12]
        A Scale is a function (Number -> Interval)
        A Scale is a function (Number -> Semitones)

        -- TODO simplify etc
        isMelodicDissonance :: Interval -> Bool


    "Post-tonal"
    
        Messiaen
        
        mode1 = [2,2,2,2,2]
        mode2 = [1,2, 1,2, 1,2, 1,2]
        mode3 = [2,1,1, 2,1,1, 2,1,1]
        mode4 = [1,1,3,1,   1,1,3,1]
        mode5 = [1,4,1,     1,4,1]
        mode6 = [2,2,1,1,   2,2,1,1]
        mode7 = [1,1,1,2,1, 1,1,1,2,1]


-   Old stuff>



-- Semitone is an enumerated associated type
type family Semitone a :: *
type family Alteration a :: *

-- A scale is a function :: Semitone a -> a
newtype Scale a = Scale { getScale :: [Semitone a] } 
-- Eq, Show

semitone :: Scale a -> Semitone a -> a
semitone = undefined


semitone (Scale xs) p = xs !! (fromIntegral p `mod` length xs)


fromSemitone :: (Num a, Ord a, Integral b, Num c) => Scale a -> b -> c
fromSemitone (Scale xs) p = fromIntegral $ fromMaybe (length xs - 1) $ List.findIndex (>= fromIntegral p) xs

scaleFromSemitones :: Num a => [a] -> Scale a
scaleFromSemitones = Scale . accum
    where
        accum = snd . List.mapAccumL add 0
        add a x = (a + x, a + x)

-- numberOfSemitones :: Scale a -> Int
numberOfSemitones = length . getScale

major :: Num a => Scale a
major = scaleFromSemitones [0,2,2,1,2,2,2,1]

naturalMinor :: Num a => Scale a
naturalMinor = scaleFromSemitones [0,2,1,2,2,1,2,2]

harmonicMinor :: Num a => Scale a                     
harmonicMinor = scaleFromSemitones [0,2,1,2,2,1,3,1]

-}
