




-- Apply to each part
mapParts  :: HasPart     => [a -> b] -> S a -> S b

-- Apply to each phrase
-- A phrase in this context is everything tagged with startPhrase, midPhrase, endPhrase 
-- Could be automatically determined by looking at rest structure
mapPhrase :: HasPhrase a => [a -> b] -> S a -> S b
-- from this, derive (first, middle, last)


mapPartPhrase ::              [[a -> b]] -> S a -> S b
mapPhrasePart ::              [[a -> b]] -> S a -> S b

-- TODO How to combine these with slice/before/after

-- TODO Switch name of mapAllParts/mapParts?

-- TODO note that the trio mapParts/mapAllParts/modifyParts apply for other aspects as well,
-- notably mapPitch/mapAllPitches/modifyPitches

-- TODO note that the "selections/folds" defined here could be used to implement the special maps
-- *or* the filter, and one in terms of the other. Compare then lens libraries.
