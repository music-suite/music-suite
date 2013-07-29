


type Parser a b = [a] -> Either String b
type RhParser a = Parser (DurationT, a) (Rhythm a)

instance Functor (Parser a)
instance Applicative (Parser a)
instance Alternative (Parser a)
instance Monad (Parser a)
instance MonadPlus (Parser a)
instance Monoid (Parser a)

-- Possibility to define standard quantizations for 4/4, 3/4 etc
-- Prefer local tuplets to global etc


-- Top-level:
--
-- 4/4      rh2 rh2 rh2 rh2
--          rh2     rh2 rh2
--          rh2 rh2     rh2
--          rh2 rh2 rh2




-- etc
rh6
rh6
rh5

rh3
    -- (notated) duration is multiple of 3
    -- either: 
    --  dotted note
    --  rh2*2 followed by rh2

rh2 -- 2-based rhythm
    -- (notated) duration is multiple of 2
    -- either:
    --  single note 
    --  rh2 followed by rh2
    --  rh3 followed by rh2/2
    --  rh2/2 followed by rh3
