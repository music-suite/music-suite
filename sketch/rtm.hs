
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Music.Prelude
import Control.Applicative
import Numeric.Positive
import qualified Text.Parsec as P



data Rtm = Rtm BeatCount RtmList deriving (Eq, Ord, Show)

type BeatCount = Positive
type RtmList   = [RtmValue]

data RtmValue
    = RtmRest { _duration :: Positive } 
    | RtmNote { _isTiedFromPreviousNote :: Bool, _duration :: Positive }
    | RtmEmbed Rtm
    deriving (Eq, Ord, Show)

rhythmToScore :: Rtm -> Score StandardNote 
rhythmToScore (Rtm bc rl) = mfilter (\x -> all (== c) $ map (! 0) $ x^..pitches')
  $ stretchTo (realToFrac bc) $ scat $ map rhythmListToScore rl -- TODO: use BeatCount

rhythmListToScore :: RtmValue -> Score StandardNote
rhythmListToScore (RtmRest d)       = stretch (realToFrac d) g_
rhythmListToScore (RtmNote True d)  = stretch (realToFrac d) c 
rhythmListToScore (RtmNote False d) = endTie $ stretch (realToFrac d) c 
rhythmListToScore (RtmEmbed r)      = rhythmToScore r

openRtm :: Rtm -> IO ()
openRtm = open . compress 4 . rhythmToScore


type Parser a = P.Parsec String () a

parseRtm :: String -> Either P.ParseError Rtm
parseRtm = doParse rhythmParser
  where
    doParse :: Parser a -> String -> Either P.ParseError a
    doParse p = P.parse p "No source"

rhythmParser :: Parser Rtm
rhythmParser = paren $ liftA2 Rtm (number <* space) rtmListParser
  where
    rtmListParser = paren $ P.sepBy1 rtmParser space
    rtmParser     = P.choice [restParser, noteParser, embedParser]
    noteParser    = P.choice [P.try $ RtmNote True <$> number <* dotParser <* zeros, RtmNote False <$> number]
    restParser    = RtmRest <$> (negSignParser *> number)
    embedParser   = RtmEmbed <$> (P.try (P.lookAhead paren1) *> rhythmParser)

    paren1        = P.char '('
    paren x       = P.char '(' *> x <* P.char ')'
    number        = fmap (fromInteger . read) $ (P.many1 P.digit)
    zeros         = P.many1 $ P.char '0'
    dotParser     = P.char '.'
    space         = P.char ' '
    negSignParser = P.char '-'
        














-- testRtm = Rtm 2 (replicate 3 $ RtmEmbed $ innerRtm)
--     where 
--         innerRtm = Rtm 1 (replicate 5 (RtmNote False 1))


-- (2 ((1 (1 1 1 1)) (1 (1 1 1 1)) (1 (1 1 1 1))))
-- (4 (1 (1 -1 1 -1)) (1 (1 1 2)) (1 (2.0 -1 1)) (1 (1.0 -2 1)))
-- (4 ((1 (1 -1 1 -1)) (1 (1 1 2)) (1 (2.0 -1 1)) (1 (1.0 -2 1))))

-- FIMXE unexpected results if first component in a group is negative (a rest)!
foo = openRtm $ fromRight $ parseRtm "(4 ((1 (1 -1 1 -1)) (1 (1 1 2)) (1 (2.0 -1 1)) (1 (1.0 -2 1))))"
fromRight (Right x) = x



