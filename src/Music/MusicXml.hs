
module Music.MusicXml -- (
--  ) 
where


data Score 
    = PartwiseScore
        ScoreAttrs
        ScoreHeader
        [[(MeasureAttrs, Music)]]
    | TimewiseScore
        ScoreAttrs
        ScoreHeader
        [(MeasureAttrs, [Music])]

data ScoreAttrs
    = ScoreAttrs
        -- id?

type Music = [MusicData]
data MusicData = MusicData

data MeasureAttrs
    = MeasureAttrs
        -- number
        -- i?mplicit
        -- nonContr?
        -- width?

data ScoreHeader
    = ScoreHeader
        -- titles?
        -- identification?
        -- defaults?
        -- credit*
        -- partlist?
        

data Defaults
    = Defaults
        -- page layout (marigins, distance etc)
        -- system layout
        -- staff layout
        -- scaling
        -- appearance (line width etc)
