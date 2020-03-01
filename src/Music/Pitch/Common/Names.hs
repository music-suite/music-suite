{-# OPTIONS_GHC -Wall
  -Wcompat
  -Wincomplete-record-updates
  -Wincomplete-uni-patterns
  -Werror
  -fno-warn-name-shadowing
  -fno-warn-unused-imports
  -fno-warn-redundant-constraints
  #-}
-- | Common pitch names in various (human) languages.
module Music.Pitch.Common.Names
  ( Language (..),
    Mode (..),
    showKey,
    showPitch,
    showMode,
    showSep,
  )
where

import qualified Data.List
import Data.Maybe
import Music.Pitch.Common.Interval
import Music.Pitch.Common.Pitch
import Music.Pitch.Literal

{-
Original at http://tinyurl.com/ongo4al
Use convertcsv.com to convert to JSON array
-}

data Language
  = English
  | German
  | Dutch
  | Japanese
  | Italian
  | French
  | Spanish
  | Portuguese
  | Russian
  | Romanian
  | Swedish
  | NewSwedish
  deriving (Show, Eq, Ord)

data Mode = MajorMode | MinorMode
  deriving (Eq, Ord, Show)

showKey :: Language -> Pitch -> Mode -> String
showKey lang pitch mode = showPitch lang pitch ++ showSep lang ++ showMode lang mode

showPitch :: Language -> Pitch -> String
showPitch lang pitch = (!! (pitchToIndex + pitchNameOffset)) $ fromMaybe (error "showPitch: Bad lang") $ listToMaybe $ filter (\xs -> head xs == show lang) $ pitchNames
  where
    -- TODO normalize dbb etc.
    pitchToIndex =
      fromMaybe (error "showPitch: Bad pitch") $
        Data.List.findIndex
          (== pitch)
          [ cb,
            c,
            cs,
            db,
            d,
            ds,
            eb,
            e {-es,-},
            {-fb,-}
            f,
            fs,
            gb,
            g,
            gs,
            ab,
            a,
            as,
            bb,
            b,
            bs
          ]
    pitchNameOffset = 3

showMode :: Language -> Mode -> String
showMode lang mode = (!! (modeToIndex + modeNameOffset)) $ fromMaybe (error "showMode: Bad lang") $ listToMaybe $ filter (\xs -> head xs == show lang) $ modeNames
  where
    modeToIndex =
      fromMaybe (error "showPitch: Bad mode") $
        Data.List.findIndex
          (== mode)
          [MajorMode, MinorMode]
    modeNameOffset = 1

showSep :: Language -> String
showSep lang = fromMaybe (error "showSep: Bad lang") $ Data.List.lookup (show lang) nameModeSeparator

pitchNames :: [[String]]
-- [[language,_,_,cb,c,cs,db...bs]]
pitchNames =
  [ [ "English",
      "",
      "",
      "C flat",
      "C",
      "C sharp",
      "D flat",
      "D",
      "D sharp",
      "E flat",
      "E",
      "F",
      "F sharp",
      "G flat",
      "G",
      "G sharp",
      "A flat",
      "A",
      "A sharp",
      "B flat",
      "B",
      "B sharp"
    ],
    [ "German",
      "",
      "",
      "Ces",
      "C",
      "Cis",
      "Des",
      "D",
      "Dis",
      "Es",
      "E",
      "F",
      "Fis",
      "Ges",
      "G",
      "Gis",
      "As",
      "A",
      "Ais",
      "B",
      "H",
      "His"
    ],
    [ "Dutch",
      "(Netherlands)",
      "(Written)",
      "Ces / C mol",
      "C",
      "Cis / C kruis",
      "Des / D mol",
      "D",
      "Dis / D kruis",
      "Es / E mol",
      "E",
      "F",
      "Fis / F kruis",
      "Ges / G mol",
      "G",
      "Gis / G kruis",
      "As / A mol",
      "A",
      "Ais / A kruis",
      "Bes / B mol",
      "B",
      "Bis / B kruis"
    ],
    [ "Japanese",
      "",
      "",
      "\22793\12495 (hen-ha)",
      "\12495 (ha)",
      "\23344\12495 (ei-ha)",
      "\22793\12491 (hen-ni)",
      "\12491 (ni)",
      "\23344\12491 (ei-ni)",
      "\22793\12507 (hen-ho)",
      "\12507 (ho)",
      "\12504 (he)",
      "\23344\12504 (ei-he)",
      "\22793\12488 (hen-to)",
      "\12488 (to)",
      "\23344\12488 (ei-to)",
      "\22793\12452 (hen-i)",
      "\12452 (i)",
      "\23344\12452 (ei-i)",
      "\22793\12525 (hen-ro)",
      "\12525 (ro)",
      ""
    ],
    [ "Italian",
      "",
      "",
      "Do bemolle",
      "Do",
      "Do diesis",
      "Re bemolle",
      "Re",
      "Re diesis",
      "Mi bemolle",
      "Mi",
      "Fa",
      "Fa diesis",
      "Sol bemolle",
      "Sol",
      "Sol diesis",
      "La bemolle",
      "La",
      "La diesis",
      "Si bemolle",
      "Si",
      "Si diesis"
    ],
    [ "French",
      "",
      "",
      "Do b\233mol",
      "Do (Ut)",
      "Do di\232se",
      "R\233 b\233mol",
      "R\233",
      "R\233 di\232se",
      "Mi b\233mol",
      "Mi",
      "Fa",
      "Fa di\232se",
      "Sol b\233mol",
      "Sol",
      "Sol di\232se",
      "La b\233mol",
      "La",
      "La di\232se",
      "Si b\233mol",
      "Si",
      "Si di\232se"
    ],
    [ "Spanish",
      "",
      "",
      "Do bemol",
      "Do",
      "Do sostenido",
      "Re bemol",
      "Re",
      "Re sostenido",
      "Mi bemol",
      "Mi",
      "Fa",
      "Fa sostenido",
      "Sol bemol",
      "Sol",
      "Sol sostenido",
      "La bemol",
      "La",
      "La sostenido",
      "Si bemol",
      "Si",
      "Si sostenido"
    ],
    [ "Portuguese",
      "",
      "",
      "D\243 bemol",
      "D\243",
      "D\243 sustenido",
      "R\233 bemol",
      "R\233",
      "R\233 sustenido",
      "Mi bemol",
      "Mi",
      "F\225",
      "F\225 sustenido",
      "Sol bemol",
      "Sol",
      "Sol sustenido",
      "L\225 bemol",
      "L\225",
      "L\225 sustenido",
      "Si bemol",
      "Si",
      "Si sustenido"
    ],
    [ "Russian",
      "",
      "",
      "\1044\1086-\1073\1077\1084\1086\1083\1100",
      "\1044\1086",
      "\1044\1086-\1076\1080\1077\1079",
      "\1056\1077-\1073\1077\1084\1086\1083\1100",
      "\1056\1077",
      "\1056\1077-\1076\1080\1077\1079",
      "\1052\1080-\1073\1077\1084\1086\1083\1100",
      "\1052\1080",
      "\1060\1072",
      "\1060\1072-\1076\1080\1077\1079",
      "\1057\1086\1083\1100-\1073\1077\1084\1086\1083\1100",
      "\1057\1086\1083\1100",
      "\1057\1086\1083\1100-\1076\1080\1077\1079",
      "\1051\1103-\1073\1077\1084\1086\1083\1100",
      "\1051\1103",
      "\1051\1103-\1076\1080\1077\1079",
      "\1057\1080-\1073\1077\1084\1086\1083\1100",
      "\1057\1080",
      ""
    ],
    [ "Romanian",
      "",
      "",
      "Do bemol",
      "Do",
      "Do diez",
      "Re bemol",
      "Re",
      "Re diez",
      "Mi bemol",
      "Mi",
      "Fa",
      "Fa diez",
      "Sol bemol",
      "Sol",
      "Sol diez",
      "La bemol",
      "La",
      "La diez",
      "Si bemol",
      "Si",
      "Si diez"
    ],
    [ "Belgian dutch",
      "",
      "",
      "Do mol",
      "Do",
      "Do kruis",
      "Re mol",
      "Re",
      "Re kruis",
      "Mi mol",
      "Mi",
      "Fa",
      "Fa kruis",
      "Sol mol",
      "Sol",
      "Sol kruis",
      "La mol",
      "La",
      "La kruis",
      "Si mol",
      "Si",
      "Si kruis"
    ],
    [ "Swedish",
      "",
      "",
      "Cess",
      "C",
      "Ciss",
      "Dess",
      "D",
      "Diss",
      "Ess",
      "E",
      "F",
      "Fiss",
      "Gess",
      "G",
      "Giss",
      "Ass",
      "A",
      "Aiss",
      "B",
      "H",
      "Hiss"
    ],
    [ "NewSwedish",
      "",
      "",
      "Cess",
      "C",
      "Ciss",
      "Dess",
      "D",
      "Diss",
      "Ess",
      "E",
      "F",
      "Fiss",
      "Gess",
      "G",
      "Giss",
      "Ass",
      "A",
      "Aiss",
      "Bess",
      "B",
      "Biss"
    ]
  ]

modeNames :: [[String]] -- [[language,major,minor]]
modeNames =
  [ ["English", "major", "minor"],
    ["German", "Dur", "Moll"],
    ["Dutch", "groot", "klein"],
    ["Japanese", "\38263\35519 (ch\333ch\333)", "\30701\35519 (tanch\333)"],
    ["Italian", "maggiore", "minore"],
    ["French", "majeur", "mineur"],
    ["Spanish", "mayor", "menor"],
    ["Portuguese", "maior", "menor"],
    ["Russian", "\1084\1072\1078\1086\1088", "\1084\1080\1085\1086\1088"],
    ["Romanian", "major", "minor"],
    ["Belgian dutch", "groot", "klein"],
    ["Swedish", "dur", "moll"],
    ["NewSwedish", "dur", "moll"],
    ["", "dur", "moll"]
  ]

nameModeSeparator :: [(String, String)] -- [(language,separator)]
nameModeSeparator =
  [ ("English", " "),
    ("German", "-"),
    ("Dutch", " "),
    ("Japanese", " "),
    ("Italian", " "),
    ("French", " "),
    ("Spanish", " "),
    ("Portuguese", " "),
    ("Russian", " "),
    ("Romanian", " "),
    ("Belgian dutch", " "),
    ("Swedish", "-"),
    ("NewSwedish", "-")
  ]
