

import Music.Prelude.Basic
import Music.Score.Import.Sibelius


main = do   
    result <- readSibEither "test.json"
    case result of
        Left e -> putStrLn $ "Error: " ++ e
        Right x -> do
            -- writeMidi "test.mid" $ asScore $ f x
            -- openXml $ f x
            openLy $ asScore $ f x

    -- let score = fromSib $ fromJust $ decode' json
    -- openLy score
    
    where                
        -- f = id   
        f = (\x -> text (show $ duration x) x) . retrograde
        -- f x = stretch (1/1) $ times 2 x |> times 2 (stretch 2 x)

        fromJust (Just x) = x
