

import Music.Preludes.Basic
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
        f = retrograde
        -- f x = stretch (1/4) $ times 2 x |> times 2 (stretch 2 x)

        fromJust (Just x) = x
