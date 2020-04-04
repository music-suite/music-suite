
import qualified Data.ByteString

main = do
    Data.ByteString.writeFile "LE.bom" (Data.ByteString.pack [0xFF,0xFE])
    Data.ByteString.writeFile "BE.bom" (Data.ByteString.pack [0xFE,0xFF])