import Data.Bits
import Data.Word
import Numeric (showHex, showIntAtBase)

quarterRound1 :: (Word32, Word32, Word32, Word32) -> (Word32, Word32, Word32, Word32)
quarterRound1 (a, b, c, d) = do
    let dprime = xor d (a + b)
    let dout = rotateL dprime 16
        in (a + b, b, c, dout)

chacha20QuarterRound :: (Word32, Word32, Word32, Word32) -> (Word32, Word32, Word32, Word32)
chacha20QuarterRound (a, b, c, d) = (0, 0, 0, 0)
