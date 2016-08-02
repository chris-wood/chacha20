import Data.Bits
import Data.Word
import Numeric (showHex, readHex, showIntAtBase)

quarterRound1 :: (Word32, Word32, Word32, Word32) -> (Word32, Word32, Word32, Word32)
quarterRound1 (a, b, c, d) = do
    let aprime = a + b
    let dprime = xor d aprime
    let dout = rotateL dprime 16
        in (aprime, b, c, dout)

quarterRound2 :: (Word32, Word32, Word32, Word32) -> (Word32, Word32, Word32, Word32)
quarterRound2 (a, b, c, d) = do
    let cprime = c + d
    let bprime = xor b cprime
    let bout = rotateL bprime 12
        in (a, bout, cprime, d)

quarterRound3 :: (Word32, Word32, Word32, Word32) -> (Word32, Word32, Word32, Word32)
quarterRound3 (a, b, c, d) = do
    let aprime = a + b
    let dprime = xor d aprime
    let dout = rotateL dprime 8
        in (aprime, b, c, dout)

quarterRound4 :: (Word32, Word32, Word32, Word32) -> (Word32, Word32, Word32, Word32)
quarterRound4 (a, b, c, d) = do
    let cprime = c + d
    let bprime = b + cprime
    let bout = rotateL b 7
        in (a, bout, cprime, d)

fullQuarterRound :: (Word32, Word32, Word32, Word32) -> (Word32, Word32, Word32, Word32)
fullQuarterRound (a, b, c, d) = quarterRound4 (quarterRound3 (quarterRound2 (quarterRound1 (a, b, c, d) )))

type ChaChaState = [Word32]

replaceNthElement :: Int -> Word32 -> [Word32] -> [Word32]
replaceNthElement n v (x:xs)
    | n == 0 = v:xs
    | otherwise = x:(replaceNthElement (n - 1) v xs)

quarterRound :: ChaChaState -> (Int, Int, Int, Int) -> ChaChaState
quarterRound state (x, y, w, z) = do
    let (w', x', y', z') = fullQuarterRound (state!!w, state!!x, state!!y, state!!z)
        in replaceNthElement w w' (replaceNthElement x x' (replaceNthElement y y' (replaceNthElement z z' state)))

-- Test cases
toWord32 :: [Char] -> Word32
toWord32 string = fromIntegral (fst ((readHex string) !! 0))

testState :: IO()
testState = do
    let state = [toWord32 "879531e0", toWord32 "c5ecf37d", toWord32 "516461b1", toWord32 "c9a62f8a",
                 toWord32 "44c20ef3", toWord32 "3390af7f", toWord32 "d9fc690b", toWord32 "2a5f714c",
                 toWord32 "53372767", toWord32 "b00a5631", toWord32 "974c541a", toWord32 "359e9963",
                 toWord32 "5c971061", toWord32 "3d631689", toWord32 "2098d9d6", toWord32 "91dbd320"]
    print state
    let newState = quarterRound state (2, 7, 8, 13)
        in print newState
    
