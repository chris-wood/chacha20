module ChaCha20 (
    chaCha20Encrypt
) where

import Data.Bits
import Data.Word
import Numeric (showHex, readHex, showIntAtBase)
import Data.ByteString.Internal (unpackBytes)
import Data.ByteString.Char8 (pack)
import Data.Char (intToDigit)

-- Helpers
toWord32 :: [Char] -> Word32
toWord32 string = fromIntegral (fst ((readHex string) !! 0))

printHex :: Word32 -> IO()
printHex x = putStrLn $ showHex x ""

flipWord :: Word32 -> Word32
flipWord x = (shiftL (x .&. 0xFF) 24) .|. (shiftL (x .&. 0xFF00) 8) .|. (shiftR (x .&. 0xFF0000) 8) .|. (shiftR (x .&. 0xFF000000) 24)

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
    let bprime = xor b cprime
    let bout = rotateL bprime 7
        in (a, bout, cprime, d)

fullQuarterRound :: (Word32, Word32, Word32, Word32) -> (Word32, Word32, Word32, Word32)
fullQuarterRound (a, b, c, d) = quarterRound4 (quarterRound3 (quarterRound2 (quarterRound1 (a, b, c, d) )))

testFullQuarterRound :: IO()
testFullQuarterRound = do
    let a = toWord32 "11111111"
    let b = toWord32 "01020304"
    let c = toWord32 "9b8d6f43"
    let d = toWord32 "01234567"
    print [a, b, c, d]
    let (a', b', c', d') = fullQuarterRound (a, b, c, d)
        in 
            print [a', b', c', d']

type ChaChaState = [Word32]

displayState :: ChaChaState -> IO()
displayState state = mapM_ printHex state

flipState :: ChaChaState -> ChaChaState
flipState state = map flipWord state

word32ToBytes :: Word32 -> [Word8]
word32ToBytes x = map fromIntegral [(shiftR (x .&. 0xFF000000) 24), (shiftR (x .&. 0xFF0000) 16), (shiftR (x .&. 0xFF00) 8), ((x .&. 0xFF))]

bytesToWord32 :: [Word8] -> Word32
bytesToWord32 = foldl accum 0
    where
        accum a o = (a `shiftL` 8) .|. fromIntegral o

merge :: [[Word8]] -> [Word8]
merge [] = []
merge ([]:xss) = merge xss
merge ((x:xs):y) = [x] ++ xs ++ (merge y)

stateToBytes :: ChaChaState -> [Word8] 
stateToBytes state = merge (map word32ToBytes state)

bytesToState :: [Word8] -> ChaChaState
bytesToState (x1:x2:x3:x4:xs) = (bytesToWord32 [x1,x2,x3,x4]) : (bytesToState xs)

replaceNthElement :: Int -> Word32 -> [Word32] -> [Word32]
replaceNthElement n v (x:xs)
    | n == 0 = v:xs
    | otherwise = x:(replaceNthElement (n - 1) v xs)

quarterRound :: ChaChaState -> (Int, Int, Int, Int) -> ChaChaState
quarterRound state (w, x, y, z) = do
    let (w', x', y', z') = fullQuarterRound (state!!w, state!!x, state!!y, state!!z)
        in replaceNthElement w w' (replaceNthElement x x' (replaceNthElement y y' (replaceNthElement z z' state)))

testState :: IO()
testState = do
    let state = [toWord32 "879531e0", toWord32 "c5ecf37d", toWord32 "516461b1", toWord32 "c9a62f8a",
                 toWord32 "44c20ef3", toWord32 "3390af7f", toWord32 "d9fc690b", toWord32 "2a5f714c",
                 toWord32 "53372767", toWord32 "b00a5631", toWord32 "974c541a", toWord32 "359e9963",
                 toWord32 "5c971061", toWord32 "3d631689", toWord32 "2098d9d6", toWord32 "91dbd320"]
    print state
    let newState = quarterRound state (2, 7, 8, 13)
        in displayState newState

chaCha20BlockRound :: ChaChaState -> ChaChaState
chaCha20BlockRound state = do
    let state1 = quarterRound state  (0, 4,  8, 12)
    let state2 = quarterRound state1 (1, 5,  9, 13)
    let state3 = quarterRound state2 (2, 6, 10, 14) 
    let state4 = quarterRound state3 (3, 7, 11, 15)
    let state5 = quarterRound state4 (0, 5, 10, 15)
    let state6 = quarterRound state5 (1, 6, 11, 12)
    let state7 = quarterRound state6 (2, 7,  8, 13)
    let state8 = quarterRound state7 (3, 4,  9, 14)
        in 
            state8

chaCha20BlockLoop :: ChaChaState -> Int -> ChaChaState
chaCha20BlockLoop state n
    | n == 0 = state
    | otherwise = chaCha20BlockLoop (chaCha20BlockRound state) (n - 1)
    
-- Main block function
chaCha20Block :: [Word32] -> [Word32] -> Word32 -> [Word32]
chaCha20Block key nonce count 
    | length key /= 8 = error "Invalid key length -- must be 256 bits"
    | length nonce /= 3 = error "Invalid nonce length -- must be 96 bits"
    | otherwise = do 
        let state = [toWord32 "61707865", toWord32 "3320646e", toWord32 "79622d32", toWord32 "6b206574",
                     flipWord (key !! 0), flipWord (key !! 1), flipWord (key !! 2), flipWord (key !! 3),
                     flipWord (key !! 4), flipWord (key !! 5), flipWord (key !! 6), flipWord (key !! 7),
                     count, flipWord (nonce !! 0), flipWord (nonce !! 1), flipWord (nonce !! 2)]
        
        let mixedState = chaCha20BlockLoop state 10
            in
                flipState (zipWith (+) state mixedState)

testChaCha20Block :: IO()
testChaCha20Block = do
    let key = [toWord32 "00010203", toWord32 "04050607", toWord32 "08090a0b", toWord32 "0c0d0e0f", 
               toWord32 "10111213", toWord32 "14151617", toWord32 "18191a1b", toWord32 "1c1d1e1f"]
    let nonce = [toWord32 "00000009", toWord32 "0000004a", toWord32 "00000000"]
    let counter = toWord32 "1"
    let state = chaCha20Block key nonce counter
        in
            displayState state

-- Main encrypt function
chaCha20Encrypt :: [Word8] -> [Word32] -> Word32 -> [Word32] -> [Word8] -> [Word8]
chaCha20Encrypt acc key counter nonce [] = []
chaCha20Encrypt acc key counter nonce block = do 
    let keyStream = chaCha20Block key nonce counter 
    let pad = stateToBytes keyStream
    let len = min 64 (length block)
    let maskedBlock = zipWith xor (take len pad) (take len block)
        in
            maskedBlock ++ chaCha20Encrypt maskedBlock key (counter + 1) nonce (drop len block)

stringToBytes :: String -> [Word8]
stringToBytes = unpackBytes . pack 

testChaCha20Encrypt :: IO()
testChaCha20Encrypt = do
    let key = [toWord32 "00010203", toWord32 "04050607", toWord32 "08090a0b", toWord32 "0c0d0e0f", 
               toWord32 "10111213", toWord32 "14151617", toWord32 "18191a1b", toWord32 "1c1d1e1f"]
    let nonce = [toWord32 "00000000", toWord32 "0000004a", toWord32 "00000000"]
    let counter = toWord32 "1"
    let plaintext = stringToBytes "Ladies and Gentlemen of the class of '99: If I could offer you only one tip for the future, sunscreen would be it."
    let ciphertext = chaCha20Encrypt [] key counter nonce plaintext
    let hexBytes = map (\x -> showHex x "") ciphertext
        in
            print hexBytes

