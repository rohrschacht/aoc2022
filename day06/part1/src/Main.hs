import System.IO
import Control.Monad

main :: IO ()
main = do
        handle <- openFile "./input.txt" ReadMode
        contents <- hGetContents handle
        print (startOfPacket contents 4)
        hClose handle

startOfPacket :: [Char] -> Int -> Int
startOfPacket [] y = y
startOfPacket (x1:x2:x3:x4:xs) y = if unique [x1, x2, x3, x4]
                                      then y
                                      else startOfPacket (x2:x3:x4:xs) (y + 1)

unique :: (Eq a) => [a] -> Bool
unique [] = True
unique (x:xs) = if elem x xs
                  then False
                  else unique xs
