import System.IO
import Control.Monad

main :: IO ()
main = do
        handle <- openFile "./input.txt" ReadMode
        contents <- hGetContents handle
        print (startOfMessage contents 14)
        hClose handle

startOfMessage :: [Char] -> Int -> Int
startOfMessage [] y = y
startOfMessage ls y = if unique (take 14 ls)
                                      then y
                                      else startOfMessage (tail ls) (y + 1)

unique :: (Eq a) => [a] -> Bool
unique [] = True
unique (x:xs) = if elem x xs
                  then False
                  else unique xs
