import System.Random
import Data.List

bingo:: IO ()
bingo = aux [1..90]
         where aux [] = return ()
               aux l = do x <- sorteio l
                       getChar
                       aux (delete x l)

sorteio::[a]->IO a
sorteio l = do x <- randomRIO (0,(length (l-1)))
            return ((!!) l x)

----------------------------------------------------------------------------------------------------------------------------------------------------------------
