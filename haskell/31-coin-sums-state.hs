-- from https://andrew.neitsch.ca/publications/m496pres1.nb.pdf
import Prelude hiding (lookup)
import Data.Map
import Control.Monad.Trans.State.Lazy

type Table = Map (Int, Int) Int

change :: (Int, Int) -> State Table Int
change (n, k) | n < 0 || k < 1 = return 0
change (0, _) = return 1
change (n, k) = do
    table <- get
    let term1 = (n, (k - 1))
    let term2 = ((n - (coins !! (k - 1))), k)
    x <- case lookup term1 table of
        Just v -> return v
        Nothing -> do
                    newv <- change term1
                    modify (insert term1 newv)
                    return newv
    y <- case lookup term2 table of
        Just v -> return v
        Nothing -> do
                    newv <- change term2
                    modify (insert term2 newv)
                    return newv
    return $ x + y

coins = [1, 2, 5, 10, 20, 50, 100, 200]

main = putStrLn $ show $ evalState (change (200, length coins)) empty
