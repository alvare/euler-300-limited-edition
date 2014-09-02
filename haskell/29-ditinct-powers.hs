import Debug.Trace
import qualified Data.Set as S
import qualified Data.Vector as V

table :: (Int, Int) -> (Int, Int) -> V.Vector (V.Vector Integer)
table (a1, a2) (b1, b2) = V.generate (b2 - b1 + 1) (\b -> V.generate (a2 - a1 + 1) (\a -> exp (a + a1) (b + b1)))
    where exp x y = (fromIntegral x) ^ (fromIntegral y)

vectToSet :: V.Vector (V.Vector Integer) -> S.Set Integer
vectToSet = V.foldl (\s v -> V.foldl (\s' e -> e `S.insert` s') s v) S.empty

main = putStrLn . show . S.size . vectToSet $ table (2, 100) (2, 100)
