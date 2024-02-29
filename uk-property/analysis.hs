#!/usr/bin/env runhaskell

import Control.DeepSeq (force)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import Data.Containers.ListUtils (nubOrd)
import Numeric.AD (grad,auto)
import System.Random (randomRs, mkStdGen)
import Text.Printf(printf)

type FVector = V.Vector Double


{- PARSE AND PREPARE -}

type NormRow = (Int,Int,Double)

data Row = Row { house :: String, month :: String,
                 pType :: String, isNew :: Bool,
                 duration :: String, price :: Double } deriving (Show)

toRows :: String -> [Row]
toRows str = map (f . words) $ lines str
  where f [a,b,c,d,e,f] = Row a (take 6 b) c (toBool d) e (read f :: Double)
        f _ = error "Invalid format."
        toBool x = if x=="Y" then True else False

toNorm :: [Row] -> (V.Vector String, V.Vector String, [NormRow])
toNorm rs = (V.fromList hs, V.fromList ds, prices)
  where (hs,ds) = ( (nubOrd $ map house rs), (nubOrd $ map month rs) )
        hsID    = M.fromList $ zip hs [0..]
        dsID    = M.fromList $ zip ds [0..]
        prices  = map (\r -> ((hsID M.! house r), (dsID M.! month r), price r)) rs


{- MODEL AND FIT -}

adaMax :: (FVector->FVector) -> FVector -> Int -> FVector -> FVector -> FVector
adaMax f' th t m u
  | (V.maximum $ V.map abs g) < 1e-4 = th'
  | otherwise = adaMax f' (force th') (t+1) m' u'
  where g   = f' th
        m'  = V.zipWith  (\m g->beta1 * m + (1-beta1) * g) m g
        u'  = V.zipWith  (\u g->max (beta2*u) (abs g)) u g
        th' = V.zipWith3 (\th m u->th-(alpha/(1-beta1^(t+1)))*m/u) th m' u'
        (alpha, beta1, beta2) = (0.002, 0.99, 0.999)

optimise :: [NormRow] -> (FVector, FVector)
optimise ps = (V.take m ws, V.map (\x->x*mxP) $ V.drop m ws)
  where
     ws     = adaMax (grad cost) start 0 zeros zeros
     cost v = sqrt . sum $ map (\(i,j,p)-> (auto p-(v V.! i)*(v V.! (m+j)))^2) ps'
     m      = 1 + (maximum $ map (\(i,_,_)->i) ps)
     n      = 1 + (maximum $ map (\(_,j,_)->j) ps)
     mxP    = maximum $ map (\(_,_,p)-> p) ps
     ps'    = map (\(i,j,p)->(i, j, p/mxP)) ps
     start  = V.fromList $ take (m+n) $ randomRs (0.0, 1.0) (mkStdGen 100)
     zeros  = V.fromList (replicate (length start) 0)


{- INVOKE AND FORMAT OUTPUT -}

main :: IO ()
main = do
  str <- readFile "example.tsv"
  let tId          = "538303"
      tId'         = length $ V.takeWhile (/=tId) hs
      (hs, ds, ps) = toNorm $ toRows str
      (m, n)       = (length hs, length ds)
      (wHs, wDs)   = optimise ps
      wTarget      = wHs V.! tId'
      interpol     = V.toList $ V.imap (\i x-> (ds V.! i, wTarget * x, 0)) wDs
      points       = map (\(i,j,x)->(ds V.! j, x, 1)) $ filter(\(i,_,_)->i==tId') ps
  mapM_ (\(d,p,f)->printf "%s01\t%.0f\t%d\n" d p (f :: Int)) (interpol ++ points)
