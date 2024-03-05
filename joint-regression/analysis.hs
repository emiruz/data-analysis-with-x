import System.Random (randomRs, mkStdGen, Random)

detAnneal :: (Integral p,Ord t1,Fractional t1)=>(t2->t1)->(p->t2->t2)->p->[t1]->t2->(t1,t2)
detAnneal cost pert' n ts_ start = anneal 1 start 1e10 ts_
  where anneal _ cur old [] = (old, cur)
        anneal m cur old (t:ts)
          | f < t = anneal (m+1) new cost' ts'
          | otherwise  = anneal (m+1) cur old ts'
          where new   = pert' m cur
                cost' = cost new
                ts'   = if mod m n == 0 then ts else (t:ts)
                f     = cost' / (old+1e-10)

transform :: (Ord a, Floating a) => a -> a -> a -> a -> a -> a -> a
transform x0 l1 l2 a b x = a * tra + b
  where tra = (1-sig) * (yeo l1) + (sig) * (yeo l2)
        sig = 1 / (1 + exp (-50 * (x - x0)))
        yeo l | l /= 0 && x >= 0 = ((x+1)**l - 1) / l
              | l == 0 && x >= 0 = log (x + 1)
              | l /= 2 && x <  0 = -((-x + 1)**(2-l)-1)/(2-l)
              | otherwise        = -log (-x + 1)

boxMuller :: (Random p, Floating p) => Int -> p -> p -> [p]
boxMuller seed mu sig = f $ randomRs (0.0, 1.0) (mkStdGen seed)
  where f (u1:u2:xs) = (mu + (z0 u1 u2) * sig) : f xs
        f []         = []
        z0 u1 u2     = sqrt (-2.0 * log(u1)) * cos(2.0 * pi * u2)

generate :: (Ord b, Random b, Floating b) => Int -> Int ->b->b->b->b->b->b-> [(b, b)]
generate seed n x0 l1 l2 a b sig = zip xs ys'
  where ys  = map (transform x0 l1 l2 a b) xs
        xs  = take n $ randomRs (0, 100) (mkStdGen seed)
        ys' = zipWith (+) ys (boxMuller seed 0 sig)

xyToFile :: (Show a1, Show a2) => FilePath -> [(a1, a2)] -> IO ()
xyToFile fn xy = do
  writeFile fn (unlines [show x ++ "\t" ++ show y | (x,y) <- xy])

perturb :: (Integral p, Num c, Random c) => Double->Double->p->[c]->Int->[c]->[c]
perturb start stop total ws i xs = zipWith (+) xs (zipWith (*) ys inc)
  where ys  = zipWith (*)  ws $ randomRs (-1,1) (mkStdGen i)
        inc = map (\x->if x<=p then 1 else 0) $ randomRs (0 :: Double, 1) (mkStdGen (10+i))
        p = stop - (stop - start) * (fromIntegral i) / (fromIntegral total)

invert :: (Floating t1, Ord t1, Enum t1, Random t1) => [t1] -> [t1] -> (t1, [t1])
invert xs ys = detAnneal cost pert' n ts start
  where n     = 100
        ts    = [1.05,1.04999..1]
        start = [0, 1, 1, 1, 1]
        pert' = perturb 0.2 2 (n * length ts) [1, 0.01, 0.01, 0.1, 1]
        cost [x0,l1,l2,a,b] = sum $ zipWith (\y' y->(y-y')^2)
          (map (\x->transform x0 l1 l2 a b x) xs) ys

main :: IO ()
main = do
  let xy = generate 1 1000 (50 :: Double) 0.7 1.4 2 100 50
  xyToFile "test.tsv" xy
  let ws@(cost, [x0,l1,l2,a,b]) = invert (map fst xy) (map snd xy)
      xy' = generate 1 1000 x0 l1 l2 a b 0
  xyToFile "fitted.tsv" xy'
  putStrLn (show ws)
