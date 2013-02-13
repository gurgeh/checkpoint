import Data.List

dropOne :: [a] -> [[a]]
dropOne [] = []
dropOne (x:xs) =
  xs:map (x:) (dropOne xs)

getAlternatives :: [a] -> a -> [[a]]  
getAlternatives [] _ = []
getAlternatives points now =
  points:map (\pts -> pts ++ [now]) (dropOne points)

scoreAlternatives :: Num a => (a -> [a] -> a) -> [a] -> a -> [(a, [a])]
scoreAlternatives fitness points now =
  let alts = getAlternatives points now
  in
   zip (map (fitness now) alts) alts

expfit :: Floating a => a -> a -> a -> [a] -> a
expfit scale start now points =
  let 
    expo = log (now - start) / (log $ fromIntegral $ length points)
    expDeviation (p,i) = abs $ now - p - scale * (fromIntegral i) ** expo
  in
   sum $ map expDeviation (zip (reverse points) [0..length points])

ageAndSpread :: Num a => (a -> a -> a) -> (a -> [a] -> a) -> a -> [a] -> a
ageAndSpread agefit spreadfit now points =
  sum (map (agefit now) points) + spreadfit now points

noage :: Num c => a -> b -> c
noage _ _ = 0

linage :: Num a => a -> a -> a -> a
linage scale now point =
  scale * (now - point)

divage :: Fractional a => a -> a -> a -> a -> a
divage scale offset now point =
  negate $ scale / (now - point + offset)
  
expage :: Floating a => a -> a -> a -> a -> a -> a
expage expo scale offset _ point =
  negate $ scale * (point - offset) ** expo
  
spread :: Floating a => a -> a -> a -> a -> [a] -> a
spread expo scale start end points =
  let target = (end - start) / (fromIntegral $ length points + 1)
      fit [] = 0
      fit (_:[]) = 0
      fit (p1:p2:pts) =
        abs (scale * (p2 - p1 - target) ** expo) + fit (p2:pts)
  in
   fit $ start:points ++ [end]
   
scoreStep :: (Ord a, Num a) => (a -> [a] -> a) -> [a] -> a -> [[a]]
scoreStep fitness points now =
  let next = snd . head . sort $ scoreAlternatives fitness points now
  in
   next : scoreStep fitness next (now + 1)