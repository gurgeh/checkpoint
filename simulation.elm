screenwidth = 800
screenheight = 1200

dropOne xs =
  if | xs == [] -> []
     | otherwise -> (tail xs):(map ((:) $ head xs) (dropOne $ tail xs))

getAlternatives points now =
  if points == [] then []
  else points:map (\pts -> pts ++ [now]) (dropOne points)

scoreAlternatives fitness points now =
  let alts = getAlternatives points now
  in
   zip (map (fitness now) alts) alts

expfit start now points =
  let --40
    logr = logBase 2 (now - start)
    logl = logBase 2 $ toFloat (length points)
    expo = logr / logl
    expDeviation (p,i) = (abs $ now - p - i ^ expo) ^ 2
  in
   sum $ map expDeviation (zip (reverse points) [0..length points])

generalMin alts =
  let genmin xz lowp lowl =
    if xz == [] then lowl else -- 50
      let
        x = head xz
        xs = tail xz
      in
        if (fst x) < lowp then genmin xs (fst x) (snd x)
        else genmin xs lowp lowl
  in
   genmin alts 4000000000 [9]

scoreStep fitness points now end =
  let next = generalMin $ scoreAlternatives fitness points now
  in
   if now >= end then [next]
   else next:(scoreStep fitness next (now + 1) end)

expstep nrcps steps =
  scoreStep (expfit 0) [1..nrcps] (nrcps + 1) (steps + nrcps)

dot y x =
  filled blue $ rect 1 1 (x*2 + 20, y*2 + 100)

dotline (y, points) =
  map (dot y) $ points


nrsteps = 300

component dropDown nrpoints =
  let graph =
     collage screenwidth screenheight
     $ concatMap dotline $ zip [0..nrsteps] (expstep nrpoints nrsteps)
  in graph `below` dropDown

choices = [2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 30]


(drop,choice) = Input.dropDown $ map (\c -> (show c, c)) choices

main = lift (component drop) choice

