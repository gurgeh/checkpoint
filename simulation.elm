screenwidth = 800
screenheight = 1200
barwidth = 400
barheight = 3

minbarx = (screenwidth - barwidth) / 2
maxbarx = (screenwidth + barwidth) / 2


arrowsize = 10

bar = rect barwidth barheight (screenwidth / 2, 100)
arrow x = polygon [(0, 0),
                   (arrowsize / 2, arrowsize),
                   (0 - arrowsize / 2, arrowsize)]
                   (x, 100 + barheight / 2 + 2)

slider x = collage screenwidth screenheight
    [ filled blue bar
    , filled green (arrow x)] --20

slidecap = clamp minbarx maxbarx

addhead x xs = x:xs

dropOne xs =
  if | xs == [] -> []
     | otherwise -> (tail xs):(map (addhead $ head xs) (dropOne $ tail xs))

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
    expDeviation (p,i) = abs $ now - p - i ^ expo
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

--alt to line
--show many lines

-- main = lift (slider . slidecap . fst) (keepWhen Mouse.isDown (150, 0) Mouse.position)
-- main = plainText $ show $ scoreStep (expfit 0) [0..5] 6 100

nrsteps = 200

main = collage screenwidth screenheight
        $ concatMap dotline $ zip [0..nrsteps] (expstep 20 nrsteps)
--main = plainText $ show $ scoreAlternatives (expfit 1 0) [0..9] 10

    