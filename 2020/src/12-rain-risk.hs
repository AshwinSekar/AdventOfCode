import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Tuple.Extra
import Utils

nav (theta, pos) ("F", s) =
  ( theta,
    theta
      & sin . d2r &&& cos . d2r
      & both (* s)
      & psum pos
  )
nav (theta, pos) ("R", s) = (theta - s, pos)
nav (theta, pos) ("L", s) = (theta + s, pos)
nav (theta, pos) (dir -> d, s) = (theta, psum pos $ both (* s) d)

navWP (wp, pos) ("F", s) = (wp, psum pos $ both (* s) wp)
navWP (wp, pos) ("R", s) = (rot (-s) wp, pos)
navWP (wp, pos) ("L", s) = (rot s wp, pos)
navWP (wp, pos) (dir -> d, s) = (psum wp $ both (* s) d, pos)

rot (d2r -> theta) (y, x) = (x * sin theta + y * cos theta, x * cos theta - y * sin theta)

main :: IO ()
main = do
  (p1, p2) <-
    getFile "data/12-puzzle-input"
      <&> map (second read . splitAt 1)
      <&> foldl nav (0, (0, 0)) &&& foldl navWP ((1, 10), (0, 0))
      <&> snd *** snd
      <&> both (round . uncurry (+) . both abs)
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2
