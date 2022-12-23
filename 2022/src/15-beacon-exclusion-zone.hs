import           Data.List       (nub)
import           Text.Megaparsec (some)
import           Utils           (Parser, manhattan, parseFile, signed, symbol)

scanLine :: Integer
scanLine = 2000000

sensorBeaconParser :: Parser ((Integer, Integer), (Integer, Integer))
sensorBeaconParser = do
  xs <- symbol "Sensor at x=" *> signed
  ys <- symbol ", y=" *> signed
  xb <- symbol ": closest beacon is at x=" *> signed
  yb <- symbol ", y=" *> signed
  pure ((xs, ys), (xb, yb))

main :: IO ()
main = do
  sensorBeacon <- parseFile "data/15-puzzle-input" $ some sensorBeaconParser
  let sensors = map (\(sensor, beacon) -> (sensor, manhattan sensor beacon)) sensorBeacon
      (_, beacons) = unzip sensorBeacon
      beaconsOnLine = toInteger . length . nub $ filter ((== scanLine) . snd) beacons
      ranges =
        map (\((x, y), d) -> (x - d + abs (y - scanLine), x + d - abs (y - scanLine))) sensors
      p1 = maximum (map snd ranges) - minimum (map fst ranges) - beaconsOnLine + 1
  putStrLn $ "Part 1: " ++ show p1
  -- putStrLn $ "Part 2: " ++ show p2
