import System.Environment

main = do
  args <- getArgs
  putStrLn $ createPerfScript args

createPerfScript :: [String] -> String
createPerfScript [width, height] =
  unlines $ getStrips (read width) (read height)

getStrips :: Float -> Float -> [String]
getStrips width height =
  let numberOfHoles = round $ (width - 2.54) / 2.54
      numberOfRows = round $ (height - 2.54) / 2.54
  in  getHoleCommands numberOfHoles numberOfRows

getHoleCommands :: Int -> Int -> [String]
getHoleCommands numHoles rows =
  let holesForRow = \y -> (foldr holeCommand [] (zip [1..numHoles] (repeat y)))
  in "GRID MM;":"CHANGE DRILL 0.6":foldr (\row acc -> holesForRow row ++ acc) [] [1..rows]

holeCommand :: (Int,Int) -> [String] -> [String]
holeCommand (x,y) acc =
  ("VIA 1.0 Square (" ++ (show ( 2.54 * (fromIntegral x))) ++ " " ++ (show ( 2.54 *(fromIntegral y))) ++ ")"):acc
