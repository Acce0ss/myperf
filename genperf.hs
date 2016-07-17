import System.Environment

main = do
  args <- getArgs
  putStrLn $ createPerfScript args

data Point = Point Float Float

instance Show Point where
  show (Point x y) = "(" ++ (show x) ++ " " ++ (show y) ++ ")"

-- Units in mm
spacing :: Float
spacing = 2.54

padWidth :: Float
padWidth = spacing / 1.7

drillDiam :: Float
drillDiam = 1

distFromHole :: Float
distFromHole = spacing / 3.333

bottomAndLeftMargin :: Float
bottomAndLeftMargin = spacing / 2

createPerfScript :: [String] -> String
createPerfScript [width, height] =
  let w = read width
      h = read height
      cols = round $ (w - spacing*1.5) / spacing
      rows = round $ (h - spacing*1.5) / spacing
      gridUnitSetting = ["GRID MM;"]
      strips = getStrips cols rows
      labels = getLabels cols rows
      outline = getBoardOutline w h
      signature = getSignature (Point (-spacing/3) (spacing/5))
  in unlines $ gridUnitSetting ++ strips ++ labels ++
     outline ++ signature

getSignature :: Point -> [String]
getSignature (Point x y) =
  let layerSetting = "LAYER 25;"
      fontSetting = "CHANGE FONT Vector"
      alignSetting = "CHANGE ALIGN Left"
      sizeSetting = "CHANGE SIZE 0.9"
      settings = layerSetting:fontSetting:alignSetting:sizeSetting:[]
      pos = Point x y
      cpos = Point x (y+1)
      vpos = Point x (y-1)
  in settings ++ [labelCommand pos "Acce", labelCommand cpos "'(C)'",
                  labelCommand vpos "'v0.1'"]

getLabels :: Int -> Int -> [String]
getLabels cols rows =
  let layerSetting = "LAYER 25;"
      fontSetting = "CHANGE FONT Vector"
      alignSetting = "CHANGE ALIGN Center"
      sizeSetting = "CHANGE SIZE 0.9"
      settings = layerSetting:fontSetting:alignSetting:sizeSetting:[]
      horzLabels = horizontalLabels cols rows
      vertLabels = verticalLabels cols rows
  in settings ++ horzLabels ++ vertLabels

horizontalLabels :: Int -> Int -> [String]
horizontalLabels cols rows =
  let letterLabels = map (:[]) ['A'..'Z'] ++ [[a,x] | a <- ['A'..'Z'], x <- ['A'..'Z']]
      y = ((fromIntegral rows) + 0.8) * spacing
      positions = foldr (\x acc -> (Point (x*spacing) y):acc) [] (map fromIntegral [1..cols])
  in foldr (\x acc -> (labelCommand (fst x) (snd x)):acc) [] (zip positions letterLabels)

verticalLabels :: Int -> Int -> [String]
verticalLabels cols rows =
  let numberLabels = map show (reverse [1..rows])
      x = ((fromIntegral cols) + 0.75) * spacing
      positions = foldr (\y acc -> (Point x (y*spacing)):acc) [] (map fromIntegral [1..rows])
  in foldr (\x acc -> (labelCommand (fst x) (snd x)):acc) [] (zip positions numberLabels)

labelCommand :: Point -> String -> String
labelCommand (Point x y) label =
  let pos = Point (x+bottomAndLeftMargin) (y+bottomAndLeftMargin)
  in ("TEXT " ++ label ++ " " ++ show pos)

getStrips :: Int -> Int -> [String]
getStrips cols rows =
  let holeCommands = getHoleCommands cols rows
      stripCommands = getStripCommands cols rows
  in holeCommands ++ stripCommands

getBoardOutline :: Float -> Float -> [String]
getBoardOutline w h =
  let layerSetting = "LAYER 20;"
      leftSide = "WIRE 0 " ++ (show (Point 0 0)) ++ " " ++ (show (Point 0 h))
      topSide = "WIRE 0 " ++ (show (Point 0 h)) ++ " " ++ (show (Point w h))
      rightSide = "WIRE 0 " ++ (show (Point w h)) ++ " " ++ (show (Point w 0))
      bottomSide = "WIRE 0 " ++ (show (Point w 0)) ++ " " ++ (show (Point 0 0))
  in layerSetting:leftSide:topSide:rightSide:bottomSide:[]

getHoleCommands :: Int -> Int -> [String]
getHoleCommands cols rows =
  let drillSetting = "CHANGE DRILL " ++ (show drillDiam)
      matrixPositions = [Point x y | x<-(map ((*) spacing . fromIntegral) [1..rows]),
                         y <- (map ((*) spacing . fromIntegral) [1..cols])]
      holeMatrix = foldr (\x acc -> (holeCommand x):acc) [] matrixPositions
      bottomSideHoles = foldr bottomSideHoleCommand [] [1..cols]
      leftSideHoles = foldr leftSideHoleCommand [] [1..rows]
  in drillSetting:holeMatrix ++ bottomSideHoles ++ leftSideHoles

distFromSide :: Float
distFromSide = spacing / 5

bottomSideHoleCommand :: Int -> [String] -> [String]
bottomSideHoleCommand col acc =
  let position = Point ( spacing * (fromIntegral col) + spacing / 2) (distFromSide)
      newHole = holeCommand position
  in newHole:acc

leftSideHoleCommand :: Int -> [String] -> [String]
leftSideHoleCommand row acc =
  let position = Point (distFromSide) ( spacing * (fromIntegral row) + spacing / 2)
      newHole = holeCommand position
  in newHole:acc

holeCommand :: Point -> String
holeCommand (Point x y) =
  let newHoleLocation = Point (bottomAndLeftMargin + x) (bottomAndLeftMargin + y)
  in "VIA " ++ (show padWidth) ++ " Square " ++ show (newHoleLocation)

getStripCommands :: Int -> Int -> [String]
getStripCommands cols rows =
  (getTopHorizontalStripCommands cols rows) ++
  (getBottomVerticalStripCommands cols rows)

getTopHorizontalStripCommands :: Int -> Int -> [String]
getTopHorizontalStripCommands cols rows =
  let layerSetting = "Layer 1;"
      width = repeat $ (fromIntegral cols)*spacing + spacing
      strips = foldr horizontalStripCommand [] (zip width [1..rows])
      stopLayerSetting = "Layer 29;"
      stopMaskStrips = foldr horizontalStripStopmask [] [(i,j) | i <- [1..rows], j <- [1..cols]]
  in layerSetting:strips ++ stopLayerSetting:stopMaskStrips

horizontalStripCommand :: (Float, Int) -> [String] -> [String]
horizontalStripCommand (l, row) acc =
  let topLeft = Point ( l-spacing/2.5 ) (spacing *(fromIntegral row) + spacing/10 + distFromHole)
      bottomRight = Point ( spacing/2.5 ) (spacing *(fromIntegral row) + spacing/3.333 + distFromHole)
      stripForRow = rectCommand topLeft bottomRight
  in stripForRow:acc

horizontalStripStopmask :: (Int, Int) -> [String] -> [String]
horizontalStripStopmask (row, col) acc =
  let baseX = (fromIntegral col) * spacing
      baseY = (fromIntegral row) * spacing
      topLeft = Point ( baseX-padWidth/2 - spacing/24) ( baseY + spacing/3.333 + distFromHole)
      bottomRight = Point ( baseX+padWidth/2 + spacing/24) (baseY + spacing/10 + padWidth/3)
      stripForSpot = rectCommand topLeft bottomRight
  in stripForSpot:acc      
          
getBottomVerticalStripCommands :: Int -> Int -> [String]
getBottomVerticalStripCommands cols rows =
  let layerSetting = "Layer 16;"
      width = repeat $ (fromIntegral cols)*spacing + spacing
      strips = foldr verticalStripCommand [] (zip width [1..cols])
      stopLayerSetting = "Layer 30;"
      stopMaskStrips = foldr verticalStripStopmask [] [(i,j) | i <- [1..rows], j <- [1..cols]]
  in layerSetting:strips ++ stopLayerSetting:stopMaskStrips
  
verticalStripCommand :: (Float, Int) -> [String] -> [String]
verticalStripCommand (l, col) acc =
  let topLeft = Point (spacing *(fromIntegral col) + spacing/10 + distFromHole) ( l-spacing/2.5)
      bottomRight = Point (spacing *(fromIntegral col) + spacing/3.333 + distFromHole) ( spacing/2.5 )
      stripForRow = rectCommand topLeft bottomRight
  in stripForRow:acc

verticalStripStopmask :: (Int, Int) -> [String] -> [String]
verticalStripStopmask (row, col) acc =
  let baseX = (fromIntegral col) * spacing
      baseY = (fromIntegral row) * spacing
      topLeft = Point ( baseX+spacing/3.333 + distFromHole) (baseY + padWidth/2 + spacing/24)
      bottomRight = Point ( baseX+spacing/10+padWidth/3) (baseY - padWidth/2 - spacing/24)
      stripForSpot = rectCommand topLeft bottomRight
  in stripForSpot:acc      

rectCommand :: Point -> Point -> String
rectCommand (Point x1 y1) (Point x2 y2) =
  let topLeftWithMargin = Point (x1+bottomAndLeftMargin) (y1+bottomAndLeftMargin)
      bottomRightWithMargin = Point (x2+bottomAndLeftMargin) (y2+bottomAndLeftMargin)
  in "RECT R0 " ++ (show topLeftWithMargin) ++ " " ++ (show bottomRightWithMargin)
