module Library where

import Control.Concurrent (threadDelay)
import Data.List.Split (chunksOf)
import GHC.Internal.Word
import System.MIDI
import System.MIDI.Utility

-- Grid has a width, height, and then its entries
data Grid = Grid Int Int [[Colour]] deriving (Show)

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave xs [] = xs
interleave (x : xt) (y : yt) = x : y : interleave xt yt

readGridValue :: Grid -> Int -> Int -> Colour
readGridValue (Grid w h entries) x y =
    (entries !! (x'' - 1)) !! (y'' - 1)
  where
    (x', y') = (x `mod` w, y `mod` h)
    (x'', y'') = (if x' == 0 then w else x', if y' == 0 then h else y')

blankRow :: Grid
blankRow = Grid 8 1 [replicate 8 blank]

blankCol :: Grid
blankCol = Grid 1 8 $ replicate 8 [blank]

sumNeighbours :: Grid -> Int -> Int -> Int
sumNeighbours grid x y =
    length $
        filter
            (/= blank)
            [ readGridValue grid (x + 1) (y + 1)
            , readGridValue grid (x + 1) (y - 1)
            , readGridValue grid (x - 1) (y + 1)
            , readGridValue grid (x - 1) (y - 1)
            , readGridValue grid x (y + 1)
            , readGridValue grid (x + 1) y
            , readGridValue grid x (y - 1)
            , readGridValue grid (x - 1) y
            ]

updateGrid :: Grid -> Grid
updateGrid grid@(Grid w h entries) = Grid w h $ chunksOf h $ do
    x <- [1 .. w]
    y <- [1 .. h]
    let alive = readGridValue grid x y
    let count = sumNeighbours grid x y
    return $ case alive == blank of
        True
            | count == 3 -> white
            | otherwise -> blank
        False
            | count < 2 -> blank
            | count > 3 -> blank
            | otherwise -> white

progMode :: [Word8]
progMode = [0x00, 0x20, 0x29, 0x02, 0x0E, 0x00, 0x11, 0x00, 0x00]

liveMode :: [Word8]
liveMode = [0x00, 0x20, 0x29, 0x02, 0x0E, 0x00, 0x04, 0x00, 0x00]

toProgMode :: Connection -> IO ()
toProgMode c = do
    sendSysEx c progMode
    threadDelay 50000

toLiveMode :: Connection -> IO ()
toLiveMode c = do
    sendSysEx c liveMode
    threadDelay 50000

type Colour = Int
red :: Colour
red = 5
blue :: Colour
blue = 45
green :: Colour
green = 21
blank :: Colour
blank = 0
white :: Colour
white = 3

-- A pad has coordinates and it should know which device it belongs to
data Pad = Pad Int Int Connection

padToIndex :: Pad -> Int
padToIndex (Pad x y _) = 10 * x + y

updatePad :: Pad -> Colour -> IO ()
updatePad p@(Pad _ _ c) colour = do
    let command = NoteOn (padToIndex p) colour
    let msg = MidiMessage 1 command
    send c msg

fillColour :: Connection -> Colour -> IO ()
fillColour connection colour = sequence_ $ do
    x <- [0 .. 9]
    y <- [0 .. 9]
    return $ updatePad (Pad x y connection) colour

toColour :: Colour -> Grid -> Grid
toColour colour (Grid w h entries) = Grid w h (map (map (\c -> if c == blank then blank else colour)) entries)

clearPads :: Connection -> IO ()
clearPads connection = fillColour connection blank

strobePads :: Connection -> Colour -> IO ()
strobePads connection colour = sequence_ $ concat $ do
    let pads = map (\(x, y) -> Pad x y connection) $ [(x, y) | x <- [1 .. 8], y <- [1 .. 8]]
    pad <- pads
    return [updatePad pad colour, threadDelay 20000]

-- Draws the first 8x8 portion of a grid
drawGrid :: Connection -> Grid -> IO ()
drawGrid connection grid = drawGridAtPos connection grid 1 1

drawGridAtPos :: Connection -> Grid -> Int -> Int -> IO ()
drawGridAtPos connection (Grid _ _ entries) x y =
    sequence_ $
        do
            x' <- [1 .. 8]
            y' <- [1 .. 8]
            let entry = (entries !! (y + y' - 2)) !! (x + x' - 2)
            return $ updatePad (Pad x' y' connection) entry

-- Scrolls across a grid that is wider than 8 pads
scrollGridHorizontal :: Connection -> Grid -> IO ()
scrollGridHorizontal connection grid@(Grid w _ _) = sequence_ $ concat $ do
    x <- [1 .. (w - 7)]
    return [drawGridAtPos connection grid x 1, threadDelay 200000]

scrollGridVertical :: Connection -> Grid -> IO ()
scrollGridVertical connection grid@(Grid _ h _) = sequence_ $ concat $ do
    y <- [1 .. (h - 7)]
    return [drawGridAtPos connection grid 1 y, threadDelay 100000]

joinGridsVertical :: Grid -> Grid -> Grid
joinGridsVertical (Grid w1 h1 entries1) (Grid w2 h2 entries2) = case () of
    _
        | w1 /= w2 -> error "Widths of added grids do not agree"
        | otherwise -> Grid w1 (h1 + h2) (entries1 ++ entries2)
