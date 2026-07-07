module Main (main) where

import Alphabet
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Monad (void)
import Library
import System.MIDI
import System.MIDI.Utility

grid :: Grid
grid =
    Grid
        8
        8
        [ [blank, blank, blank, blank, blank, blank, white, blank]
        , [blank, blank, blank, blank, blank, blank, blank, blank]
        , [blank, white, white, white, blank, blank, blank, blank]
        , [blank, blank, blank, white, blank, blank, blank, blank]
        , [blank, blank, white, blank, blank, blank, blank, blank]
        , [blank, blank, blank, blank, blank, blank, blank, blank]
        , [blank, blank, blank, blank, blank, white, white, white]
        , [blank, blank, blank, blank, blank, blank, blank, white]
        ]

grids :: [Grid]
grids = iterate updateGrid grid

mainLoop :: Connection -> IO ()
mainLoop connection = do
    -- drawWord connection blue "Touch my beloveds thought while her worlds affluence crumbles at my feet"

    -- n <- [1 .. 26]
    -- -- let letter = getLetter n
    -- return [drawGrid connection white (pixelAlphabet !! (n - 1)), void getLine]

    -- sequence_ $
    --     interleave (map (drawGrid connection white) grids) (repeat (threadDelay 50000))

    strobePads connection red
    strobePads connection blue
    strobePads connection green
    mainLoop connection

main :: IO ()
main = do
    dst <- selectOutputDevice "" (Just "LPProMK3 MIDI")

    connection <- openDestination dst
    putStrLn "Connected"

    toProgMode connection
    putStrLn "Switched to programmer mode"

    clearPads connection

    -- drawGrid connection grid
    -- let grid' = updateGrid grid
    -- drawGrid connection grid'

    -- _ <- mainLoop connection
    withAsync (mainLoop connection) $ \_ -> do
        putStrLn "Looping. Press enter to stop."
        _ <- getLine
        putStrLn "Done"

    toLiveMode connection
    putStrLn "Switched back to live mode"

    putStrLn "Connection closed"
    close connection
