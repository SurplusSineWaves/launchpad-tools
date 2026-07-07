module Alphabet where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.Char
import Library
import System.MIDI

t, f :: Colour
t = white
f = blank

getLetter :: Int -> Char
getLetter n = chr (n + 64)
letterPos :: Char -> Int
letterPos ' ' = 0
letterPos c = ord (toUpper c) - 64

-- Draws a word as a disjoint sequence of frames
drawWord :: Connection -> Colour -> String -> IO ()
drawWord connection colour word =
    sequence_ $ interleave (replicate (1 + length word) $ void (threadDelay 200000)) $ map (\n -> drawGrid connection (toColour colour (pixelAlphabet !! n))) $ map letterPos word

wordToGridVertical :: Colour -> String -> Grid
wordToGridVertical colour word =
    foldl1 joinGridsVertical $
        interleave
            (map (\c -> toColour colour (pixelAlphabet !! letterPos c)) word)
            (replicate (-1 + length word) blankRow)

-- return [drawGrid connection white (pixelAlphabet !! (n - 1)), void getLine]

-- An association list mapping uppercase characters to their 8x8 pixel representations
pixelAlphabet :: [Grid]
pixelAlphabet =
    [ Grid
        8
        8
        [ [f, f, f, f, f, f, f, f]
        , [f, f, f, f, f, f, f, f]
        , [f, f, f, f, f, f, f, f]
        , [f, f, f, f, f, f, f, f]
        , [f, f, f, f, f, f, f, f]
        , [f, f, f, f, f, f, f, f]
        , [f, f, f, f, f, f, f, f]
        , [f, f, f, f, f, f, f, f]
        ]
    , Grid
        8
        8
        [ [f, f, t, t, t, t, f, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, t, t, t, t, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, f, f, f, f, f, f, f]
        ]
    , Grid
        8
        8
        [ [f, t, t, t, t, t, f, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, t, t, t, t, f, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, t, t, t, t, f, f]
        , [f, f, f, f, f, f, f, f]
        ]
    , Grid
        8
        8
        [ [f, f, t, t, t, t, f, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, f, f]
        , [f, t, f, f, f, f, f, f]
        , [f, t, f, f, f, f, f, f]
        , [f, t, f, f, f, f, t, f]
        , [f, f, t, t, t, t, f, f]
        , [f, f, f, f, f, f, f, f]
        ]
    , Grid
        8
        8
        [ [f, t, t, t, t, f, f, f]
        , [f, t, f, f, f, t, f, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, t, f, f]
        , [f, t, t, t, t, f, f, f]
        , [f, f, f, f, f, f, f, f]
        ]
    , Grid
        8
        8
        [ [f, t, t, t, t, t, t, f]
        , [f, t, f, f, f, f, f, f]
        , [f, t, f, f, f, f, f, f]
        , [f, t, t, t, t, t, f, f]
        , [f, t, f, f, f, f, f, f]
        , [f, t, f, f, f, f, f, f]
        , [f, t, t, t, t, t, t, f]
        , [f, f, f, f, f, f, f, f]
        ]
    , Grid
        8
        8
        [ [f, t, t, t, t, t, t, f]
        , [f, t, f, f, f, f, f, f]
        , [f, t, f, f, f, f, f, f]
        , [f, t, t, t, t, t, f, f]
        , [f, t, f, f, f, f, f, f]
        , [f, t, f, f, f, f, f, f]
        , [f, t, f, f, f, f, f, f]
        , [f, f, f, f, f, f, f, f]
        ]
    , Grid
        8
        8
        [ [f, f, t, t, t, t, f, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, f, f]
        , [f, t, f, f, t, t, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, f, t, t, t, t, f, f]
        , [f, f, f, f, f, f, f, f]
        ]
    , Grid
        8
        8
        [ [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, t, t, t, t, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, f, f, f, f, f, f, f]
        ]
    , Grid
        8
        8
        [ [f, f, t, t, t, t, f, f]
        , [f, f, f, f, t, f, f, f]
        , [f, f, f, f, t, f, f, f]
        , [f, f, f, f, t, f, f, f]
        , [f, f, f, f, t, f, f, f]
        , [f, f, f, f, t, f, f, f]
        , [f, f, t, t, t, t, f, f]
        , [f, f, f, f, f, f, f, f]
        ]
    , Grid
        8
        8
        [ [f, f, f, t, t, t, t, f]
        , [f, f, f, f, f, t, f, f]
        , [f, f, f, f, f, t, f, f]
        , [f, f, f, f, f, t, f, f]
        , [f, f, f, f, f, t, f, f]
        , [f, t, f, f, f, t, f, f]
        , [f, f, t, t, t, f, f, f]
        , [f, f, f, f, f, f, f, f]
        ]
    , Grid
        8
        8
        [ [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, t, f, f]
        , [f, t, f, f, t, f, f, f]
        , [f, t, t, t, f, f, f, f]
        , [f, t, f, f, t, f, f, f]
        , [f, t, f, f, f, t, f, f]
        , [f, t, f, f, f, f, t, f]
        , [f, f, f, f, f, f, f, f]
        ]
    , Grid
        8
        8
        [ [f, t, f, f, f, f, f, f]
        , [f, t, f, f, f, f, f, f]
        , [f, t, f, f, f, f, f, f]
        , [f, t, f, f, f, f, f, f]
        , [f, t, f, f, f, f, f, f]
        , [f, t, f, f, f, f, f, f]
        , [f, t, t, t, t, t, t, f]
        , [f, f, f, f, f, f, f, f]
        ]
    , Grid
        8
        8
        [ [f, t, f, f, f, f, t, f]
        , [f, t, t, f, f, t, t, f]
        , [f, t, f, t, t, f, t, f]
        , [f, t, f, t, t, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, f, f, f, f, f, f, f]
        ]
    , Grid
        8
        8
        [ [f, t, f, f, f, f, t, f]
        , [f, t, t, f, f, f, t, f]
        , [f, t, f, t, f, f, t, f]
        , [f, t, f, f, t, f, t, f]
        , [f, t, f, f, f, t, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, f, f, f, f, f, f, f]
        ]
    , Grid
        8
        8
        [ [f, f, t, t, t, t, f, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, f, t, t, t, t, f, f]
        , [f, f, f, f, f, f, f, f]
        ]
    , Grid
        8
        8
        [ [f, t, t, t, t, t, f, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, t, t, t, t, f, f]
        , [f, t, f, f, f, f, f, f]
        , [f, t, f, f, f, f, f, f]
        , [f, t, f, f, f, f, f, f]
        , [f, f, f, f, f, f, f, f]
        ]
    , Grid
        8
        8
        [ [f, f, t, t, t, t, f, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, t, f, t, f]
        , [f, t, f, f, f, t, f, f]
        , [f, f, t, t, t, t, t, f]
        , [f, f, f, f, f, f, f, f]
        ]
    , Grid
        8
        8
        [ [f, t, t, t, t, t, f, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, t, t, t, t, f, f]
        , [f, t, f, t, f, f, f, f]
        , [f, t, f, f, t, f, f, f]
        , [f, t, f, f, f, t, f, f]
        , [f, f, f, f, f, f, f, f]
        ]
    , Grid
        8
        8
        [ [f, f, t, t, t, t, f, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, f, f]
        , [f, f, t, t, t, t, f, f]
        , [f, f, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, f, t, t, t, t, f, f]
        , [f, f, f, f, f, f, f, f]
        ]
    , Grid
        8
        8
        [ [f, t, t, t, t, t, t, f]
        , [f, f, f, t, t, f, f, f]
        , [f, f, f, t, t, f, f, f]
        , [f, f, f, t, t, f, f, f]
        , [f, f, f, t, t, f, f, f]
        , [f, f, f, t, t, f, f, f]
        , [f, f, f, t, t, f, f, f]
        , [f, f, f, f, f, f, f, f]
        ]
    , Grid
        8
        8
        [ [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, f, t, t, t, t, f, f]
        , [f, f, f, f, f, f, f, f]
        ]
    , Grid
        8
        8
        [ [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, f, t, f, f, t, f, f]
        , [f, f, t, f, f, t, f, f]
        , [f, f, f, t, t, f, f, f]
        , [f, f, f, f, f, f, f, f]
        ]
    , Grid
        8
        8
        [ [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, t, t, f, t, f]
        , [f, t, t, f, f, t, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, f, f, f, f, f, f, f]
        ]
    , Grid
        8
        8
        [ [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, f, t, f, f, t, f, f]
        , [f, f, f, t, t, f, f, f]
        , [f, f, t, f, f, t, f, f]
        , [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, f, f, f, f, f, f, f]
        ]
    , Grid
        8
        8
        [ [f, t, f, f, f, f, t, f]
        , [f, t, f, f, f, f, t, f]
        , [f, f, t, f, f, t, f, f]
        , [f, f, f, t, t, f, f, f]
        , [f, f, f, t, t, f, f, f]
        , [f, f, f, t, t, f, f, f]
        , [f, f, f, t, t, f, f, f]
        , [f, f, f, f, f, f, f, f]
        ]
    , Grid
        8
        8
        [ [f, t, t, t, t, t, t, f]
        , [f, f, f, f, f, t, f, f]
        , [f, f, f, f, t, f, f, f]
        , [f, f, f, t, f, f, f, f]
        , [f, f, t, f, f, f, f, f]
        , [f, t, f, f, f, f, f, f]
        , [f, t, t, t, t, t, t, f]
        , [f, f, f, f, f, f, f, f]
        ]
    ]
