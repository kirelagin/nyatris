module Nyatris.Random (
    chooseRIO,
) where

import System.Random


-- Returns random item from a list
chooseRIO :: [a] -> IO a
chooseRIO list = fmap (list !!) $ randomRIO (0, length list - 1)
