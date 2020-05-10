module PatternMatching where

import Prelude

reply :: Int -> String
reply guess
  | guess < 41 = "Too low"
  | guess > 43 = "Too high"
  | guess == 41 = "So close"
  | guess == 43 = "So close"
  | otherwise = "Correct"
