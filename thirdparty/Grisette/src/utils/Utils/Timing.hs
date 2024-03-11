{-# LANGUAGE OverloadedStrings #-}

module Utils.Timing where

import Data.Text.Lazy.Builder
import Formatting
import Formatting.Internal
import System.Clock

fmt :: Integer -> Builder
fmt diff =
  bprint
    (fixed 6 % " s")
    ( fromIntegral diff
        / (fromIntegral (10 ^ (9 :: Integer) :: Integer) :: Double)
    )

timeSpecs :: Format r (TimeSpec -> TimeSpec -> r)
timeSpecs = Format (\g x y -> g (fmt0 x y))
  where
    fmt0 (TimeSpec s1 n1) (TimeSpec s2 n2) = fmt diff
      where
        diff :: Integer
        diff = a2 - a1
        a1 = (fromIntegral s1 * 10 ^ (9 :: Integer)) + fromIntegral n1
        a2 = (fromIntegral s2 * 10 ^ (9 :: Integer)) + fromIntegral n2

timeItAll :: String -> IO a -> IO a
timeItAll str x = do
  startMono <- getTime Monotonic
  startProcessCPU <- getTime ProcessCPUTime
  r <- x
  endMono <- getTime Monotonic
  endProcessCPU <- getTime ProcessCPUTime
  fprint (string % " -- Mono clock: " % timeSpecs % "\n") str startMono endMono
  fprint (string % " -- CPU clock: " % timeSpecs % "\n") str startProcessCPU endProcessCPU
  return r
