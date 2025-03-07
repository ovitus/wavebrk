module Main where

import Control.Monad (when)
import Data.List (intercalate)
import Options.Applicative
import System.Process (callCommand)
import Text.ParserCombinators.ReadP 
  ( char
  , choice
  , readP_to_S
  , readS_to_P
  , string
  )
import Text.Printf (printf)

data Wavebrk = Wavebrk
  { plot :: Bool
  , name :: String
  , time :: [Double]
  , wave :: Waveform
  , freq :: Double
  , ampMin :: Double
  , ampMax :: Double
  , phase :: Double
  }

data Waveform = RSaw | Saw | Sin | Sqr | Tri
  deriving Read

wavebrk :: Wavebrk -> [(Double, Double)] 
wavebrk (Wavebrk _ name time wave freq ampMin ampMax phase) =
  case wave of
    RSaw -> [(t, ampMax - (ampMax - ampMin) * frac t) | t <- time]
    Saw -> [(t, ampMin + (ampMax - ampMin) * frac t) | t <- time]
    Sin -> [(t, ampMidPt + amp * sin (2 * pi * freq * t + degToRad phase)) | t <- time]
    Sqr -> [(t, if isHigh t then ampMax else ampMin) | t <- time]
    Tri -> [(t, ampMin + (ampMax - ampMin) * tri t) | t <- time]
  where 
    amp = (ampMax - ampMin) / 2
    ampMidPt = ampMin + amp
    degToRad d = d * pi / 180
    frac t = (((freq * t + degToRad (phase/360)) `mod'` 1.0) + 1.0) `mod'` 1.0
    isHigh t = (((freq * t + degToRad (phase/360)) `mod'` 1.0) + 1.0) `mod'` 1.0 >= 0.5
    mod' x y = x - y * fromIntegral (floor (x/y))
    tri t = 1 - abs (1 - 2 * frac t)

wavebrkIO :: Wavebrk -> IO ()
wavebrkIO (Wavebrk plot name time wave freq ampMin ampMax phase) = do
  writeFile name . intercalate "\n" . map (\(t, x) -> printf "%.1f %.3f" t x) $
    wavebrk (Wavebrk plot name time wave freq ampMin ampMax phase)
  when plot $ callCommand $ "gnuplot -p -e \"plot '" <> name <> "' with lines\""

wavebrkParser :: Parser Wavebrk
wavebrkParser = Wavebrk
  <$> switch (short 'p' <> help "Gnuplot")
  <*> argument str (metavar "<file>" <> help "Filename (\"example.brk\")")
  <*> (listParser <$> argument str (metavar "<time>" <> help "Time list (seconds; \"[start,step..end]\")"))
  <*> argument auto (metavar "<wave>" <> help "Waveform (RSaw,Saw,Sin,Sqr,Tri)")
  <*> argument auto (metavar "<freq>" <> help "Frequency (hertz)")
  <*> argument auto (metavar "<amp min>" <> help "Amplitude minimum")
  <*> argument auto (metavar "<amp max>" <> help "Amplitude maximum")
  <*> argument auto (metavar "<phase>" <> help "Phase (degrees)")

main :: IO ()
main = execParser (info (helper <*> wavebrkParser) (fullDesc <> progDesc "wavebrk - Create breakpoint files with wave oscillation" <> header "Wavebrk 1.0")) >>= wavebrkIO

listParser :: String -> [Double]
listParser s = case readP_to_S listParser' (filter (/= ' ') s) of
  [(l, "")] -> l
  _ -> error "Error parsing timelist"
  where
    listParser' = choice
      [ do
          char '['
          start <- readS_to_P reads
          string ".."
          end <- readS_to_P reads
          char ']'
          return [start .. end]
      , do
          char '['
          start <- readS_to_P reads
          char ','
          step <- readS_to_P reads
          string ".."
          end <- readS_to_P reads
          char ']'
          return [start, step .. end]
      ]
