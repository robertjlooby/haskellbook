import Control.Monad
import Data.Char (toLower)
import System.Exit (exitSuccess)

main :: IO ()
main =
  forever $ do
    line1 <- filter (flip elem ['a' .. 'z']) <$> (fmap toLower) <$> getLine
    case (line1 == reverse line1) of
      True -> putStrLn "It's a palindrome!"
      False -> do
        putStrLn "Nope!"
        exitSuccess
