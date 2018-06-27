import Chapter11 (cipher)
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Enter the key: "
  key <- getLine
  putStr "Enter the message: "
  message <- getLine
  putStrLn $ "The ciphered message: " ++ cipher key message
