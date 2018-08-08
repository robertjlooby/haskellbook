module Chapter24 where

import Data.Maybe (catMaybes)
import Test.Hspec
import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

stringParser :: Parser String
stringParser = choice [string "123", string "12", string "1"]

stringParser' :: Parser String
stringParser' = do
  one <- char '1'
  two <- optional (char '2')
  three <- optional (char '3')
  return $ one : catMaybes [two, three]

spec :: SpecWith ()
spec =
  describe "Parsing Practice" $ do
    it "using eof" $ do
      testParse one "123" `shouldBe` Right '1'
      testParse oneTwo "123" `shouldBe` Right '2'
      testParse (one >> eof) "123" `shouldBe` Left ()
      testParse (oneTwo >> eof) "123" `shouldBe` Left ()
    it "using string" $ do
      testParse stringParser "1" `shouldBe` Right "1"
      testParse stringParser "12" `shouldBe` Right "12"
      testParse stringParser "123" `shouldBe` Right "123"
      testParse stringParser "11" `shouldBe` Left ()
      testParse stringParser "122" `shouldBe` Left ()
      testParse stringParser "13" `shouldBe` Left ()
      testParse stringParser "23" `shouldBe` Left ()
    it "using char" $ do
      testParse stringParser' "1" `shouldBe` Right "1"
      testParse stringParser' "12" `shouldBe` Right "12"
      testParse stringParser' "123" `shouldBe` Right "123"
      testParse stringParser' "11" `shouldBe` Left ()
      testParse stringParser' "122" `shouldBe` Left ()
      testParse stringParser' "13" `shouldBe` Left ()
      testParse stringParser' "23" `shouldBe` Left ()
  where
    testParse parser string =
      case parseString parser mempty string of
        Failure e -> Left ()
        Success a -> Right a
