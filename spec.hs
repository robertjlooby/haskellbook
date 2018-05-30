import Chapter06
import Chapter07
import Chapter09
import Chapter10
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    Chapter06.spec
    Chapter07.spec
    Chapter09.spec
    Chapter10.spec
