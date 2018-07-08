import Chapter06
import Chapter07
import Chapter09
import Chapter10
import Chapter11
import Chapter12
import Chapter13
import Chapter14
import Chapter15
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    Chapter06.spec
    Chapter07.spec
    Chapter09.spec
    Chapter10.spec
    Chapter11.spec
    Chapter12.spec
    Chapter13.spec
    Chapter14.spec
    Chapter15.spec
