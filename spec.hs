import Chapter06
import Chapter07
import Chapter09
import Chapter10
import Chapter11
import Chapter12
import Chapter13
import Chapter14
import Chapter15
import Chapter16
import Chapter17
import Chapter18
import Chapter20
import Chapter21
import Chapter22
import Chapter23
import Chapter25
import Chapter26
import Chapter28
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
    Chapter16.spec
    Chapter17.spec
    Chapter18.spec
    Chapter20.spec
    Chapter21.spec
    Chapter22.spec
    Chapter23.spec
    Chapter25.spec
    Chapter26.spec
    Chapter28.spec
