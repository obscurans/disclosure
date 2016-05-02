import Test.Tasty
import qualified Disclosure.Base.Util.Test

main = defaultMain $ testGroup "All tests"
    [ Disclosure.Base.Util.Test.tests ]

