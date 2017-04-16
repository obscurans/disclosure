import Test.Tasty
import qualified Disclosure.Base.Util.Test
import qualified Disclosure.Base.Range.Test
import qualified Disclosure.Base.CompoundRange.Test

main = defaultMain $ testGroup "All tests"
    [ Disclosure.Base.Util.Test.tests
    , Disclosure.Base.Range.Test.tests
    , Disclosure.Base.CompoundRange.Test.tests ]

