import qualified Fold       as TF
import qualified Prisms     as TP
import           Test.Hspec

main :: IO ()
main = do
  hspec TP.suite
  hspec TF.suite
