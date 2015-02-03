import           Test.Hspec
import qualified Prisms as TP
import qualified Fold   as TF

main :: IO ()
main = do
  hspec TP.suite
  hspec TF.suite
