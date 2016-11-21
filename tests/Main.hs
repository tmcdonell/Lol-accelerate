
import Crypto.Lol.Cyclotomic.Tensor.Accelerate
import Crypto.Lol.Tests.Standard
import Data.Proxy

main :: IO ()
main = defaultTestMain (Proxy::Proxy AT)