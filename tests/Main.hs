
import Crypto.Lol.Tests.Standard
import Crypto.Lol.Cyclotomic.Tensor.Accelerate
import Data.Proxy

import Test.Framework

main :: IO ()
main =
  flip defaultMainWithArgs ["--threads=1","--maximum-generated-tests=100"] $
    defaultTests (Proxy::Proxy AT)
