import Control.IOExcept
import Control.ST

%access public export

-- Some convenience boilerplating

interface ConsoleExcept (e : Type) (m : Type -> Type) where
  raise : e -> m a

implementation ConsoleExcept e (IOExcept e) where
  raise e = ioe_fail e

failure : (ConsoleIO m, ConsoleExcept String m, Monad m) => String -> STrans m t xs r
failure {t = t} s =
  putStrLn s >>= (\ _ =>
  (>>=) {a = t} (lift (raise s)) (\_ => failure s)) -- hack!

sellerPort : Int
sellerPort = 9445

bankPort : Int
bankPort = 9443

-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
