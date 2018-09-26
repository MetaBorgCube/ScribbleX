import Control.IOExcept
import Control.ST

-- Some convenience boilerplating

public export
interface ConsoleExcept (e : Type) (m : Type -> Type) where
  raise : e -> m a

public export
implementation ConsoleExcept e (IOExcept e) where
  raise e = ioe_fail e

public export
failure : (ConsoleIO m, ConsoleExcept String m, Monad m) => String -> STrans m t xs r
failure {t = t} s =
  putStrLn s >>= (\ _ =>
  (>>=) {a = t} (lift (raise s)) (\_ => failure s)) -- hack!


-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
