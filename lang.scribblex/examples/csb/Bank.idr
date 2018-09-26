import Control.ST
import Control.ST.ImplicitCall
import Control.IOExcept
import Network.Socket
import Network
import System
import Threads
import Data.String
import Decidable.Equality

import csb_B
import Boot

using (ConsoleExcept e, ConsoleIO io, CSB_B io)
  protocol : STrans io () [] (const [])
  protocol = do
    putStrLn "[starting ...]"
    st <- start
    putStrLn "[listening for transfer request ...]"
    p <- choice_C0 st
    putStrLn $ "[received transfer request for " ++ cast p ++ " funds ...]"
    putStrLn "[sending acknowledgement of transfer ...]"
    ackTransfer st (p ** Refl)
    putStrLn "[DONE]"
    done st


implementation (Sockets io, ConsoleIO io, ConsoleExcept String io, Monad io) => CSB_B io where
  StateT Ready  = Composite [Sock {m=io} Listening, Sock {m=io} Open]
  StateT (S1 _) = Composite [Sock {m=io} Listening, Sock {m=io} Open]
  StateT (S2 _) = Composite [Sock {m=io} Closed]

  start = do
    Right sock <- socket Stream | Left _ => failure "could not open socket"
    Right _ <- bind sock Nothing 9443 | Left _ => failure "could not bind"
    Right _ <- listen sock | Left _ => failure "could not listen"
    Right sock' <- accept sock | Left _ => failure "could not accept"
    st <- new ()
    combine st [sock, sock']
    pure st
       
  choice_C0 st = do
    [sock0, sock] <- split st
    Right string <- recv sock | Left _ => failure "could not receive"
    combine st [sock0, sock]
    case (parsePositive {a=Int} string) of
      Just x => pure x
      _      => failure "parse error req"
       
  ackTransfer st (p ** Refl) = do
    [sock0, sock] <- split st
    close sock; remove sock
    Right sock' <- socket Stream | Left _ => failure "could not open socket"
    Right _ <- connect sock' (Hostname "localhost") 9442 | Left _ => failure "could not connect to seller"
    Right _ <- send sock' (cast p) | Left _ => failure "could not send ack"
    close sock'; remove sock'
    close sock0
    combine st [sock0];
  
  done st = do
    [sock0] <- split st
    call (remove sock0)
    delete st


main : IO ()
main = do
  runIOExcept {err=String} $ run {m=IOExcept String} protocol
  pure ()

-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
