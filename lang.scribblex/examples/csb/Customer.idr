import Control.ST
import Control.ST.ImplicitCall
import Control.IOExcept
import Network.Socket
import Network
import System
import Threads
import Data.String
import Decidable.Equality

import csb_C
import Boot

mutual
  protocol : (ConsoleIO io, CSB_C io) => STrans io () [] (const [])
  protocol = do
    putStrLn "[connecting to seller ...]"
    st <- start
    rec_protocol st
  
  rec_protocol : (ConsoleIO io, CSB_C io) =>
                 (l : Var) ->
                 STrans io ()
                   [l ::: StateT {m=io} Ready]
                   (const [])
  rec_protocol st = do
    putStrLn "Enter product name:"
    name <- getStr
    putStrLn "\n[requesting price ...]"
    reqPrice st name
    p <- priceInfo st
    putStrLn $ "[received price info; the price is: " ++ cast p ++ " ...]"
    putStrLn "Want to buy? If so, type \"Accept\"; if not, type anything else:"
    decision <- getStr
    case decision of 
      "Accept" => do
        putStrLn "\n[letting the seller know you've accepted ...]"
        choice_C0 st Accept
        putStrLn "\nPress [ENTER] to request bank transfer: "
        getStr
        reqTransfer st (p ** Refl)
        putStrLn "\n[DONE]"
        done st
      _ => do
        putStrLn "\n[letting the seller know you've rejected ...]"
        choice_C0 st Reject
        rec_protocol st


implementation (Sockets io, ConsoleIO io, ConsoleExcept String io, Monad io) => CSB_C io where
  StateT Ready  = Sock {m=io} Open
  StateT S1     = Sock {m=io} Open
  StateT (S2 p) = Sock {m=io} Open
  StateT (S3 p) = Sock {m=io} Open
  StateT (S4 p) = Sock {m=io} Closed

  start = do
    Right sock <- socket Stream | Left _ => failure "could not open socket"
    Right _ <- connect sock (Hostname "localhost") sellerPort | Left _ => failure "could not connect to seller"
    pure sock
       
  reqPrice sock s = do
    Right _ <- send sock s | Left _ => failure "could not send price request"
    pure ()
       
  priceInfo sock = do
    Right string <- recv sock | Left _ => failure "could not receive price info"
    case parsePositive {a=Int} string of
      Just x => pure x
      _      => failure "parse error: expected an integer price"

  choice_C0 sock Accept = do
    Right _ <- send sock "Accept" | Left _ => failure "could not send acceptance to seller"
    pure ()
  choice_C0 sock Reject = do
    Right _ <- send sock "Reject" | Left _ => failure "could not send reject to seller"
    pure ()
            
  reqTransfer sock0 (p ** Refl) = do
    close sock0
    Right sock <- socket Stream | Left _ => failure "could not open socket"
    Right _ <- connect sock (Hostname "localhost") bankPort | Left _ => failure "could not connect to bank"
    Right _ <- call (send sock (cast p)) | Left _ => failure "could not send to bank"
    call (close sock)
    call (remove sock)
  
  done sock = do remove sock

main : IO ()
main = do
  runIOExcept {err=String} $ run {m=IOExcept String} protocol
  pure ()

-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
