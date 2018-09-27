import Control.ST
import Control.ST.ImplicitCall
import Control.IOExcept
import Network.Socket
import Network
import System
import Threads
import Data.String
import Decidable.Equality

import csb_S
import Boot

mutual
  protocol : (ConsoleIO io, CSB_S io) => STrans io () [] (const [])
  protocol = do
    putStrLn "[waiting for customer to connect ...]"
    st <- start
    rec_protocol st

  rec_protocol : (ConsoleIO io, CSB_S io) =>
                 (l : Var) ->
                 STrans io ()
                   [l ::: StateT {m=io} Ready]
                   (const [])
  rec_protocol st = do
    putStrLn "[customer connected! waiting for product price request ...]"
    name <- reqPrice st
    putStrLn $ "[received price request for: \"" ++ name ++ "\" ...]"
    (let p = (if name == "Plumbus" 
              then 60 
              else 5) in do
      putStrLn $ "[sending price info amount " ++ cast p ++ " ...]"
      priceInfo st p
      putStrLn $ "[listening for customer's decision ...]"
      c <- choice_C0 st
      case c of
        Reject => do
          putStrLn "[customer rejected the offer]"
          rec_protocol st
        Accept => do
          putStrLn $ "[customer accepted! waiting for bank acknowledgement of transfer ...]"
          ackTransfer st
          putStrLn "[received acknowledgement]"
          putStrLn "[DONE]"
          done st)


implementation (Sockets io, ConsoleIO io, ConsoleExcept String io, Monad io) => CSB_S io where
  StateT Ready  = Composite [Sock {m=io} Listening, Sock {m=io} Open]
  StateT S1     = Composite [Sock {m=io} Listening, Sock {m=io} Open]
  StateT (S2 p) = Composite [Sock {m=io} Listening, Sock {m=io} Open]
  StateT (S3 p) = Composite [Sock {m=io} Listening, Sock {m=io} Open]
  StateT (S4 p) = Composite [Sock {m=io} Closed]

  start = do
    Right sock <- socket Stream | Left _ => failure "could not open socket"
    Right _ <- bind sock Nothing sellerPort | Left _ => failure "could not bind"
    Right _ <- listen sock | Left _ => failure "could not listen"
    Right sock' <- accept sock | Left _ => failure "could not accept"
    st <- new ()
    combine st [sock, sock']
    pure st
       
  reqPrice st = do
    [sock0, sock] <- split st
    Right string <- recv sock | Left _ => failure "could not receive price req"
    combine st [sock0, sock]
    pure string
       
  priceInfo st p = do
    [sock0, sock] <- split st
    Right _ <- send sock (cast p) | Left _ => failure "could not send price info"
    combine st [sock0, sock]

  choice_C0 st = do
    [sock0, sock] <- split st
    Right string <- recv sock | Left _ => failure "could not receive buyer choice"
    combine st [sock0, sock]
    case string of
      "Accept" =>
        do pure Accept
      _ =>
        do pure Reject
      
  ackTransfer {p} st = do
    [sock0, sock] <- split st
    close sock; remove sock
    Right sock' <- accept sock0 | Left _ => failure "could not bind ack"
    Right string <- call (recv sock') | Left _ => failure "could not receive ack"
    close sock'; remove sock'
    close sock0
    combine st [sock0]
    case (parsePositive {a=Int} string) of
      Just x =>
        case decEq p x of
          Yes Refl => pure (x ** Refl)
          _      => failure "invariant error"
      _      => failure "parse error ack"
  
  done st =  do
    [sock] <- split st
    call (remove sock)
    delete st


main : IO ()
main = do
  runIOExcept {err=String} $ run {m=IOExcept String} protocol
  pure ()

-- Local Variables:
-- idris-load-packages: ("contrib")
-- End:
