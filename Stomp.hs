module Stomp (
  connect ,        
  disconnect,
  send,
  subscribe,
  withSubscriptionDo,
  subscription,
  Listener (..)
)

where

import Network
import IO
import Control.Monad.State
import Control.Monad
import Control.Concurrent
import System

data Command = CONNECT | DISCONNECT | SEND | 
               SUBSCRIBE | UNSUBSCRIBE | ACK
               deriving (Show,Eq)

data Client = Client Handle

type Headers = [(String,String)]
type Destination = String

newtype Listener a = Listener { 
      onMessage :: Headers -> String -> IO a
    }


splitToPair :: String -> Char -> (String,String)
splitToPair str sep = 
    (takeWhile (/= sep) str, tail $ dropWhile (/= sep) str)

parseHeaders :: Handle -> StateT Headers IO Headers
parseHeaders h = do 
  headers <- get
  line <- lift $ hGetLine h
  if line == "" then return headers
                else do 
                  modify ((splitToPair line ':') : )
                  parseHeaders h

parseBody :: Handle -> StateT String IO String
parseBody h = do
  body <- get
  c <- lift $ hGetChar h
  if c == '\NUL' then return $ reverse body
                 else do 
                   modify (c:)
                   parseBody h

parse :: Handle -> IO (String,Headers,String)
parse h = do
  command  <- hGetLine h
  if command == "" then parse h
       else do
         headers  <- evalStateT (parseHeaders h) []
         body     <- evalStateT (parseBody h) ""
         return (command,headers,body)

connect :: HostName -> PortNumber -> Headers -> IO Client
connect host port headers = withSocketsDo $ do
  h <- connectTo host  (PortNumber port)
  hSetBuffering h NoBuffering
  netSend h CONNECT headers ""
  (cmd,_,_) <- parse h
  if cmd == "CONNECTED" then return $ Client h
                        else fail "connection failed" 

disconnect :: Client -> Headers -> IO ()
disconnect (Client h) hdrs = netSend h DISCONNECT hdrs "" >> hClose h

send :: Client -> Destination -> Headers -> String -> IO ()
send (Client h) dest headers = netSend h SEND (("destination",dest):headers)
    
headersStr :: Headers -> String
headersStr  = concat . map (\(x,y) -> concat [x,": ",y,"\n"])
    
netSend :: Handle -> Command -> Headers -> String -> IO ()
netSend h cmd hdrs body = do
  let hdrs' = ("content-length",show $  length body):hdrs
  let str = concat [show cmd,"\n",headersStr hdrs',"\n",body,"\000"]
  hPutStr h str
  hFlush h


subscribe :: Client -> Destination -> Headers -> IO ()
subscribe (Client h) dest headers  = do
  netSend h SUBSCRIBE  (("destination",dest):headers)  ""

subscription :: Client -> IO (Headers,String)
subscription client@(Client h) = do
  (_,headers,body) <- parse h
  return (headers,body)

withSubscriptionDo :: IO () -> IO ()
withSubscriptionDo  io = io >>  withSubscriptionDo io
