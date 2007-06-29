module Main
where 

import Stomp
import System

main = do 
  args <- getArgs
  client <- connect "localhost" 61613 []
  subscribe client (args !! 0) [] 
  let queue = subscription client
  withSubscriptionDo   $ do
         (hdrs,msg) <- queue
         print (hdrs,msg)

        
         
