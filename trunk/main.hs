module Main
where 

import Stomp
import System

main = do 
  args <- getArgs
  client<- connect "localhost" 61613 []
  subscribe client (args !! 0) [] $ 
            Listener  (\hdrs msg -> do
                         print (hdrs,msg)
                         send client "/queue/bar" [] ("resp:" ++ msg)
                      )
