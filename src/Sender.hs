import Stomp
import System

main = do 
  args <- getArgs
  client<- connect "localhost" 61613 []
  send client (args !! 0) [] (args !! 1)
  
  
