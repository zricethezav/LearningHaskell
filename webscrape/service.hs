import Control.Monad
import Control.Concurrent
import Network.HTTP.Conduit

-- main :: IO ()
-- main = forever $ putStrLn "do something"


get :: String -> IO String
get url = simpleHttp (getRequest url) >>= responseBody


main =
  forever $ do
    forkIO $ putStrLn "Do something"
    get "https://www.house.gov/representatives/"

    threadDelay $ 100 * 10^3


