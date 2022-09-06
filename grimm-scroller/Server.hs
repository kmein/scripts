module Main where

import Data.Text (pack, Text)
import Control.Monad (forever, forM_)
import System.IO (stderr, hPutStrLn)
import Control.Concurrent (threadDelay, forkIO)
import qualified Network.WebSockets as WS
import qualified Data.Text.IO as T
import Control.Concurrent.Chan.Unagi
import Options.Applicative

data Options = Options
  { host :: String
  , port :: Int
  , delay :: Double
  , loop :: Bool
  }

options :: Parser Options
options = Options
  <$> strOption (long "host" <> short 'h' <> metavar "ADDRESS" <> value "127.0.0.1" <> showDefault <> help "The host to listen on")
  <*> option auto (long "port" <> short 'p' <> metavar "PORT" <> help "The port to listen on")
  <*> option auto (long "delay" <> value 1000 <> metavar "MILLISECONDS" <> showDefault <> help "Delay between sending messages")
  <*> switch (long "loop" <> short 'l' <> help "Whether to loop the input")

main :: IO ()
main = do
  theOptions <- execParser $ info (options <**> helper) $ fullDesc <> progDesc "Broadcast text to websocket"
  (inChan, _) <- newChan
  forkIO $ forever $ do
    inputLine <- T.getLine
    writeChan inChan inputLine
    threadDelay $ truncate $ 1000 * delay theOptions
  WS.runServer (host theOptions) (port theOptions) (application inChan)

application :: InChan Text -> WS.ServerApp
application inChan pending = do
  outChan <- dupChan inChan
  hPutStrLn stderr ("New client connected: " ++ show (WS.pendingRequest pending))
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ forever $ do
    text <- readChan outChan
    WS.sendTextData conn (text)
