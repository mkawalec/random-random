--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where


--------------------------------------------------------------------------------
import           Control.Concurrent  (forkIO)
import           Control.Monad       (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Network.Socket      (withSocketsDo)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS

import qualified Data.CaseInsensitive as CI
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (pack)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           System.IO.Unsafe (unsafePerformIO)

import Network.WebSockets.Connection

getTime :: IO Integer
getTime = getPOSIXTime >>= \t -> return (floor (toRational t))

twitterHeader :: ByteString
twitterHeader = pack $ "OAuth oauth_consumer_key=\"ytDrsZb2llyVvfbKOrcZbyA99\", oauth_nonce=\"a2537dd1997892cba1e4b6632b402423\", oauth_signature=\"ipetHSj4HlIn7Mfyhj2959N0iZg%3D\", oauth_signature_method=\"HMAC-SHA1\", oauth_timestamp=\"" ++ time ++ "\", oauth_token=\"847970634-SB9jgeMuaRoqYWTmRgLazCMwmje2aG4RrI6OuMmm\", oauth_version=\"1.0\""
    where time = show $ unsafePerformIO getTime

headers :: WS.Headers
headers = [(CI.mk $ ((pack "Authorization")::ByteString), twitterHeader)]

--------------------------------------------------------------------------------
app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"

    -- Fork a thread that writes WS data to stdout
    _ <- forkIO $ forever $ do
        msg <- WS.receiveData conn
        liftIO $ T.putStrLn msg

    -- Read from stdin and write to WS
    let loop = do
            line <- T.getLine
            unless (T.null line) $ WS.sendTextData conn line >> loop

    loop
    WS.sendClose conn ("Bye!" :: Text)


--------------------------------------------------------------------------------
main :: IO ()
main = withSocketsDo $ WS.runClientWith "stream.twitter.com" 443 
                                        "/1.1/statuses/sample.json" 
                                        defaultConnectionOptions headers app
