module Main where

import Network.Curl
import System.Environment
import Control.Monad (forM_)

api_dev_key = "ff3d413082c193cbdf2431ab9699e722"
api_option = "paste"
postFields body = CurlPostFields [ "api_dev_key=" ++ api_dev_key, "api_option=" ++ api_option, "api_paste_code=" ++ body ] : method_POST


publish :: String -> IO (Either String String)
publish code = do
  curl <- initialize
  response <- do_curl_ curl "http://pastebin.com/api/api_post.php" (postFields code) :: IO CurlResponse
  if respCurlCode response /= CurlOK || respStatus response /= 200
    then return $ Left (show (respCurlCode response) ++ " -- " ++ respStatusLine response)
    else return $ Right (respBody response)

main :: IO ()
main = do
  files <- getArgs
  if files == []
    then do
    code <- getContents
    result <- publish code
    case result of
      Left e -> putStrLn e
      Right url -> putStrLn url
      
    else forM_ files $ \f -> do
    code <- readFile f
    result <- publish code
    case result of
      Left e -> putStrLn e
      Right url -> putStrLn url
      