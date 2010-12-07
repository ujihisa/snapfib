{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Types
import           Snap.Util.FileServe
import           Text.Templating.Heist
import           Text.Templating.Heist.TemplateDirectory

import           Glue
import           Server
import           Data.ByteString.Char8 (pack, unpack)

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibInString :: Int -> String
fibInString n = "fib(" ++ show n ++ ") = " ++ show (fibs !! n)

main :: IO ()
main = do
    td <- newTemplateDirectory' "templates" emptyTemplateState
    quickServer $ templateHandler td defaultReloadHandler $ \ts ->
        ifTop (writeBS "hello world") <|>
        route [ ("foo", writeBS "bar")
              , ("fib/:n", fibHandler)
              ] <|>
        templateServe ts <|>
        dir "static" (fileServe ".")


fibHandler :: Snap ()
fibHandler = do
    param <- getParam "n"
    maybe (writeBS "must specify fib/n in URL")
          (writeBS . pack . fibInString . read . unpack) param
