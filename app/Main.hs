{-# LANGUAGE OverloadedStrings #-}

module Main where
import           Azure
import qualified Data.Text      as T
import qualified Data.Text.Lazy as LT
import           MRequest       (MRequest (mGet))
import           Network.Wreq   (Response)
import           ResourceParser (Spec (Spec), parseSpec)
import           System.Process (readProcess)
import           Text.Parsec    (runParser)



getGoogle :: (MRequest m ) => m (Response LT.Text)
getGoogle = mGet "https://sspeaks.net"

getAuthHeader :: IO String
getAuthHeader =  head . lines . ("Bearer " <>) <$> readProcess "az" ["account", "get-access-token", "--query", "accessToken", "--output", "tsv"] []

queryResource :: (MRequest m) => ResourceConfig -> Spec -> m (Response LT.Text)
queryResource (ResourceConfig sub rg) (Spec name rp rt api pl)
    = let url = T.concat ["https://management.azure.com/subscriptions/", sub, "/resourceGroups/", rg, "/providers/", rp, "/", rt, "/", name, "?", api]
      in undefined

main :: IO ()
main = do
    -- v <- unwrap (getGoogle  :: RequestIO  (Response Text))

    -- print v
    test <- readFile "input.txt"
    let res = runParser parseSpec () "spec" test
    case res of
        Left err  -> print $ show err
        Right val -> print $ show val
    return ()
