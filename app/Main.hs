{-# LANGUAGE OverloadedStrings #-}

module Main where
import           Data.Text.Lazy (Text)
import           MRequest       (MRequest (mGet), RequestIO, Unwrap (unwrap))
import           Network.Wreq   (Response)



getGoogle :: (MRequest m ) => m (Response Text)
getGoogle = mGet "https://sspeaks.net"

main :: IO ()
main = do
    v <- unwrap (getGoogle  :: RequestIO  (Response Text))
    print v
    return ()
