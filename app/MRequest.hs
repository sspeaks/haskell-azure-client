{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

module MRequest where
import           Control.Monad.Except    (ExceptT, MonadError, runExceptT)
import           Control.Monad.IO.Class  (MonadIO (liftIO))
import           Control.Monad.State     (MonadState, StateT (runStateT))
import           Control.Monad.Trans     (MonadTrans (lift))
import           Data.Text.Lazy          (Text)
import qualified Data.Text.Lazy          as T
import qualified Data.Text.Lazy.Encoding as LT
import           Network.Wreq            (Response)
import qualified Network.Wreq            as Client

data RequestError = RequestError
    deriving Show
data AppState = AppState

class (Functor m, Applicative m, Monad m, MonadError RequestError m) => MRequest m where
    mGet :: Text -> m (Response Text)

instance (MonadTrans t, MRequest m, Functor (t m),
    Applicative (t m), Monad (t m), MonadError RequestError (t m))
    => MRequest (t m) where
    mGet = lift . mGet

newtype RequestIO a = RequestIO {
    unRequestIO :: ExceptT RequestError (StateT AppState IO) a
} deriving(Functor, Applicative, Monad, MonadIO, MonadState AppState, MonadError RequestError)

instance MRequest RequestIO where
    mGet url = fmap (fmap LT.decodeUtf8) . liftIO . Client.get $ T.unpack url

class (MRequest m) => Unwrap m where
    unwrap :: m a -> IO a
instance Unwrap RequestIO where
    unwrap m = do
        res <-  fmap fst . flip runStateT AppState . runExceptT . unRequestIO $ m
        case res of
            Left err -> error $ show err
            Right a  -> return a
