{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- http://haskell-servant.github.io/posts/2015-07-24-pulling-mock-servers-out-of-thin-air.html
-- http://haskell-servant.github.io/posts/2015-08-05-content-types.html

module Lib
    ( someFunc
    ) where

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import           Control.Monad.Except
import           Data.Aeson
import           Data.Text
import           GHC.Generics
import           Servant
import           Network.Wai.Handler.Warp

data Excuse = Excuse { claim :: !Text } deriving (Eq,Ord,Generic)
instance ToJSON Excuse where

defaultExcuses :: [Excuse]
defaultExcuses = [
    Excuse "take back control"
  ]

type ExcuseAPI = "excuse" :> "all" :> Get '[JSON] [Excuse]
            :<|> "excuse" :> "add" :> QueryParam "claim" Text :> Post '[JSON] Excuse

serveExcuses :: TVar [Excuse] -> Server ExcuseAPI
serveExcuses excuses = allExcuses :<|> addExcuses
  where
    allExcuses :: ExceptT ServantErr IO [Excuse]
    allExcuses = liftIO (readTVarIO excuses)

    addExcuses :: Maybe Text -> ExceptT ServantErr IO Excuse
    addExcuses Nothing = throwError err400
    addExcuses (Just shit) = do
        let e = Excuse shit
        liftIO . atomically $ modifyTVar excuses (e:) 
        return e

brexitServer :: TVar [Excuse] -> Server BrexitAPI
brexitServer excuses = serveExcuses excuses

type BrexitAPI = ExcuseAPI

brexit :: Proxy BrexitAPI
brexit = Proxy

someFunc :: IO ()
someFunc = do
    excuses <- newTVarIO defaultExcuses
    run 8080 (serve brexit $ brexitServer excuses)
