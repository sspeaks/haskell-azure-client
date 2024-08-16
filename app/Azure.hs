module Azure where

import           Data.Text.Lazy

data CloudType = Prod | Dogfood deriving Show
data ResourceConfig = ResourceConfig {
      subscriptionId :: Text
    , resourceGroup  :: Text
}
