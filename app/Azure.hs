module Azure where

import           Data.Text

data CloudType = Prod | Dogfood deriving Show
data ResourceConfig = ResourceConfig {
      subscriptionId :: !Text
    , resourceGroup  :: !Text
}
