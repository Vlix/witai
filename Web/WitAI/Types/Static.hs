module Web.WitAI.Types.Static where

import           Data.Text

import           Data.Aeson
import           Data.Aeson.Types           (typeMismatch)


-- ================ --
--   STATIC TYPES   --
-- ================ --

data WitAIConverseType = Merge | Action | Message | Stop
  deriving (Eq, Show)


-- ----------------------- --
--  STATIC TYPE INSTANCES  --
-- ----------------------- --

instance FromJSON WitAIConverseType where
  parseJSON (String "merge")  = pure Merge
  parseJSON (String "action") = pure Action
  parseJSON (String "msg")    = pure Message
  parseJSON (String "stop")   = pure Stop
  parseJSON (String _)        = fail "Incorrect WitAIConverseType"
  parseJSON wat = typeMismatch "WitAIConverseType" wat

instance ToJSON WitAIConverseType where
  toJSON Merge   = "merge"
  toJSON Action  = "action"
  toJSON Message = "msg"
  toJSON Stop    = "stop"
