module Web.WitAI.Types.Responses 
    ( module Web.WitAI.Types.Responses.Message
    , module Web.WitAI.Types.Responses.Converse
    , module Web.WitAI.Types.Responses.Entities
    ) where

import           Data.Text

import           Data.Aeson
import           Data.Aeson.Types           (typeMismatch)

import           Web.WitAI.Types.Responses.Message
import           Web.WitAI.Types.Responses.Converse
import           Web.WitAI.Types.Responses.Entities

-- ================== --
--   ERROR RESPONSE   --
-- ================== --

data WitAIErrorResponse = WitAIErrorResponse
  { witres_error_error :: Text
  , witres_error_code  :: Maybe Int
  } deriving (Eq, Show)


-- -------------------------- --
--  ERROR RESPONSE INSTANCES  --
-- -------------------------- --

instance FromJSON WitAIErrorResponse where
  parseJSON (Object o) = WitAIErrorResponse <$> o .: "error"
                                            <*> o .: "code"
  parseJSON wat = typeMismatch "WitAIErrorResponse" wat

instance ToJSON WitAIErrorResponse where
  toJSON (WitAIErrorResponse err code) = object [ "error" .= err
                                                , "code"  .= code
                                                ]
