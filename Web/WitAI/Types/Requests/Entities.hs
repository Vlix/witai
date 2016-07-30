module Web.WitAI.Types.Requests.Entities where

import           Data.Text

import           Data.Aeson
import           Data.Aeson.Types     (typeMismatch)


-- ==================== --
--   ENTITIES REQUEST   --
-- ==================== --

-- POST request to https://api.wit.ai/entities
data WitAIEntitiesRequest = WitAIEntitiesRequest
    { witreq_entities_id      :: Text
  -- ID or name of the requested entity
    , witreq_entities_doc     :: Maybe Text
  -- Short sentence describing this entity
    , witreq_entities_values  :: Maybe [WitAIValue]
  -- Possible values for this entity
    , witreq_entities_lookups :: Maybe [Text]
  -- Currently only supporting “trait” or “keywords” Search Strategy.
  -- If not provided, it will default to “keywords”. Traits are only available for new 'Bot Engine apps'
    } deriving (Eq, Show)

-- PUT request to https://api.wit.ai/entities/$ENTITY_ID
-- NOTE: You cannot update the lookups field (aka Search Strategy) via the API
data WitAIUpdateEntitiesRequest = WitAIUpdateEntitiesRequest
    { witreq_updateentities_id     :: Maybe Text
  -- ID or name of the entity
    , witreq_updateentities_doc    :: Maybe Text
  -- Short sentence describing this entity
    , witreq_updateentities_values :: Maybe [WitAIValue]
  -- Possible values for this entity
    } deriving (Eq, Show)

-- Also functions as POST request to https://api.wit.ai/entities/$ENTITY_ID/values
data WitAIValue = WitAIValue
    { witai_value_value       :: Text
  -- Name of this value for the Entity
    , witai_value_expressions :: Maybe [Text]
  -- These are full sentences when the 'lookups' is set to "trait"
    , witai_value_metadata    :: Maybe Text
  -- Data the developer would like to attach to this Value
    } deriving (Eq, Show)

-- POST request to https://api.wit.ai/entities/$ENTITY_ID/values/$VALUE_ID/expressions
newtype WitAIExpressionEntitiesRequest =
    WitAIExpressionEntitiesRequest { witreq_entities_expression :: Text }


-- ---------------------------- --
--  ENTITIES REQUEST INSTANCES  --
-- ---------------------------- --

instance ToJSON WitAIEntitiesRequest where
    toJSON (WitAIEntitiesRequest id doc values lookups) = object [ "id"      .= id
                                                                 , "doc"     .= doc
                                                                 , "values"  .= values
                                                                 , "lookups" .= lookups
                                                                 ]

instance ToJSON WitAIUpdateEntitiesRequest where
    toJSON (WitAIUpdateEntitiesRequest id doc values) = object [ "id"      .= id
                                                               , "doc"     .= doc
                                                               , "values"  .= values
                                                               ]

instance ToJSON WitAIValue where
    toJSON (WitAIValue value exps metadata) = object [ "value"       .= value
                                                     , "expressions" .= exps
                                                     , "metadata"    .= metadata
                                                     ]

instance ToJSON WitAIExpressionEntitiesRequest where
    toJSON (WitAIExpressionEntitiesRequest exp) = object [ "expression" .= exp ]


instance FromJSON WitAIEntitiesRequest where
    parseJSON (Object o) = WitAIEntitiesRequest <$> o .: "id"
                                                <*> o .:? "doc"
                                                <*> o .:? "values"
                                                <*> o .:? "lookups"
    parseJSON wat = typeMismatch "WitAIEntitiesRequest" wat

instance FromJSON WitAIUpdateEntitiesRequest where
    parseJSON (Object o) = WitAIUpdateEntitiesRequest <$> o .: "id"
                                                      <*> o .:? "doc"
                                                      <*> o .:? "values"
    parseJSON wat = typeMismatch "WitAIUpdateEntitiesRequest" wat

instance FromJSON WitAIValue where
    parseJSON (Object o) = WitAIValue <$> o .: "value"
                                      <*> o .:? "expressions" 
                                      <*> o .:? "metadata" 
    parseJSON wat = typeMismatch "WitAIValue" wat

instance FromJSON WitAIExpressionEntitiesRequest where
    parseJSON (Object o) = WitAIExpressionEntitiesRequest <$> o .: "expression"
    parseJSON wat = typeMismatch "WitAIExpressionEntitiesRequest" wat
