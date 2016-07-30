module Web.WitAI.Types.Responses.Entities where

import           Data.Text

import           Data.Aeson
import           Data.Aeson.Types                   (typeMismatch)

import           Web.WitAI.Types.Requests.Entities  (WitAIValue)

-- ===================== --
--   ENTITIES RESPONSE   --
-- ===================== --

-- Response to a POST /entities request
data WitAIEntitiesResponse = WitAIEntitiesResponse
    { witres_entities_id      :: Text
    , witres_entities_name    :: Text
    , witres_entities_lookups :: Maybe [Text]
    , witres_entities_exotic  :: Maybe Bool
    , witres_entities_builtin :: Maybe Bool
    , witres_entities_lang    :: Maybe Text
    , witres_entities_doc     :: Maybe Text
    , witres_entities_values  :: Maybe [WitAIValue]
-- Response to a GET /entities/$ENTITY_ID request will have values
    } deriving (Eq, Show)


-- Response to a DELETE request of /entities/$ENTITY_ID or /entities/$ENTITY_ID/values/$ENTITY_VALUE
newtype WitAIDeleteEntityResponse =
    WitAIDeleteEntityResponse { witres_entity_deleted :: Text }
  deriving (Eq, Show)


-- ----------------------------- --
--  ENTITIES RESPONSE INSTANCES  --
-- ----------------------------- --

instance FromJSON WitAIEntitiesResponse where
    parseJSON (Object o) = WitAIEntitiesResponse <$> o .: "id"
                                                 <*> o .: "name"
                                                 <*> o .:? "lookups"
                                                 <*> o .:? "exotic"
                                                 <*> o .:? "builtin"
                                                 <*> o .:? "lang"
                                                 <*> o .:? "doc"
                                                 <*> o .:? "values"
    parseJSON wat = typeMismatch "WitAIEntitiesResponse" wat

instance FromJSON WitAIDeleteEntityResponse where
    parseJSON (Object o) = WitAIDeleteEntityResponse <$> o .: "deleted"
    parseJSON wat = typeMismatch "WitAIDeleteEntityResponse" wat


instance ToJSON WitAIEntitiesResponse where
    toJSON (WitAIEntitiesResponse id      name  lookups exotic
                                  builtin lang  doc     values) = object [ "id"      .= id
                                                                         , "name"    .= name
                                                                         , "lookups" .= lookups
                                                                         , "exotic"  .= exotic
                                                                         , "builtin" .= builtin
                                                                         , "lang"    .= lang
                                                                         , "doc"     .= doc
                                                                         , "values"  .= values
                                                                         ]

instance ToJSON WitAIDeleteEntityResponse where
    toJSON (WitAIDeleteEntityResponse del) = object [ "deleted" .= del ]
