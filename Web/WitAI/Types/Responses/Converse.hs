{-# LANGUAGE FlexibleInstances #-}

module Web.WitAI.Types.Responses.Converse where

import           Data.Text

import           Data.Aeson
import           Data.Aeson.Types     (typeMismatch)
import           Data.Map.Strict

import           Web.WitAI.Types.Static

-- ===================== --
--   CONVERSE RESPONSE   --
-- ===================== --

data WitAIConverseResponse = WitAIConverseResponse
    { witres_converse_type       :: WitAIConverseType
    , witres_converse_entities   :: Maybe (Map Text [WitAIConverseEntity])
    , witres_converse_msg        :: Maybe Text
    , witres_converse_action     :: Maybe Text
    , witres_converse_confidence :: Double
    } deriving (Eq, Show)

data WitAIConverseEntity = WitAIConverseEntity
    { witai_converse_entity_body   :: Maybe Text
    , witai_converse_entity_value  :: Maybe (Either Text WitAIConverseEntityValue)
    , witai_converse_entity_start  :: Maybe Int
    , witai_converse_entity_end    :: Maybe Int
    , witai_converse_entity_entity :: Maybe Text
    } deriving (Eq, Show)

data WitAIConverseEntityValue = WitAIConverseEntityValue
    { witai_converse_value_type      :: Maybe Text
    , witai_converse_value_value     :: Maybe Text
    , witai_converse_value_suggested :: Maybe Bool
    } deriving (Eq, Show)


-- ----------------------------- --
--  CONVERSE RESPONSE INSTANCES  --
-- ----------------------------- --

instance FromJSON WitAIConverseResponse where
    parseJSON (Object o) = WitAIConverseResponse <$> o .: "type"
                                                 <*> o .:? "entities"
                                                 <*> o .:? "msg"
                                                 <*> o .:? "action"
                                                 <*> o .: "confidence"
    parseJSON wat = typeMismatch "WitAIConverseResponse" wat

instance FromJSON WitAIConverseEntity where
    parseJSON (Object o) = WitAIConverseEntity <$> o .:? "body"
                                               <*> o .:? "value"
                                               <*> o .:? "start"
                                               <*> o .:? "end"
                                               <*> o .:? "entity"
    parseJSON wat = typeMismatch "WitAIConverseEntity" wat

instance FromJSON WitAIConverseEntityValue where
    parseJSON (Object o) = WitAIConverseEntityValue <$> o .:? "type"
                                                    <*> o .:? "value"
                                                    <*> o .:? "suggested"
    parseJSON wat = typeMismatch "WitAIConverseEntityValue" wat

instance {-# OVERLAPPING #-} FromJSON (Either Text WitAIConverseEntityValue) where
    parseJSON obj@(Object _) = Right <$> parseJSON obj
    parseJSON str@(String _) = Left  <$> parseJSON str
    parseJSON wat = typeMismatch "(Either Text WitAIConverseEntityValue)" wat


instance ToJSON WitAIConverseResponse where
    toJSON (WitAIConverseResponse typ entities msg action confidence) = object [ "type"       .= typ
                                                                               , "entities"   .= entities
                                                                               , "msg"        .= msg
                                                                               , "action"     .= action
                                                                               , "confidence" .= confidence
                                                                               ]

instance ToJSON WitAIConverseEntity where
    toJSON (WitAIConverseEntity body value start end entity) = object [ "body"   .= body
                                                                      , "value"  .= value
                                                                      , "start"  .= start
                                                                      , "end"    .= end
                                                                      , "entity" .= entity
                                                                      ]

instance ToJSON WitAIConverseEntityValue where
    toJSON (WitAIConverseEntityValue typ value suggested) = object [ "type"      .= typ
                                                                   , "value"     .= value
                                                                   , "suggested" .= suggested
                                                                   ]

instance {-# OVERLAPPING #-} ToJSON (Either Text WitAIConverseEntityValue) where
    toJSON (Left a)  = String a
    toJSON (Right b) = toJSON b