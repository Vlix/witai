module Web.WitAI.Types.Requests.Message where

import Data.Text

import Data.Aeson           ((.:?))
import Data.Aeson.Types     (typeMismatch)


-- =============================== --
--   GET MESSAGE MEANING REQUEST   --
-- =============================== --

-- GET request to https://api.wit.ai/message
data WitAIMessageRequest = WitAIMessageRequest 
    { witreq_message_q         :: Text
  -- User’s query. Length must be > 0 and < 256
    --, witreq_message_context :: Maybe Context -- deprecated
    , witreq_message_msg_id    :: Maybe Text
  -- A specific Id you want to assign to the message that will be processed.
  -- If not set, Wit.ai will auto generate one for you
    , witreq_message_thread_id :: Maybe Text
  -- A specific Id that will let you group requests per conversation
    } deriving (Eq, Show)


-- ------------------------------- --
--  GET MESSAGE MEANING INSTANCES  --
-- ------------------------------- --

instance ToJSON WitAIMessageRequest where
    toJSON (WitAIMessageRequest q msg_id thread_id) = object [ "q" .= q
                                                             , "msg_id" .= msg_id
                                                             , "thread_id" .= thread_id
                                                             ]

instance FromJSON WitAIMessageRequest where
    parseJSON (Object o) = WitAIMessageRequest <$> o .: "q"
                                               <*> o .:? "msg_id"
                                               <*> o .:? "thread_id"
    parseJSON wat = typeMismatch "WitAIMessageRequest" wat
