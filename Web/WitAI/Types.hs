module Web.WitAI.Types
    ( module Web.WitAI.Types.Requests
    , module Web.WitAI.Types.Responses
    , module Web.WitAI.Types.Static
    ) where

import           Web.WitAI.Types.Requests
import           Web.WitAI.Types.Responses
import           Web.WitAI.Types.Static

import           Data.Text
import           Data.Default
import           Data.Map.Strict


type Message   = Text
type SessionID = Text
type Token     = Text
type Version   = Text
type Context   = Map Text Text

-- N.B. YOU MUST AT LEAST SET
-- def {witAI_token = INSERT YOUR TOKEN HERE}
data WitAIConverseSettings = WitAIConverseSettings
    { witAI_version    :: Version
  -- The wit.ai version used (date format YYYYMMDD) default = "20160731"
    , witAI_token      :: Token
  -- Your wit.ai token
    , witAI_session_id :: SessionID
  -- A unique id for this session
    , witAI_contect    :: Context
  -- The current context of the conversation.
  -- Needs to be supplied again after a 
    , witAI_textfunc   :: Message -> Message
  -- A function that manipulates the message before it's sent to wit.ai. default = toLower

data WitAIConverseHandlers = WitAIConverseHandlers
    , witAI_mergefunc   :: Map Text [WitAIConverseEntity] -> Context
  -- Function to handle the first MERGE to make the first context
    , witAI_actionfunc  :: (MonadIO m, MonadThrow m) => Action -> Context -> m Context
  -- Function to handle the ACTIONs received from wit.ai and update the context appropriately
    , witAI_messagefunc :: (MonadIO m, MonadThrow m) => Message -> SessionID -> Context -> m ()
  -- Function to handle MESSAGEs the bot should send and
  -- should save the Context with the SessionID to continue the conversation after the user replies.
    , witAI_stopfunc    :: (MonadIO m, MonadThrow m) => SessionID -> m ()
  -- Function to handle the STOP response and close the session
    }

instance Default WitAIConverseSettings where
    def = WitAIConverseSettings "20160731"
                                ""
                                toLower
                                "test"

data WitAIException = InvalidResponse String
  deriving Typeable

instance Show WitAIException where
    show (InvalidResponse t) = "Invalid response from witAI: " ++ t

instance Exception WitAIException