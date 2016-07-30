module Web.WitAI.Types
    ( module Web.WitAI.Types.Requests
    , module Web.WitAI.Types.Responses
    , module Web.WitAI.Types.Static
    , WitAIConverseSettings (..)
    , WitAIConverseHandlers (..)
    , WitAIException (..)
    ) where

import           Data.Text
import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Catch
import           Data.Typeable

import           Data.Map.Strict

import           Web.WitAI.Types.Requests
import           Web.WitAI.Types.Responses
import           Web.WitAI.Types.Static


type Version   = Text
type Token     = Text
type SessionID = Text
type Context   = Map Text Text
type Message   = Text
type Action    = Text

-- N.B. YOU MUST AT LEAST SET
-- def {witAI_token = INSERT YOUR TOKEN HERE}
data WitAIConverseSettings = WitAIConverseSettings
    { witAI_version    :: Version
  -- The wit.ai version used (date format YYYYMMDD) default = "20160731"
    , witAI_token      :: Token
  -- Your wit.ai token
    , witAI_session_id :: SessionID
  -- A unique id for this session
    , witAI_context    :: Context
  -- The current context of the conversation.
  -- Needs to be supplied again after a 
    , witAI_textfunc   :: Message -> Message
  -- A function that manipulates the message before it's sent to wit.ai. default = toLower
    }

data (MonadIO m, MonadThrow m) => WitAIConverseHandlers m = WitAIConverseHandlers
    { witAI_mergefunc   :: Map Text [WitAIConverseEntity] -> Context
  -- Function to handle the first MERGE to make the first context
    , witAI_actionfunc  :: Action -> Context -> m Context
  -- Function to handle the ACTIONs received from wit.ai and update the context appropriately
    , witAI_messagefunc :: Message -> SessionID -> Context -> m ()
  -- Function to handle MESSAGEs the bot should send and
  -- should save the Context with the SessionID to continue the conversation after the user replies.
    , witAI_stopfunc    :: SessionID -> m ()
  -- Function to handle the STOP response and close the session
    }

class Default a where
  def :: a

instance Default WitAIConverseSettings where
    def = WitAIConverseSettings "20160731"
                                ""
                                "test"
                                Data.Map.Strict.empty
                                toLower

data WitAIException = InvalidResponse String
  deriving Typeable

instance Show WitAIException where
    show (InvalidResponse t) = "Invalid response from witAI: " ++ t

instance Exception WitAIException