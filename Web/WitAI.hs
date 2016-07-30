{-# LANGUAGE PatternGuards     #-}

module Web.WitAI where

import           Web.WitAI.Types

--import           Data.ByteString.Lazy
import           Data.Text
import           Data.Text.Lazy.Encoding    (encodeUtf8)
import           Data.Aeson
import           Data.Map.Strict
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Network.HTTP.Client        (parseRequest, Request(..))

witAI :: (MonadIO m, MonadThrow m) => WitAIConverseHandlers -> WitAIConverseSettings -> Text -> m ()
witAI handlers settings msg = recWitAIRequest True empty
  where
    recWitAIRequest isFirst contextmap = do
        req' <- parseRequest "https://api.wit.ai/converse"
        let request' = req' { requestHeaders = [ (hAccept, "application/json")
                                               , (hContentType, "application/json")
                                               , (hAuthorization,"Bearer " <> (encodeUtf8 $ witAI_token settings))
                                               ]
                            , method = "POST"
                            , requestBody = RequestBodyLBS $ encode contextmap
                            }
            isFirst' = if isFirst
                            then [("q", Just . encodeUtf8 $ witAI_textfunc msg)]
                            else []
            version   = witAI_version settings
            sessionid = witAI_session_id settings
            querystring = [ ("v", Just $ encodeUtf8 version)
                          , ("session_id", Just $ encodeUtf8 sessionid)
                          ] ++ isFirst' isFirst
            request = setQueryString querystring request'
        res <- liftIO $ httpLbs tlsManagerSettings request
        -- The message is sent to Wit.AI
        let responseBS = responseBody res :: ByteString
        -- This is what Wit.AI found and below is how to formulate a response
        case (eitherDecode' responseBS :: Either String WitAIConverseResponse) of
            -- this is the ParseConverseResponse being sent to the new handler?...
            Right response ->
                case witres_converse_type response of
                    Stop    -> witAI_stopfunc handlers sessionid
                    Merge   -> do
                        let Just ents = witres_converse_entities response
                            newContext   = witAI_mergefunc handlers ents
                        recWitAIRequest False newContext
                    Action  -> do
                        let Just action = witres_converse_action response
                        newContext <- witAI_actionfunc handlers action contextmap
                        recWitAIRequest False newContext
                    Message -> do
                        let Just message = witres_converse_msg response
                        witAI_messagefunc handlers message sessionid contextmap
            Left error'    -> throwM $ InvalidResponse error'