module Web.WitAI.Types.Requests.Speech where

import           Data.Text

import           Data.Aeson
import           Data.Aeson.Types     (typeMismatch)


-- ============================== --
--   GET SPEECH MEANING REQUEST   --
-- ============================== --

-- POST request to https://api.wit.ai/speech
data WitAISpeechRequest = WitAISpeechRequest 
    { witreq_speech_msg_id    :: Maybe Text
  -- A specific Id you want to assign to the speech that will be processed.
  -- If not set, Wit.ai will auto generate one for you
    , witreq_speech_thread_id :: Maybe Text
  -- A specific Id that will let you group requests per conversation
    , witreq_speech_n         :: Maybe Int
  -- The number of n-best outcomes you want to get back. default is 1
    } deriving (Eq, Show)


-- ------------------------------ --
--  GET SPEECH MEANING INSTANCES  --
-- ------------------------------ --

instance ToJSON WitAISpeechRequest where
    toJSON (WitAISpeechRequest msg_id thread_id n) = object [ "msg_id" .= msg_id
                                                            , "thread_id" .= thread_id
                                                            , "n" .= n
                                                            ]

instance FromJSON WitAISpeechRequest where
    parseJSON (Object o) = WitAISpeechRequest <$> o .:? "msg_id"
                                              <*> o .:? "thread_id"
                                              <*> o .:? "n"
    parseJSON wat = typeMismatch "WitAISpeechRequest" wat


{-
Headers

In addition to the authorization header, you must set a Content-type header to:

audio/wav     if you are sending a WAV file. We support RIFF WAVE (linear, little-endian).
audio/mpeg3   for mp3 file or stream
audio/ulaw    for G.711 u-law file or stream. Sampling rate must be 8khz
audio/raw.    For this content-type, the following parameters are mandatory

parameter possible values
encoding  signed-integer, unsigned-integer, floating-point, mu-law, a-law, ima-adpcm, ms-adpcm or gsm-full-rate
bits      8, 16, or 32
rate      an integer value like 8000 or 8k
endian    big or little (usually little, cf. this Wikipedia article)

Example:
'content-type': 'audio/raw;encoding=unsigned-integer;bits=16;rate=8000;endian=big'

For now Wit.ai is only able to process mono so you must make sure to send mono and not stereo to the API.
-}