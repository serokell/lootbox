{-# LANGUAGE RecordWildCards #-}
module Loot.Log.Internal.Message
       ( Message (..)
         -- * Formating and converting
       , fmtMessageFlat
       , fmtMessageColored
       , toSyslogMessage
       ) where

import Loot.Log.Internal.Name (Name)

import Colog.Syslog.Priority
import qualified Colog.Syslog.Message as SM

import Fmt ((|++|), (|+), (+|))
import System.Console.ANSI (Color (..), ColorIntensity (Vivid),
    ConsoleLayer (Foreground), SGR (SetColor, Reset), setSGRCode)

data Message = Message
    { msgSeverity :: Severity
    , msgName     :: Name
    , msgContent  :: Text
    }

-- | Simple formatting function to 'ByteString', without any coloring
fmtMessageFlat :: Message -> ByteString
fmtMessageFlat = encodeUtf8 . SM.fmtMessageFlat . toSyslogMessage

-- | Formatting function to 'ByteString' with colored 'Severity'
fmtMessageColored :: Message -> ByteString
fmtMessageColored Message {..} = (encodeUtf8 :: Text -> ByteString) $
    withColor|++|msgSeverity|++|resetColor|+" ["+|msgName|+"] "+|msgContent|+""
  where
    color = case msgSeverity of
        Emergency -> Red
        Alert     -> Red
        Critical  -> Red
        Error     -> Red
        Warning   -> Yellow
        Notice    -> Blue
        Info      -> White
        Debug     -> Green
    withColor = toText (setSGRCode [SetColor Foreground Vivid color])
    resetColor = toText (setSGRCode [Reset])

-- | Converts a 'Message' to a 'Colog.Syslog.Message'
toSyslogMessage :: Message -> SM.Message
toSyslogMessage Message {..} = SM.Message msgSeverity $ " ["+|msgName|+"] "+|msgContent|+""
