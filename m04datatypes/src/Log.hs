module Log where
import Data.Time.Clock (UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info

data LogEntry = LogEntry {timestamp :: UTCTime, logLevel :: LogLevel, message :: String}

logLevelToString :: LogLevel -> String
logLevelToString Error = "Error"
logLevelToString Warning = "Warning"
logLevelToString Info = "Info"

logEntryToString :: LogEntry -> String
logEntryToString logge = (timeToString $ timestamp logge) ++ ": " ++ (logLevelToString $ logLevel logge) ++ ": " ++ (message logge)
