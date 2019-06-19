{-|

Nagios monitoring plugin for Obelisk miner API

-}

module CheckObelisk where

import Control.Monad (when)
import Control.Exception.Base (try, IOException)
import Network.Wreq (responseBody, responseStatus, statusCode)
import qualified Network.Wreq.Session as S
import Network.Socket ( getAddrInfo, defaultHints, getNameInfo, addrAddress
                      , NameInfoFlag (NI_NUMERICHOST, NI_NUMERICSERV), AddrInfo)
import GHC.Generics (Generic)
import Data.Aeson ( ToJSON(toEncoding), FromJSON, genericToEncoding, defaultOptions, encode
                  , Value(Number,String)
                  )
import Options.Applicative
  ( execParser, info, header, progDesc, fullDesc, helper, Parser, option, long, short, metavar
  , value, help, str, auto, showDefault, infoOption)
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Control.Lens ((^?), (^..))
import Data.Aeson.Lens (key, values)
import Text.Read (readEither)
import Data.Scientific (Scientific)
import Paths_check_obelisk (version)
import Data.Version (showVersion)
import System.Nagios.Plugin ( NagiosPlugin, runNagiosPlugin, addResult
                            , CheckStatus (Unknown, Warning, Critical, OK)
                            , addPerfDatum, PerfValue (RealValue), UOM (NullUnit) )


data LoginRequest = LoginRequest {
      username :: T.Text
    , password :: T.Text
    } deriving (Generic, Show)
instance ToJSON LoginRequest where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON LoginRequest

data CliOptions = CliOptions
  { host :: String
  , port :: String
  , optUser :: String
  , optPass :: String
  , temp_warning :: Double
  , temp_error :: Double
  }

defaultHost :: String
defaultHost = "127.0.0.1"
defaultPort :: String
defaultPort = "80"
defaultUsername :: String
defaultUsername = "admin"
defaultPassword :: String
defaultPassword = "admin"
defaultTempWarningThreshold :: Double
defaultTempWarningThreshold = 90
defaultTempCriticalThreshold :: Double
defaultTempCriticalThreshold = 100


cliOptions :: Parser CliOptions
cliOptions = CliOptions
  <$> option str
      ( long "host"
     <> short 'H'
     <> metavar "HOST"
     <> value defaultHost
     <> help "Hostname of miner API"
     <> showDefault
      )
  <*> option str
      ( long "port"
     <> short 'P'
     <> metavar "PORT"
     <> value defaultPort
     <> help "Port of miner API"
     <> showDefault
      )
  <*> option str
      ( long "username"
     <> short 'u'
     <> metavar "STRING"
     <> value defaultUsername
     <> help "Username of login to API"
     <> showDefault
      )
  <*> option str
      ( long "password"
     <> short 'p'
     <> metavar "STRING"
     <> value defaultPassword
     <> help "Password of login to API"
     <> showDefault
      )
  <*> option auto
      ( long "temp_warn"
     <> short 't'
     <> metavar "NUMBER"
     <> value defaultTempWarningThreshold
     <> help "Warning temperature threshold in Celsius"
     <> showDefault
      )
  <*> option auto
      ( long "temp_crit"
     <> short 'T'
     <> metavar "NUMBER"
     <> value defaultTempCriticalThreshold
     <> help "Critical temperature threshold in Celsius"
     <> showDefault
      )

getExhaustTemp :: BL.ByteString -> [Rational]
getExhaustTemp t =
  maybe [] (\dsv -> expectRational <$> dsv ^.. values . key "exhaustTemp") $ t ^?
  key "hashboardStatus"

-- We really want a rational from the data so make it happen here.
expectRational :: Value -> Rational
expectRational (Number n) = toRational n
expectRational (String s) = textToRational s
expectRational e = error $ "Could not parse " ++ show e
textToRational :: T.Text -> Rational
textToRational t =
  let e = (readEither $ T.unpack t) :: Either String Scientific
  in case e of
    Left s -> if t == ""
              then 0
              else error $ s ++ "\nFailed to parse number: '" ++ T.unpack t ++ "'"
    Right r -> toRational r


mainExec :: IO ()
mainExec = execParser opts >>= execCheck
  where
    opts = info (helper <*> versionOption <*> cliOptions)
      ( fullDesc
     <> progDesc "Return Nagios formatted string based on Obelisk miner API returned values"
     <> header "check_obelisk - Nagios monitoring plugin for Obelisk miner API" )
    versionOption = infoOption ("check_obelisk " ++ showVersion version)
      ( long "version" <> short 'v' <> help "Show version information" )


maximumTempThreshold :: Double
maximumTempThreshold = 120
minimumTempThreshold :: Double
minimumTempThreshold = 20

newtype Stats = Stats [Rational]
data Thresholds = Thresholds Rational Rational

checkStats :: Stats -> Thresholds -> NagiosPlugin ()
checkStats (Stats temp) (Thresholds tw tc) = do
  when (any (tw <=) temp) $
    addResult Warning ("Temperature over warning threshold of " <> (T.pack . show) (toDouble tw) <> " C" )

  when (any (tc <=) temp) $
    addResult Critical ("Temperature over critical threshold of " <> (T.pack . show) (toDouble tc) <> " C" )

  addResult OK $ "Max temp: " <> (T.pack . show) (toDouble $ maximum temp) <> " C"

  addTempData "exhaustTemp" temp
  where
    toDouble :: Rational -> Double
    toDouble = fromRational
    addTempData :: T.Text -> [Rational] -> NagiosPlugin ()
    addTempData s ts = let indexTemps = zip [s <> (T.pack . show) i | i <- [1..(length ts)]] ts
                       in mapM_ (uncurry addTempData') indexTemps

    addTempData' s t = addPerfData s t minimumTempThreshold maximumTempThreshold tw tc
    addPerfData s t mint maxt w c = addPerfDatum s (RealValue $ fromRational t) NullUnit
                              (Just $ RealValue mint) (Just $ RealValue maxt)
                              (Just $ RealValue $ fromRational w) (Just $ RealValue $ fromRational c)

execCheck :: CliOptions -> IO ()
execCheck (CliOptions h p user pass tw tc) = do
  sess <- S.newSession

  -- Resolve IP address for Obelisk because some checks were failing from LXD containers
  -- when using hostname instead of IP address.
  addrs <- try (getAddrInfo (Just defaultHints) (Just h) (Just "http")) :: IO (Either IOException [AddrInfo])

  (Just host_ip, _) <- case addrs of
    Right (addr:_) -> getNameInfo [NI_NUMERICHOST, NI_NUMERICSERV] True True $ addrAddress addr
    (Right []) -> do runNagiosPlugin $ addResult Unknown $ "Could not get ip address for '" <> T.pack h <> "'"
                     undefined
    Left _ -> do runNagiosPlugin $ addResult Unknown $ "Could not get ip address for hostname '" <> T.pack h <> "'"
                 undefined

  -- Login
  login_request <- S.post sess (url host_ip p "/api/login") $ encode $ LoginRequest (T.pack user) (T.pack pass)

  case login_request ^? responseStatus . statusCode of
    (Just 200) -> return ()
    _ -> runNagiosPlugin $ addResult Unknown "Could not parse dashboard response."

  -- Get Dashboard
  dashboard_request <- S.get sess $ url host_ip p "/api/status/dashboard"

  -- Check to see if stats are over threshold
  case getExhaustTemp <$> dashboard_request ^? responseBody of
    Just temp -> runNagiosPlugin $ checkStats (Stats temp) (Thresholds (toRational tw) (toRational tc))
    _ -> runNagiosPlugin $ addResult Unknown "Could not parse dashboard response."

  -- Logout
  _ <- S.post sess (url host_ip p "/api/logout") (""::B.ByteString)
  return ()
  where url h' p' r = "http://" ++ h' ++ ":" ++ p' ++ r
