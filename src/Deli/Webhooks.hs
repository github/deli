module Deli.Webhooks
    ( WebhookDelivery(..)
    , readWebhookDeliveries
    ) where

import Control.Lens (to)
import Data.List (intercalate)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Deli

data WebhookDelivery = WebhookDelivery
    { _wdStart :: !Deli.Time
    , _wdDuration :: !Deli.Duration
    , _wdHost :: !String
    } deriving (Show)

webhookDeliveryTiming
    :: WebhookDelivery
    -> Deli.JobTiming
webhookDeliveryTiming delivery =
    Deli.JobTiming (_wdStart delivery) (_wdDuration delivery)

instance Deli.HasJobTiming WebhookDelivery where
    jobTiming = to webhookDeliveryTiming

readWebhookDeliveries
    :: FilePath
    -> IO [WebhookDelivery]
readWebhookDeliveries path = do
    contents <- BS.readFile path
    let fileLines = BS.lines contents
        rows = BS.split ',' <$> fileLines
        asStrings = fmap BS.unpack <$> rows
        deliveries = rowToWebhookDelivery <$> asStrings
    return (zeroTimes deliveries)

rowToWebhookDelivery
    :: [String]
    -> WebhookDelivery
rowToWebhookDelivery [startS, _, durationS, hostS] =
    let startI :: Integer
        startI = read startS

        durationI :: Integer
        durationI = read durationS

    in WebhookDelivery
        { _wdStart = Deli.millisecondsToTime startI
        , _wdDuration = Deli.millisecondsToDuration durationI
        , _wdHost = hostS
        }
rowToWebhookDelivery row = error $ "Row has the wrong number of columns: " ++ intercalate "," row

zeroTimes
    :: [WebhookDelivery]
    -> [WebhookDelivery]
zeroTimes [] = []
zeroTimes xs@(h:_tl) =
    let epoch = _wdStart h
        durationToTime (Deli.Duration d) = Deli.Time d
        zeroT r = r { _wdStart = durationToTime (Deli.subtractTime (_wdStart r) epoch) }
    in zeroT <$> xs
