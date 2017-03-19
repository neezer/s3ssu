module Main where

import qualified Aws
import qualified Aws.Core                     as Aws
import qualified Aws.S3                       as S3
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString              as S
import           Data.ByteString.Char8        (pack)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Lib
import           Network.HTTP.Conduit         (RequestBody (..), newManager,
                                               tlsManagerSettings)
import           System.Console.CmdArgs
import           System.IO
import           System.Posix.Files


main :: IO ()
main = do
    config <- cmdArgs argConfig
    config <- defaultFromEnv config

    let Config access_key secret_access_key _ _ directory = config

    filePaths <- lfiles directory
    manager <- newManager tlsManagerSettings

    creds <- Aws.makeCredentials (pack access_key) (pack secret_access_key)
    verbosity <- getVerbosity

    let cfg = Aws.Configuration Aws.Timestamp creds (Aws.defaultLog $ logLevel verbosity)
    let s3cfg = S3.s3 Aws.HTTP (pack "s3-us-west-2.amazonaws.com") False

    forM_ filePaths $ uploadFilePath config cfg s3cfg manager


uploadFilePath config cfg s3cfg manager filePath = do
    let Config _ _ bucket_name project_name directory = config
    let streamer sink = withFile filePath ReadMode $ \h -> sink $ S.hGet h 10240

    size <- liftIO (fromIntegral . fileSize <$> getFileStatus filePath :: IO Integer)

    let body = RequestBodyStream (fromInteger size) streamer
    let name = s3FilePath (T.pack project_name) (T.pack directory) (T.pack filePath)

    runResourceT $ Aws.pureAws cfg s3cfg manager $
        (S3.putObject (T.pack bucket_name) name body)
            { S3.poContentType = Just $ pack $ fileContentType filePath
            }


projectPath :: Text -> Text -> Text
projectPath directory = T.replace directory T.empty


s3FilePath :: Text -> Text -> Text -> Text
s3FilePath projectName directory filePath =
    T.append projectName $ projectPath directory filePath


logLevel :: Verbosity -> Aws.LogLevel
logLevel Quiet  = Aws.Error
logLevel Normal = Aws.Info
logLevel Loud   = Aws.Debug
