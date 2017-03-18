module Main where

import qualified Aws
import qualified Aws.Core                     as Aws
import qualified Aws.S3                       as S3
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString              as S
import           Data.ByteString.Char8        (pack)
import qualified Data.ByteString.Lazy         as L
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
    filePaths <- lfiles (directory config)
    manager <- newManager tlsManagerSettings

    creds <- Aws.makeCredentials
        (pack $ access_key config)
        (pack $ secret_access_key config)

    let cfg = Aws.Configuration Aws.Timestamp creds (Aws.defaultLog Aws.Debug)
    let s3cfg = S3.s3 Aws.HTTP (pack "s3-us-west-2.amazonaws.com") False

    forM_ filePaths $ uploadFilePath config cfg s3cfg manager

uploadFilePath config cfg s3cfg manager filePath = do
    -- streams large file content, without buffering more than 10k in memory
    let streamer sink = withFile filePath ReadMode $ \h -> sink $ S.hGet h 10240

    size <- liftIO (fromIntegral . fileSize <$> getFileStatus filePath :: IO Integer)

    let body = RequestBodyStream (fromInteger size) streamer
    let bucket = T.pack $ bucket_name config
    let projectName = T.pack $ project_name config
    let filePathWithoutDirectory = T.replace (T.pack $ directory config) (T.pack "") (T.pack filePath)
    let name = T.append projectName filePathWithoutDirectory
    let contentType = Just $ pack $ fileContentType filePath

    rsp <- runResourceT $ Aws.pureAws cfg s3cfg manager $
        (S3.putObject bucket name body)
            { S3.poContentType = contentType
            }

    liftIO $ print rsp
