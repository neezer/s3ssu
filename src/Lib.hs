{-# LANGUAGE DeriveDataTypeable #-}

module Lib
    ( argConfig
    , defaultFromEnv
    , lfiles
    , fileContentType
    , Config (..)
    ) where

import           Data.Foldable          (toList)
import           Data.Text              (replace)
import           System.Console.CmdArgs
import           System.Directory.Tree
import           System.FilePath.Posix  (takeExtension)
import           System.Posix.Env       (getEnvDefault)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}


data Config = Config { access_key        :: String
                     , secret_access_key :: String
                     , bucket_name       :: String
                     , project_name      :: String
                     , directory         :: FilePath
                     }
                     deriving (Show, Data, Typeable)


argConfig :: Config
argConfig = Config
    { access_key = def &=
        typ "S3_ACCESS_KEY" &=
        help "Set the AWS access key" &=
        groupname "Flags"
    , secret_access_key = def &=
        typ "S3_SECRET_ACCESS_KEY" &=
        help "Set the AWS secret access key"
    , bucket_name = def &=
        typ "DOCS_BUCKET_NAME" &=
        help "Set the name of the S3 bucket"
    , project_name = def &=
        typ "PROJECT_NAME" &=
        help "Set the name of your project\n\n(should be equivalent to the name of your Github repo)"
    , directory = def &=
        argPos 0 &=
        typ "DIRECTORY_TO_UPLOAD"
    } &=
    program "s3ssu" &=
    verbosity &=
    help "Upload a folder to an S3 bucket as a static site" &=
    summary "s3ssu v0.0.0, (C) Kofile" &=
    details [ ""
            , "Example use:"
            ]


defaultTo :: String -> String -> String
defaultTo fallback "" = fallback
defaultTo _ preferred = preferred


defaultFromEnv :: Config -> IO Config
defaultFromEnv config = do
    envAccessKey <- getEnvDefault "S3_ACCESS_KEY" ""
    envSecretAccessKey <- getEnvDefault "S3_SECRET_ACCESS_KEY" ""
    envBucketName <- getEnvDefault "DOCS_BUCKET_NAME" ""

    let Config access_key secret_access_key bucket_name _ _ = config

    return config { access_key = defaultTo envAccessKey access_key
                  , secret_access_key = defaultTo envSecretAccessKey secret_access_key
                  , bucket_name = defaultTo envBucketName bucket_name
                  }


lfiles :: FilePath -> IO [FilePath]
lfiles dirpath = toList . dirTree <$> readDirectoryWith return dirpath


fileContentType :: FilePath -> String
fileContentType filePath = case takeExtension filePath of
    ".png"  -> "image/png"
    ".html" -> "text/html"
    ".js"   -> "application/javascript"
    ".css"  -> "text/css"
    ".svg"  -> "image/svg+xml"
    _       -> "text/plain"
