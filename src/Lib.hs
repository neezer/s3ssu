{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Lib
    ( argConfig
    , defaultFromEnv
    ) where

import           System.Console.CmdArgs
import           System.Posix.Env       (getEnvDefault)

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
        help "Set the AWS access key"
    , secret_access_key = def &=
        typ "S3_SECRET_ACCESS_KEY" &=
        help "Set the AWS secret access key"
    , bucket_name = def &=
        typ "DOCS_BUCKET_NAME" &=
        help "Set the name of the S3 bucket to upload to"
    , project_name = def &=
        typ "PROJECT_NAME" &=
        help "Set the name of your project, which will be the folder name on S3"
    , directory = def &=
        argPos 0 &=
        typ "DIRECTORY_TO_UPLOAD"
    } &=
    program "s3ssu" &=
    verbosity &=
    help "Upload a folder to an S3 bucket as a static site" &=
    summary "s3ssu v0.0.0, (C) Kofile" &=
    details [
        "Example use:"
    ]

defaultTo :: String -> String -> String
defaultTo fallback ""        = fallback
defaultTo fallback preferred = preferred


defaultFromEnv :: Config -> IO Config
defaultFromEnv config = do
    envAccessKey <- getEnvDefault "S3_ACCESS_KEY" ""
    envSecretAccessKey <- getEnvDefault "S3_SECRET_ACCESS_KEY" ""
    envBucketName <- getEnvDefault "DOCS_BUCKET_NAME" ""

    return config { access_key = defaultTo envAccessKey (access_key config)
                  , secret_access_key = defaultTo envSecretAccessKey (secret_access_key config)
                  , bucket_name = defaultTo envBucketName (bucket_name config)
                  }
