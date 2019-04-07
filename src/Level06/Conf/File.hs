{-# LANGUAGE OverloadedStrings #-}
module Level06.Conf.File where

import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as LBS

import           Data.Text                  (Text, pack)

import           Data.Bifunctor             (first)
import           Data.Monoid                (Last (Last))

import           Waargonaut.Attoparsec      (pureDecodeAttoparsecByteString)

import           Control.Exception          (try)

import qualified Data.Attoparsec.ByteString as AB

import           Waargonaut                 (Json)
import qualified Waargonaut.Decode          as D
import           Waargonaut.Decode.Error    (DecodeError (ParseFailed))

import           Level06.AppM               (AppM, AppM (AppM), liftEither)
import           Level06.Types              (ConfigError (BadConfFile),
                                             PartialConf (PartialConf),
                                             partialConfDecoder)
-- $setup
-- >>> :set -XOverloadedStrings

-- | The configuration file is in the JSON format, so we need to write a
-- 'waargonaut' 'Decoder' to go from JSON to our 'PartialConf'.
--
-- Update these tests when you've completed this function.
--
-- >>> readConfFile "badFileName.no"
-- Left (undefined "badFileName.no: openBinaryFile: does not exist (No such file or directory)")
-- >>> readConfFile "files/test.json"
-- Right "{\n  \"foo\": 33\n}\n"
--
readConfFile
  :: FilePath
  -> AppM ConfigError ByteString
readConfFile fp =
  AppM $ first BadConfFile . pure <$> LBS.readFile fp

-- | Construct the function that will take a ``FilePath``, read it in, decode it,
-- and construct our ``PartialConf``.
parseJSONConfigFile
  :: FilePath
  -> AppM ConfigError PartialConf
parseJSONConfigFile fp =
  liftEither . first BadConfFile . doDecode =<< readConfFile fp
  where
    doDecode = pureDecodeAttoparsecByteString partialConfDecoder

-- Go to 'src/Level06/Conf.hs' next.
