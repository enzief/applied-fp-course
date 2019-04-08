module Level07.Types.Topic
  ( Topic
  , mkTopic
  , getTopic
  , encodeTopic
  , topicParser
  ) where

import           Database.SQLite.Simple          (SQLData (SQLText))
import           Database.SQLite.Simple.FromRow  (FromRow (fromRow), field)
import           Database.SQLite.Simple.Internal (RowParser)
import           Database.SQLite.Simple.ToRow    (ToRow (toRow))

import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E

import           Data.Functor.Contravariant ((>$<))
import           Data.Text                  (Text)

import           Level07.Types.Error        (Error (EmptyTopic), nonEmptyText)

newtype Topic = Topic Text
  deriving (Show)

encodeTopic :: Applicative f => Encoder f Topic
encodeTopic = getTopic >$< E.text

topicParser :: RowParser (Either Error Topic)
topicParser = mkTopic <$> field

mkTopic
  :: Text
  -> Either Error Topic
mkTopic =
  nonEmptyText Topic EmptyTopic

getTopic
  :: Topic
  -> Text
getTopic (Topic t) =
  t

instance FromRow Topic where
  fromRow = Topic <$> field

instance ToRow Topic where
  toRow t = [SQLText . getTopic $ t]
