module Database (Database, empty, parse, render, addEntry, Entry(..), lookupEntryUser, hasEntry, entryNames) where

import           Prelude hiding (lookup)

import           Data.List (intercalate, sort)
import           Data.String.Utils
import           Data.Maybe
import           Control.DeepSeq
import           Text.Printf (printf)

import           Data.Config.String   (Config)
import qualified Data.Config.String as Config

import           Util (match, MatchResult(..))

data Entry = Entry {
  entryName     :: String
, entryUser     :: Maybe String
, entryPassword :: Maybe String
, entryUrl      :: Maybe String
} deriving (Eq, Show)

instance NFData Entry where
  rnf (Entry x1 x2 x3 x4) = rnf x1 `seq` rnf x2 `seq` rnf x3 `seq` rnf x4

newtype Database = Database { config :: Config }

empty :: Database
empty = Database Config.empty

lookupEntry :: Database -> String -> Either String [Entry]
lookupEntry db s = case match s $ entryNames db of
  None        -> Left "no match"
  Ambiguous l -> Left  $ printf "ambiguous, could refer to:\n  %s" $ intercalate "\n  " l
  Match name  -> Right $ getEntries db name

lookupEntryUser :: Database -> String -> Maybe String -> Either String [Entry]
lookupEntryUser db s mUser =
  case entries of
     Left _  -> entries
     Right l -> Right $ maybe l (maybeToList . findUser l) mUser
  where entries = lookupEntry db s

getEntries :: Database -> String -> [Entry]
getEntries db name = 
  map makeEntry $ zip3 users passwords urls
    where
      knownKeys    = sort $ Config.keys name (config db)
      isKey k x    = (x == k || startswith (k ++ "_") x)
      userSuffixes = map (drop 4) $ filter (isKey "user") knownKeys
      users        = map (lookup . ("user"++)) userSuffixes
      passwords    = map (lookup . ("password"++)) userSuffixes
      urls         = map (lookup . ("url"++)) userSuffixes
      lookup k     = Config.lookup name k (config db)

      makeEntry (us, pw, ur) = Entry {
           entryName = name
         , entryUser = us
         , entryPassword = pw
         , entryUrl = ur
         }

buildEntrySuffix :: Database -> String -> String
buildEntrySuffix db name = 
  newSuffixes !! 0
    where
      knownKeys    = sort $ Config.keys name (config db)
      isKey k x    = (x == k || startswith (k ++ "_") x)
      userSuffixes = map (drop 4) $ filter (isKey "user")     knownKeys
      passSuffixes = map (drop 8) $ filter (isKey "password") knownKeys
      urlSuffixes  = map (drop 3) $ filter (isKey "url") knownKeys
      suffixes     = userSuffixes ++ passSuffixes ++ urlSuffixes
      newSuffixes  = dropWhile (`elem` suffixes) ("":map (("_" ++) . show) [1..])

findUser :: [Entry] -> String -> Maybe Entry
findUser []     _  = Nothing
findUser (x:xs) u = if (entryUser x) == Just u
                       then Just x
                       else findUser xs u

hasEntry :: String -> Database -> Bool
hasEntry name = Config.hasSection name . config

entryNames :: Database -> [String]
entryNames = Config.sections . config

addEntry :: Database -> Entry -> Bool -> Either String Database
addEntry db entry forceAdd =
  case hasEntry name db of
    True  -> 
      if forceAdd && not (isJust $ findUser (getEntries db name) (fromJust $ entryUser entry))
         then Right $ doInsert
         else Left  $ printf "Entry with name \"%s\" already exists!" name
    False -> Right  $ doInsert
  where
    name  = entryName entry
    ni = buildEntrySuffix db name
    doInsert = db {config = insertEntry entry ni $ config db}

insertEntry :: Entry -> String-> Config -> Config
insertEntry entry ni =
    mInsert ("user" ++ ni)     user
  . mInsert ("password" ++ ni) password
  . mInsert ("url" ++ ni)      url
  where
    insert = Config.insert $ entryName entry
    mInsert k = maybe id (insert k)

    user     = entryUser entry
    password = entryPassword entry
    url      = entryUrl entry

parse :: String -> Database
parse input = either error Database (Config.parse input)

render :: Database -> String
render (Database db) = Config.render db
