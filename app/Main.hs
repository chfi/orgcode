module Main where

import Protolude
import Lib

import Data.Attoparsec.Text
import qualified Data.Text as T

import Unsafe (unsafeIndex)

data Language = Purescript | Javascript | Other deriving (Eq, Ord, Show)

data Codeblock a = Codeblock { lang :: Language
                             , lineStart :: Int
                             , lineEnd :: Int
                             , contents :: a
                             }

type RawCodeblock = [Text]

parseLanguage :: Parser Language
parseLanguage = choice
  [ "purescript" *> pure Purescript
  , "javascript" *> pure Javascript
  , pure Other
  ]


getBlock :: Text -> (Maybe RawCodeblock, [RawCodeblock]) -> (Maybe RawCodeblock, [RawCodeblock])
getBlock t (Nothing, sofar)
  | "#+BEGIN_SRC" `T.isPrefixOf` t = (Just [], sofar)
  | otherwise = (Nothing, sofar) -- this could be problematic; should return Maybe ([a], [[a]])
getBlock t (Just x, sofar)
  | "#+END_SRC" `T.isPrefixOf` t = (Nothing, sofar <> [reverse x])
  | otherwise = (Just (t : x), sofar)

extractCodeblocks :: [Text] -> [RawCodeblock]
extractCodeblocks ls = snd $ foldl (flip getBlock) (Nothing, []) ls

main :: IO ()
main = do
  args <- getArgs

  case args of
    [fn] -> do
      ls <- T.lines <$> readFile fn

      let raw = extractCodeblocks ls
      print $ length raw

      for_ (zip raw [1..]) $ \(cb,i) -> do
             print $ "codeblock " <> (show i :: Text)
             for_ cb putStrLn

    _ -> print "please provide a filename to parse"
