module Main where

import Protolude
import Lib

import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.List as List

data Language = Purescript | Javascript | Other deriving (Eq, Ord, Show)


parseLanguage :: Parser Language
parseLanguage = choice
  [ "purescript" *> pure Purescript
  , "javascript" *> pure Javascript
  , pure Other
  ]

type LOC = (Int, Text)
type Block = [LOC]

-- a "ParsedBlock" is one that has simply gotten the org-mode stuff stripped,
-- with the header (everything following #+BEGIN_SRC) somehow processed and sent along (the `a`)
type ParsedBlock a = (a, [LOC])

parseBlock :: Block -> Maybe (ParsedBlock [Text])
parseBlock b = do
  header <- fmap T.words $ T.stripPrefix "#+BEGIN_SRC" =<< snd <$> headMay b
  rest <- tailMay b
  pure (header, rest)


headerProp :: Text -> ParsedBlock [Text] -> Maybe Text
headerProp prefix (h, _) = getFirst $ foldMap (First . T.stripPrefix (prefix <> ":")) h

blockLanguage :: ParsedBlock [Text] -> Maybe (ParsedBlock Language)
blockLanguage (h, b) = undefined

nextBlock :: [LOC] -> Maybe ([LOC], [LOC])
nextBlock ls = if List.null block'
  then Nothing
  else Just (block', rest')
  where begin (_, l) = "#+BEGIN_SRC" `T.isPrefixOf` l
        end   (_, l) = "#+END_SRC" `T.isPrefixOf` l
        stripped = dropWhile (not . begin) ls
        (block, rest) = break end stripped
        block' = block <> List.take 1 rest
        rest' = drop 1 rest

extractCodeblocks :: [Text] -> [Block]
extractCodeblocks ls = List.unfoldr nextBlock $ zip [1..] ls

{-
The only tools we have are those from the headers, and whatever is provided as a
cmd line argument. Headers are a list of words, and we don't want to depend too
much on arguments.
-}

-- filepathRule :: [Text] -> Maybe ([])

data Options = Options { filepath :: Maybe FilePath
                       , outputs :: Map [Text] FilePath
                       }

main :: IO ()
main = do
  args <- getArgs

  case args of
    [fn] -> do
      ls <- T.lines <$> readFile fn

      let raw = extractCodeblocks ls
      print $ length raw

      for_ (zip raw [1..]) $ \(cb,i) -> do
             putStrLn   ("#########################" :: Text)
             putStrLn $ "## codeblock #" <> (show i :: Text)
             putStrLn   ("#########################\n" :: Text)
             for_ cb (\(i, l) -> putStrLn $ show i <> "\t" <> l)

    _ -> print "please provide a filename to parse"
