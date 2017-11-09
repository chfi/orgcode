module Main where

import Protolude
import Lib

import Data.Aeson
import Data.Aeson.Types (parseMaybe, parseEither, Parser)

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text as T

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import System.Process



{-
The syntax of a code block:
#+NAME: <name>
#+BEGIN_SRC <language> <switches> <header arguments>
  <body>
#+END_SRC
-}

data CodeBlock a = CodeBlock { language :: Text
                             , args :: Map Text Text
                             , contents :: a
                             } deriving (Eq, Ord, Functor)


argsEqual :: Text -> CodeBlock a -> CodeBlock a -> Bool
argsEqual arg b1 b2 = fromMaybe False $ do
  arg1 <- Map.lookup arg (args b1)
  arg2 <- Map.lookup arg (args b2)
  pure $ arg1 == arg2

type LOC = (Int, Text)
type RawBlock = [LOC]

-- The next header argument is the two next words,
-- if the first word starts with a ':'
nextHeaderArg :: [Text] -> Maybe (Maybe (Text, Text), [Text])
nextHeaderArg (name:val:ws)
  | ":" `T.isPrefixOf` name = Just (Just (name, val), ws)
  | otherwise = Just (Nothing, val:ws) -- just one step forward if current word pair fails
nextHeaderArg _ = Nothing

parseArgs :: [Text] -> Map Text Text
parseArgs = Map.fromList . catMaybes . List.unfoldr nextHeaderArg

parseHeader :: [Text] -> Maybe (Text, Map Text Text)
parseHeader header = do
  language <- headMay header
  args <- parseArgs <$> tailMay header
  pure (language, args)

parseCodeBlock :: [Text] -> Maybe (CodeBlock [Text])
parseCodeBlock ls = do
  header <- fmap T.words $ T.stripPrefix "#+BEGIN_SRC" =<< headMay ls
  (lang, args) <- parseHeader header
  code <- initMay =<< tailMay ls
  pure $ CodeBlock lang args code


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

extractCodeblocks :: [Text] -> [RawBlock]
extractCodeblocks ls = List.unfoldr nextBlock $ zip [1..] ls


-- codeblocks are in the same equivalence class
-- re: output when these parts are equal:
--     language
--     imports to prepend (prologue)
--     output filepath (file)
equivalentBlocks :: CodeBlock a -> CodeBlock a -> Bool
equivalentBlocks b1 b2 = language b1 == language b2 &&
                         argsEqual "prologue" b1 b2 &&
                         argsEqual "file" b1 b2


-- | Output should be a list of lists of codeblocks that can be combined, in order.
-- | Culls empty output lists.
groupOutputBlocks :: [CodeBlock a] -> [NonEmpty (CodeBlock a)]
groupOutputBlocks = mapMaybe NE.nonEmpty . List.groupBy equivalentBlocks

-- | Assumes the blocks in the input list are equivalent according to `equivalentBlocks`;
-- | grouped according to `groupOutputBlocks`!
-- combineBlocks :: [CodeBlock] -> CodeBlock
combineBlocks :: Monoid a => NonEmpty (CodeBlock a) -> CodeBlock a
combineBlocks bs = (NE.head bs) { contents = foldMap contents bs }


newtype PurescriptBlock = PurescriptBlock (CodeBlock [Text])

psBlocks :: [CodeBlock [Text]] -> [PurescriptBlock]
psBlocks = mapMaybe (\b -> case language b of
                              "purescript" -> Just $ PurescriptBlock b
                              _ -> Nothing)



pscRebuildCmd :: Text -> Value
pscRebuildCmd path = object [ ("command", String "rebuild")
                            , ("params", params)]
  where params = object [("file", String path)]

parseEither' :: (a -> Parser b) -> a -> Either Text b
parseEither' f a = first T.pack $ parseEither f a


data CompileStatus = CFail [Text] | CSuccess [Text] deriving (Eq, Ord, Show)

{-
  All Responses are wrapped in the following format:
  {
    "resultType": "success|error",
    "result": Result|Error
  }

-- In the Success case you get a list of warnings in the compilers json format.
-- In the Error case you get the errors in the compilers json format
-}
parseRebuildOutput :: ByteString -> Maybe CompileStatus
parseRebuildOutput val = do
  obj <- decodeStrict val
  resultType <- parseMaybe (\o -> o .: "resultType") obj :: Maybe Text
  case resultType of
    "error" -> do
      errors <- parseMaybe (\o -> o .: "result") obj :: Maybe [Text]
      Just $ CFail errors
    "success" -> do
      warnings <- parseMaybe (\o -> o .: "result") obj :: Maybe [Text]
      Just $ CSuccess warnings
    _ -> Nothing

-- {
--   "command": "rebuild",
--   "params": {
--     "file": "/path/to/file.purs"
--     "actualFile": "/path/to/actualFile.purs"
--   }
-- }


{-
The only tools we have are those from the headers, and whatever is provided as a
cmd line argument. Headers are a list of words, and we don't want to depend too
much on arguments.
-}

-- filepathRule :: [Text] -> Maybe ([])

data Options = Options { projectPath :: FilePath
                       , filepath :: Maybe FilePath
                       , outputs :: Map [Text] FilePath
                       }

main :: IO ()
main = do
  -- print "communicating w/ server"
  -- (Just hIn, Just hOut, ideErr, h) <-
  --   createProcess shell "purs ide client" { std_in = CreatePipe
  --                                         , std_out = CreatePipe
  --                                         }

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
