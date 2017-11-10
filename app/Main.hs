module Main where

import Protolude
import Lib

import Data.Aeson
import Data.Aeson.Types (parseMaybe, parseEither, Parser)

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified Data.ByteString.Lazy as LBS

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import System.Directory (removeFile, makeAbsolute)
import System.IO (openTempFile, hClose)
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

pscRebuildCmd :: FilePath -> Value
pscRebuildCmd path = object [ ("command", String "rebuild")
                            , ("params", params)]
  where params = object [("file", String (T.pack path))]


data CompileStatus = CFail Text | CSuccess Text | ParseError Text deriving (Eq, Ord, Show)

{-
  All Responses are wrapped in the following format:
  {
    "resultType": "success|error",
    "result": Result|Error
  }

-- In the Success case you get a list of warnings in the compilers json format.
-- In the Error case you get the errors in the compilers json format
-}
parseRebuildOutput :: LByteString -> Maybe CompileStatus
parseRebuildOutput val = do
  obj <- decode val

  resultType <- parseMaybe (.: "resultType") obj

  case resultType :: Maybe Text of
    Just "error" -> CFail <$> parseMaybe (.: "result") obj
    Just "success" -> CSuccess <$> parseMaybe (.: "result") obj
    Just x -> Just $ ParseError x
    Nothing -> Just $ ParseError "Completely wrong"



pursIdeSendCmd :: Int -> Value -> IO LByteString
pursIdeSendCmd port v = do
  (Just hIn, Just hOut, ideErr, h) <-
    createProcess (shell $ "purs ide client -p " <> show port)
      { std_in = CreatePipe, std_out = CreatePipe }

  LBS.hPut hIn $ encode v
  void $ waitForProcess h
  LBS.hGetContents hOut


compilePS :: Int -> PurescriptBlock -> IO CompileStatus
compilePS port (PurescriptBlock b) = do
  (path, h) <- openTempFile "." "org.purs"
  T.IO.hPutStr h $ T.unlines $ contents b

  absPath <- makeAbsolute path
  results <- pursIdeSendCmd port $ pscRebuildCmd absPath

  hClose h
  removeFile path

  case parseRebuildOutput results of
    Nothing -> pure $ ParseError "error"
    Just x -> pure x


testBlock :: PurescriptBlock
testBlock = PurescriptBlock $ CodeBlock "purescript" mempty ls
  where ls = [ "module Test where"
             , "import Prelude"
             , "main = unit"
             ]


{-
The only tools we have are those from the headers, and whatever is provided as a
cmd line argument. Headers are a list of words, and we don't want to depend too
much on arguments.
-}


data Options = Options { projectPath :: FilePath
                       , filepath :: Maybe FilePath
                       , outputs :: Map [Text] FilePath
                       , port :: Int
                       }

main :: IO ()
main = do
  print "communicating w/ server"
  res <- compilePS 15910 testBlock
  case res of
    CFail x -> do
      print "compilation failure!"
      print "errors:"
      putStrLn x
    CSuccess x -> do
      print "compilation success!"
      print "warnings:"
      putStrLn x
    ParseError x -> do
      print "parse error"
      print x

  -- case args of
  --   [fn] -> do
  --     ls <- T.lines <$> readFile fn

  --     let raw = extractCodeblocks ls
  --     print $ length raw

  --     for_ (zip raw [1..]) $ \(cb,i) -> do
  --            putStrLn   ("#########################" :: Text)
  --            putStrLn $ "## codeblock #" <> (show i :: Text)
  --            putStrLn   ("#########################\n" :: Text)
  --            for_ cb (\(i, l) -> putStrLn $ show i <> "\t" <> l)

  --   _ -> print "please provide a filename to parse"
