module Main where

import Protolude hiding (decodeUtf8)
import Lib

import Data.Aeson
import Data.Aeson.Types (parseMaybe, parseEither, Parser)

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.IO as T.IO
import qualified Data.ByteString.Lazy as LBS

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

import System.Directory (removeFile, makeAbsolute)
import System.IO (openTempFile, hClose, hFlush)
import System.Process


data CodeBlock a = CodeBlock { language :: Text
                             , args :: Map Text Text
                             , contents :: a
                             } deriving (Eq, Ord, Functor)


argsEqual :: Text -> CodeBlock a -> CodeBlock a -> Bool
argsEqual arg b1 b2 = fromMaybe False $ do
  arg1 <- Map.lookup arg (args b1)
  arg2 <- Map.lookup arg (args b2)
  pure $ arg1 == arg2

argsOrder :: Text -> CodeBlock a -> CodeBlock a -> Ordering
argsOrder arg b1 b2 = fromMaybe EQ $ do
  arg1 <- Map.lookup arg (args b1)
  arg2 <- Map.lookup arg (args b2)
  pure $ arg1 `compare` arg2

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

rawBlocks :: [Text] -> [RawBlock]
rawBlocks ls = List.unfoldr nextBlock $ zip [1..] ls


-- codeblocks are in the same equivalence class
-- re: output when these parts are equal:
--     language
--     imports to prepend (prologue)
--     output filepath (file)
orderBlocks :: CodeBlock a -> CodeBlock a -> Ordering
orderBlocks b1 b2 = (language b1 `compare` language b2) `compare`
                    (argsOrder "prologue" b1 b2) `compare`
                    (argsOrder "file" b1 b2)

equivalentBlocks :: CodeBlock a -> CodeBlock a -> Bool
equivalentBlocks b1 b2 = EQ == orderBlocks b1 b2


-- | Output should be a list of lists of codeblocks that can be combined, in order.
-- | Culls empty output lists.
groupOutputBlocks :: [CodeBlock a] -> [[CodeBlock a]]
groupOutputBlocks =
  List.groupBy equivalentBlocks .
  List.sortBy orderBlocks

-- | Assumes the blocks in the input list are equivalent according to `equivalentBlocks`;
-- | grouped according to `groupOutputBlocks`!
combineBlocks :: Monoid a => [CodeBlock a] -> Maybe (CodeBlock a)
combineBlocks (b:bs) = Just $ b { contents = foldMap contents bs }
combineBlocks _ = Nothing


newtype PurescriptBlock = PurescriptBlock (CodeBlock [Text])

purescriptBlocks :: [CodeBlock [Text]] -> [PurescriptBlock]
purescriptBlocks = mapMaybe (\b -> case language b of
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

parseRebuildOutput :: LByteString -> CompileStatus
parseRebuildOutput val =
  case (hasSuccess, hasError) of
    (True, _) -> CSuccess "Compilation success"
    (_, True) -> CFail "Compilation failure"
    _ -> ParseError $ LT.toStrict val'

  where val' = decodeUtf8 val
        hasSuccess = "success" `LT.isInfixOf` val'
        hasError = "error" `LT.isInfixOf` val'


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

  case T.unpack <$> Map.lookup "prologue" (args b) of
    Nothing -> pure ()
    Just ip -> do
      print "prepending imports"
      importsPath <- makeAbsolute ip
      imports <- T.IO.readFile importsPath
      T.IO.hPutStrLn h imports

  T.IO.hPutStrLn h $ T.unlines $ contents b
  hFlush h

  absPath <- makeAbsolute path
  results <- pursIdeSendCmd port $ pscRebuildCmd absPath

  hClose h
  removeFile path

  pure $ parseRebuildOutput results


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

  progArgs <- getArgs

  case progArgs of
    [p, fn] -> do
      ls <- T.lines . LT.toStrict . decodeUtf8 <$> LBS.readFile fn

      port <- case readMaybe p of
        Nothing -> undefined
        Just p' -> pure p'


      let raw :: [[Text]]
          raw = fmap snd <$> rawBlocks ls
          grouped :: [[CodeBlock [Text]]]
          grouped = groupOutputBlocks $ mapMaybe parseCodeBlock raw
          combined :: [CodeBlock [Text]]
          combined = mapMaybe combineBlocks grouped
          pursBlocks :: [PurescriptBlock]
          pursBlocks = purescriptBlocks combined
          sorted =

      print "grouped"
      traverse_ (\bs -> print (fmap args bs)) grouped
      print "combined"
      traverse_ (\b -> print (args b)) combined

      results <- traverse (compilePS port) pursBlocks

      for_ (zip results pursBlocks) $ \(r, PurescriptBlock b) -> do
        -- case Map.lookup "file" (args b)
        print "in loop"
        print (args b)
        case r of
          CFail f -> print "compilation failure" *> print f
          CSuccess s -> print "compilation success" *> print s
          ParseError e -> print "parsing error" *> print e

    _ -> print "orgcode <port> <file>"
