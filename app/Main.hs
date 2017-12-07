{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Protolude hiding (decodeUtf8)

import Data.Aeson

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.IO as T.IO
import qualified Data.ByteString.Lazy as LBS

import Control.Lens
import Data.Aeson.Lens

import System.Directory (removeFile, makeAbsolute)
import System.IO (openTempFile, hClose, hFlush)
import System.Process


data CodeBlock a = CodeBlock { language :: Text
                             , args :: Map Text Text
                             , contents :: a
                             } deriving (Eq, Ord, Show, Functor)


cmpByArg :: Text -> CodeBlock a -> CodeBlock a -> Ordering
cmpByArg arg b1 b2 = Map.lookup arg (args b1) `compare` Map.lookup arg (args b2)


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


-- | Output should be a list of lists of codeblocks that can be combined, in order.
-- | Culls empty output lists.
groupOutputBlocks :: [CodeBlock a] -> [[CodeBlock a]]
groupOutputBlocks =
  let ordering = cmpByArg ":file"
  in List.groupBy (\b1 b2 -> EQ == ordering b1 b2) .
     List.sortBy ordering

-- | Assumes the blocks in the input list are equivalent according to `equivalentBlocks`;
-- | grouped according to `groupOutputBlocks`!
combineBlocks :: Monoid a => [CodeBlock a] -> Maybe (CodeBlock a)
combineBlocks (b:bs) = Just $ b { contents = foldMap contents (b:bs) }
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


data CompileStatus =
    CFail [Text]
  | CSuccess [Text]
  | ParseError [Text]
  deriving (Eq, Ord, Show)

parseCompiler :: AsValue a => a -> Maybe CompileStatus
parseCompiler val = do
  resType <- case val ^? key "resultType" . _String of
    Just "success" -> pure CSuccess
    Just "error" -> pure CFail
    _ -> Nothing

  results <- val ^? key "result" . _Array
  resType . toList <$> traverse (\x -> x ^? key "message" . _String) results


parsingFailure :: a -> CompileStatus
parsingFailure _ = ParseError ["Something went very wrong"]


parseCompileOutput :: AsValue a => a -> CompileStatus
parseCompileOutput val = fromMaybe (parsingFailure val) $ parseCompiler val


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

  case T.unpack <$> Map.lookup ":prologue" (args b) of
    Nothing -> putStrLn ("Code block lacks import" :: Text)
    Just ip -> do
      importsPath <- makeAbsolute ip
      imports <- T.IO.readFile importsPath
      T.IO.hPutStrLn h imports

  T.IO.hPutStrLn h $ T.unlines $ contents b
  hFlush h

  absPath <- makeAbsolute path
  results <- pursIdeSendCmd port $ pscRebuildCmd absPath

  hClose h
  removeFile path

  pure $ parseCompileOutput results


parseInput :: [Text] -> [PurescriptBlock]
parseInput ls = pursBlocks
  where raw :: [[Text]]
        raw = fmap snd <$> rawBlocks ls
        grouped :: [[CodeBlock [Text]]]
        grouped = groupOutputBlocks $ mapMaybe parseCodeBlock raw
        combined :: [CodeBlock [Text]]
        combined = mapMaybe combineBlocks grouped
        pursBlocks :: [PurescriptBlock]
        pursBlocks = purescriptBlocks combined

main :: IO ()
main = do

  progArgs <- getArgs

  case progArgs of
    [p, fn] -> do
      ls <- T.lines . LT.toStrict . decodeUtf8 <$> LBS.readFile fn

      port <- case readMaybe p of
        Nothing -> undefined
        Just p' -> pure p'

      let pursBlocks = parseInput ls

      results <- traverse (compilePS port) pursBlocks

      for_ (zip results pursBlocks) $ \(r, PurescriptBlock b) -> do
        print (args b)
        putStrLn ("" :: Text)

        str <- case r of
          CFail f      -> putStrLn ("Compilation failure" :: Text) *> pure f
          CSuccess s   -> putStrLn ("Compilation success" :: Text) *> pure s
          ParseError e -> putStrLn ("Parsing error" :: Text)       *> pure e

        for_ str putStrLn


    [fn] -> do
      ls <- T.lines . LT.toStrict . decodeUtf8 <$> LBS.readFile fn

      let pursBlocks = parseInput ls

      print "Purescript code blocks:"
      traverse_ (\(PurescriptBlock b) -> putStrLn (T.unlines $ contents b)) pursBlocks

    _ -> print "orgcode <port> <file>, or orgcode <file>"
