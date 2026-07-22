module R4C.Export.Markdown
  ( expandMarkdownIncludes
  , writeMarkdownIncludes
  , writeMarkdownBlock
  ) where

import Control.Monad (when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist, removeFile, renameFile)
import System.FilePath (isAbsolute, takeDirectory, (</>))

startMarker :: T.Text
startMarker = T.pack "<!-- include:start:"

endMarker :: T.Text
endMarker = T.pack "<!-- include:end -->"

expandMarkdownIncludes :: FilePath -> String -> IO String
expandMarkdownIncludes sourcePath content = do
  let lines' = lines content
  expanded <- expandLines sourcePath lines'
  pure $ unlines expanded

expandLines :: FilePath -> [String] -> IO [String]
expandLines _ [] = pure []
expandLines sourcePath (line:rest) =
  case parseStartMarker line of
    Nothing -> do
      next <- expandLines sourcePath rest
      pure (line : next)
    Just includePath -> do
      let (between, after) = break containsEndMarker rest
      inserted <- readIncludedContent sourcePath includePath
      let remaining = case after of
            [] -> []
            (_:xs) -> xs
      next <- expandLines sourcePath remaining
      let rendered = inserted ++ (case after of
            [] -> []
            (endLine:_) -> [endLine])
      pure (line : rendered ++ next)

parseStartMarker :: String -> Maybe String
parseStartMarker line =
  case T.stripPrefix startMarker (T.pack line) of
    Nothing -> Nothing
    Just rest ->
      let includePath = T.strip $ T.takeWhile (\c -> c /= '>' && c /= ' ' && c /= '\t') rest
      in if T.null includePath then Nothing else Just (T.unpack includePath)

containsEndMarker :: String -> Bool
containsEndMarker line = T.isInfixOf endMarker (T.pack line)

readIncludedContent :: FilePath -> FilePath -> IO [String]
readIncludedContent sourcePath includePath = do
  let resolvedPath = if null includePath then sourcePath else
        if isAbsolute includePath then
          includePath
        else if takeDirectory includePath == "" || takeDirectory includePath == "." then
          takeDirectory sourcePath </> includePath
        else
          includePath
  exists <- doesFileExist resolvedPath
  if exists
    then do
      included <- TIO.readFile resolvedPath
      pure (lines (T.unpack included))
    else
      pure ["<!-- include missing: " ++ includePath ++ " -->"]

writeMarkdownIncludes :: FilePath -> FilePath -> IO ()
writeMarkdownIncludes sourcePath outputPath = do
  content <- readFile sourcePath
  expanded <- expandMarkdownIncludes sourcePath content
  writeFile outputPath expanded
  pure ()

writeMarkdownBlock :: FilePath -> FilePath -> String -> FilePath -> IO ()
-- | Reads an md file and updates or appends a marked block without overwriting
--   the rest of the document.
writeMarkdownBlock sourcePath outputPath marker contentPath = do
  createDirectoryIfMissing True (takeDirectory outputPath)
  exists <- doesFileExist sourcePath
  source <- if exists then readFile sourcePath else pure ""
  contentExists <- doesFileExist contentPath
  content <- if contentExists then readFile contentPath else pure ""
  let startMarker = "<!-- include:start:" ++ marker ++ " -->"
      endMarker = "<!-- include:end -->"
      normalizedContent = trimTrailingNewlines content
      block = startMarker ++ "\n" ++ normalizedContent ++ "\n" ++ endMarker
      updated = case replaceBetweenMarkers source startMarker endMarker content of
        Just replaced -> replaced
        Nothing -> appendMarkdownBlock source block
  let tempPath = outputPath ++ ".tmp"
  tempExists <- doesFileExist tempPath
  when tempExists (removeFile tempPath)
  writeFile tempPath updated
  renameFile tempPath outputPath

appendMarkdownBlock :: String -> String -> String
appendMarkdownBlock source block =
  let separator = if null source then "" else if last source == '\n' then "" else "\n"
  in source ++ separator ++ block ++ "\n"

trimTrailingNewlines :: String -> String
trimTrailingNewlines = reverse . dropWhile (== '\n') . reverse

containsMarkerBlock :: String -> String -> String -> Bool
containsMarkerBlock source startMarker endMarker =
  isInfixOf startMarker source && isInfixOf endMarker source

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = T.isInfixOf (T.pack needle) (T.pack haystack)

replaceBetweenMarkers :: String -> String -> String -> String -> Maybe String
replaceBetweenMarkers source startMarker endMarker replacement =
  case breakOn startMarker source of
    Nothing -> Nothing
    Just (prefix, rest) ->
      case breakOn endMarker rest of
        Nothing -> Nothing
        Just (_, suffix) -> Just (prefix ++ startMarker ++ "\n" ++ replacement ++ "\n" ++ endMarker ++ suffix)

breakOn :: String -> String -> Maybe (String, String)
breakOn needle haystack =
  case T.breakOn (T.pack needle) (T.pack haystack) of
    (prefix, rest) ->
      if T.null rest then Nothing else Just (T.unpack prefix, T.unpack (T.drop (T.length (T.pack needle)) rest))
