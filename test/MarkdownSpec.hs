module MarkdownSpec (tests) where

import qualified System.Directory as Dir
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

import R4C.Export.Markdown

tests :: TestTree
tests =
  testGroup
    "Markdown includes"
    [ testCase "expands include markers from a sibling file" $ do
        withSystemTempDirectory "r4c-markdown" $ \tmpDir -> do
          let sourcePath = tmpDir </> "book.md"
              includePath = tmpDir </> "table.md"
          writeFile includePath "| A |\n|---|\n| 1 |\n"
          writeFile sourcePath "Before\n<!-- include:start:table.md -->\n<!-- include:end -->\nAfter\n"

          expanded <- expandMarkdownIncludes sourcePath =<< readFile sourcePath

          expanded @?= "Before\n<!-- include:start:table.md -->\n| A |\n|---|\n| 1 |\n<!-- include:end -->\nAfter\n"
    , testCase "rewrites the markdown file on repeated runs" $ do
        withSystemTempDirectory "r4c-markdown" $ \tmpDir -> do
          let sourcePath = tmpDir </> "book.md"
              includePath = tmpDir </> "table.md"
              outputPath = tmpDir </> "book.out.md"
          writeFile includePath "| A |\n|---|\n| 1 |\n"
          writeFile sourcePath "Before\n<!-- include:start:table.md -->\n<!-- include:end -->\nAfter\n"

          writeMarkdownIncludes sourcePath outputPath
          firstRun <- readFile outputPath
          firstRun @?= "Before\n<!-- include:start:table.md -->\n| A |\n|---|\n| 1 |\n<!-- include:end -->\nAfter\n"

          writeFile includePath "| B |\n|---|\n| 2 |\n"
          writeMarkdownIncludes sourcePath outputPath
          secondRun <- readFile outputPath
          secondRun @?= "Before\n<!-- include:start:table.md -->\n| B |\n|---|\n| 2 |\n<!-- include:end -->\nAfter\n"
    , testCase "appends a marked block at the end without overwriting existing prose" $ do
        withSystemTempDirectory "r4c-markdown" $ \tmpDir -> do
          let sourcePath = tmpDir </> "book.md"
              outputPath = tmpDir </> "book.out.md"
              contentPath = tmpDir </> "table.md"
          writeFile sourcePath "Existing prose"
          writeFile contentPath "| A |\n|---|\n| 1 |\n"

          writeMarkdownBlock sourcePath outputPath "tab1" contentPath

          actual <- readFile outputPath
          actual @?= "Existing prose\n<!-- include:start:tab1 -->\n| A |\n|---|\n| 1 |\n<!-- include:end -->\n"
    ]
