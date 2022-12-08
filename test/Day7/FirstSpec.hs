module Day7.FirstSpec (spec) where

import Day7.First
import Test.Hspec

spec :: Spec
spec = do
  testInput <- runIO (readFile "data/Day7/testInput")
  let root = parseInput testInput

  describe "Parsing" $ do
    it "parses without errors" $ do
      files root `shouldBe` [File "b.txt" 14848514, File "c.dat" 8504156]
      files (head (directories root)) `shouldBe`
        [File "f" 29116, File "g" 2557, File "h.lst" 62596]

      map name (directories root) `shouldBe` ["a", "d"]
      map (map filename . files) (directories root) `shouldBe`
        [["f", "g", "h.lst"], ["j", "d.log", "d.ext", "k"]]
      map (map name . directories) (directories root) `shouldBe`
        [["e"], []]

  describe "Solution" $ do
    it "finds all directories" $ do
      map name (allDirectories root) `shouldBe` ["/", "a", "e", "d"]

    it "finds all directories below a certain size" $ do
      map name (findSmallDirectories 100000 root) `shouldBe` ["a", "e"]

    it "finds the total size of all such directories" $ do
      totalSmallDirectoriesSize 100000 root `shouldBe` 95437
