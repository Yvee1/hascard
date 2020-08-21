import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import DeckHandling
import Parser
import States
import Recents

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testRecents
  , testChunking
  ]

testRecents :: TestTree
testRecents = testGroup "Shortening filepaths"
  [ testCase "empty recents" $
      shortenFilepaths [] @?= []
  , testRecentsExtension ".txt"
  , testRecentsExtension ".md"
  , testRecentsMix
  ]

testRecentsExtension :: String -> TestTree
testRecentsExtension ext = testGroup ("Extension " <> ext)
  [ testCase "different file names" $
      shortenFilepaths ["some/path/deck1" <> ext, "some/other/path/deck2" <> ext, "some/new/path/deck3" <> ext]
      @?=
      ["deck1", "deck2", "deck3"]

  , testCase "recents same file name" $
      shortenFilepaths ["/path/to/deck" <> ext, "/path/to/another/deck" <> ext, "other/path/normal" <> ext]
      @?=
      ["to/deck", "another/deck", "normal"] 

  , testCase "recents same directory and file name" $
      shortenFilepaths ["/some/directory/deck" <> ext, "/another/directory/deck" <> ext, "other/path/normal" <> ext]
      @?=
      ["some/directory/deck", "another/directory/deck", "normal"]
  ]

testRecentsMix :: TestTree
testRecentsMix = testGroup "Mixed extensions"
  [ testCase "different file names" $
      shortenFilepaths ["some/path/deck1.txt", "some/other/path/deck2.txt", "some/new/path/deck3.md"]
      @?=
      ["deck1.txt", "deck2.txt", "deck3.md"]

  , testCase "recents same file name, same extension" $
      shortenFilepaths ["/path/to/deck.txt", "/path/to/another/deck.txt", "other/path/normal.md"]
      @?=
      ["to/deck.txt", "another/deck.txt", "normal.md"]

  , testCase "recents same file name, different extension" $
      shortenFilepaths ["/path/to/deck.md", "/path/to/another/deck.txt", "other/path/normal.md"]
      @?=
      ["deck.md", "deck.txt", "normal.md"] 

  , testCase "recents same directory and file name, same extension" $
      shortenFilepaths ["/some/directory/deck.md", "/another/directory/deck.md", "other/path/normal.txt"]
      @?=
      ["some/directory/deck.md", "another/directory/deck.md", "normal.txt"]

  , testCase "recents same directory and file name, different extension" $
      shortenFilepaths ["/some/directory/deck.txt", "/another/directory/deck.md", "other/path/normal.txt"]
      @?=
      ["deck.txt", "deck.md", "normal.txt"]
  ]

testChunking :: TestTree
testChunking = testGroup "QuickCheck"
  [ testProperty "concat . doChunking n == id" $
      \xs n -> n > 0 ==> concat (splitIntoNChunks n xs) == (xs :: [Int])
  ]