{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
import           Control.Lens.Fold
import           Control.Lens.Getter
import           Control.Monad.Catch.Pure
import           Control.Monad.Identity
import           Control.Monad.Trans.Resource

import           Data.Conduit
import           Data.Conduit.Combinators     as Conduit hiding (length, map,
                                                          null)
import           Data.String
import           Data.Tree
import           Data.Version

import           Paths_opml_conduit

import qualified Language.Haskell.HLint       as HLint (hlint)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Text.OPML.Arbitrary          ()
import           Text.OPML.Stream.Parse
import           Text.OPML.Stream.Render
import           Text.OPML.Types
import           Text.XML.Stream.Parse        as XML


main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ unitTests
  , properties
  , hlint
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ categoriesCase
  , directoryCase
  , placesCase
  , scriptCase
  , statesCase
  , subscriptionsCase
  ]

properties :: TestTree
properties = testGroup "Properties"
  [ inverseHeadProperty
  , inverseProperty
  ]


hlint :: TestTree
hlint = testCase "HLint check" $ do
  result <- HLint.hlint [ "test", "Text" ]
  null result @?= True


categoriesCase :: TestTree
categoriesCase = testCase "Parse categories list" $ do
  dataFile <- fromString <$> getDataFileName "data/category.opml"
  result <- runResourceT . runConduit $ sourceFile dataFile =$= XML.parseBytes def =$= force "Invalid OPML" parseOpml

  (result ^. opmlVersion_) @?= Version [2,0] []
  (result ^. head_ . opmlTitle_) @?= "Illustrating the category attribute"
  show (result ^. head_ . opmlCreated_) @?= "Just 2005-10-31 19:23:00 UTC"
  length (result ^.. outlines_ . traverse . traverse . _OpmlOutlineGeneric) @?= 1
  map (map length .levels) (result ^. outlines_) @?= [[1]]


directoryCase :: TestTree
directoryCase = testCase "Parse directory tree" $ do
  dataFile <- fromString <$> getDataFileName "data/directory.opml"
  result <- runResourceT . runConduit $ sourceFile dataFile =$= XML.parseBytes def =$= force "Invalid OPML" parseOpml

  (result ^. opmlVersion_) @?= Version [2,0] []
  (result ^. head_ . opmlTitle_) @?= "scriptingNewsDirectory.opml"
  show (result ^. head_ . opmlCreated_) @?= "Just 2005-10-13 15:34:07 UTC"
  show (result ^. head_ . modified_) @?= "Just 2005-10-25 21:33:57 UTC"
  (result ^. head_ . ownerName_) @?= "Dave Winer"
  (result ^. head_ . ownerEmail_) @?= "dwiner@yahoo.com"
  (result ^. head_ . expansionState_) @?= []
  (result ^. head_ . vertScrollState_) @?= Just 1
  (result ^. head_ . window_) @?= [(Top',105), (Left',466), (Bottom',386), (Right',964)]
  length (result ^.. outlines_ . traverse . traverse . _OpmlOutlineLink) @?= 8
  map (map length .levels) (result ^. outlines_) @?= [[1], [1], [1], [1], [1], [1], [1], [1]]


placesCase :: TestTree
placesCase = testCase "Parse places list" $ do
  dataFile <- fromString <$> getDataFileName "data/placesLived.opml"
  result <- runResourceT . runConduit $ sourceFile dataFile =$= XML.parseBytes def =$= force "Invalid OPML" parseOpml

  (result ^. opmlVersion_) @?= Version [2,0] []
  (result ^. head_ . opmlTitle_) @?= "placesLived.opml"
  show (result ^. head_ . opmlCreated_) @?= "Just 2006-02-27 12:09:48 UTC"
  show (result ^. head_ . modified_) @?= "Just 2006-02-27 12:11:44 UTC"
  (result ^. head_ . ownerName_) @?= "Dave Winer"
  show (result ^. head_ . ownerId_) @?= "Just http://www.opml.org/profiles/sendMail?usernum=1"
  (result ^. head_ . expansionState_) @?= [1,2,5,10,13,15]
  (result ^. head_ . vertScrollState_) @?= Just 1
  (result ^. head_ . window_) @?= [(Top',242), (Left',329), (Bottom',665), (Right',547)]
  length (result ^.. outlines_ . traverse . traverse . _OpmlOutlineGeneric) @?= 18
  length (result ^.. outlines_ . traverse . traverse . _OpmlOutlineLink) @?= 1
  map (map length .levels) (result ^. outlines_) @?= [[1,6,12]]


scriptCase :: TestTree
scriptCase = testCase "Parse script" $ do
  dataFile <- fromString <$> getDataFileName "data/simpleScript.opml"
  result <- runResourceT . runConduit $ sourceFile dataFile =$= XML.parseBytes def =$= force "Invalid OPML" parseOpml

  (result ^. opmlVersion_) @?= Version [2,0] []
  (result ^. head_ . opmlTitle_) @?= "workspace.userlandsamples.doSomeUpstreaming"
  show (result ^. head_ . opmlCreated_) @?= "Just 2002-02-11 22:48:02 UTC"
  show (result ^. head_ . modified_) @?= "Just 2005-10-30 03:30:17 UTC"
  (result ^. head_ . ownerName_) @?= "Dave Winer"
  (result ^. head_ . ownerEmail_) @?= "dwiner@yahoo.com"
  (result ^. head_ . expansionState_) @?= [1, 2, 4]
  (result ^. head_ . vertScrollState_) @?= Just 1
  (result ^. head_ . window_) @?= [(Top', 74), (Left', 41), (Bottom', 314), (Right', 475)]
  length (result ^.. outlines_ . traverse . traverse . _OpmlOutlineGeneric) @?= 11
  map (map length .levels) (result ^. outlines_) @?= [[1,2,2], [1,2], [1], [1,1]]


statesCase :: TestTree
statesCase = testCase "Parse states list" $ do
  dataFile <- fromString <$> getDataFileName "data/states.opml"
  result <- runResourceT . runConduit $ sourceFile dataFile =$= XML.parseBytes def =$= force "Invalid OPML" parseOpml

  (result ^. opmlVersion_) @?= Version [2,0] []
  (result ^. head_ . opmlTitle_) @?= "states.opml"
  show (result ^. head_ . opmlCreated_) @?= "Just 2005-03-15 16:35:45 UTC"
  show (result ^. head_ . modified_) @?= "Just 2005-07-14 23:41:05 UTC"
  (result ^. head_ . ownerName_) @?= "Dave Winer"
  (result ^. head_ . ownerEmail_) @?= "dave@scripting.com"
  (result ^. head_ . expansionState_) @?= [1, 6, 13, 16, 18, 20]
  (result ^. head_ . vertScrollState_) @?= Just 1
  (result ^. head_ . window_) @?= [(Top', 106), (Left', 106), (Bottom', 558), (Right', 479)]
  length (result ^.. outlines_ . traverse . traverse . _OpmlOutlineGeneric) @?= 63
  map (map length .levels) (result ^. outlines_) @?= [[1, 8, 50, 4]]


subscriptionsCase :: TestTree
subscriptionsCase = testCase "Parse subscriptions list" $ do
  dataFile <- fromString <$> getDataFileName "data/subscriptionList.opml"
  result <- runResourceT . runConduit $ sourceFile dataFile =$= XML.parseBytes def =$= force "Invalid OPML" parseOpml

  (result ^. opmlVersion_) @?= Version [2,0] []
  (result ^. head_ . opmlTitle_) @?= "mySubscriptions.opml"
  show (result ^. head_ . opmlCreated_) @?= "Just 2005-06-18 12:11:52 UTC"
  show (result ^. head_ . modified_) @?= "Just 2005-08-02 21:42:48 UTC"
  (result ^. head_ . ownerName_) @?= "Dave Winer"
  (result ^. head_ . ownerEmail_) @?= "dave@scripting.com"
  (result ^. head_ . vertScrollState_) @?= Just 1
  (result ^. head_ . window_) @?= [(Top', 61), (Left', 304), (Bottom', 562), (Right',842)]
  length (result ^.. outlines_ . traverse . traverse . _OpmlOutlineSubscription) @?= 13

inverseHeadProperty :: TestTree
inverseHeadProperty = testProperty "parse . render = id (on OpmlHead)" $ \opmlHead -> either (const False) (opmlHead ==) (runIdentity . runCatchT . runConduit $ renderOpmlHead opmlHead =$= force "Invalid OPML head" parseOpmlHead)

inverseProperty :: TestTree
inverseProperty = testProperty "parse . render = id" $ \opml -> either (const False) (opml ==) (runIdentity . runCatchT . runConduit $ renderOpml opml =$= force "Invalid OPML" parseOpml)
