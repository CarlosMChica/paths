module Main where

import           Lib
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

instance Arbitrary Point where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Point x y

instance Arbitrary Segment where
  arbitrary = do
    x <- arbitrary
    x' <- arbitrary
    return $ Segment x x'

main :: IO ()
main = hspec $ do

  describe "Paths search" $ do

--    prop "calculates segment distance correctly" $ do
--        \seg@(Segment (Point x y) (Point x' y')) -> ((segmentDistance seg)^2 == fromIntegral ((x' - x)^2) + (fromIntegral (y' - y)^2))

    it "find the shortest path from a path list" $ do
      let path  = [Segment (Point (-1) 5) (Point 2 0)]
      let path1 = [Segment (Point (-1) 0) (Point 1 0)]
      let path2 = [Segment (Point 4 2) (Point 1 0)]
      let paths = [path, path1, path2]

      shortestPath paths `shouldBe` path1

    it "find the path with the least number of stops from a path list" $ do
      let path  = [
                    Segment (Point (-1) 5) (Point 2 0),
                    Segment (Point 3 5) (Point 3 0)
                  ]
      let path1 = [Segment (Point (-1) 0) (Point 1 0)]
      let paths = [path, path1]

      leastStopsPath paths `shouldBe` path1

    it "find the paths stopping at given points" $ do
      let path  = [
                    Segment (Point 0 0) (Point 2 0),
                    Segment (Point 1 0) (Point (-1) 3)
                  ]
      let path1 = [Segment (Point (-1) 0) (Point 1 0)]
      let paths = [path, path1]

      pathsStoppingAt [Point 0 0, Point 1 0, Point (-1) 3] paths `shouldBe` [path]

    it "find the shortest path that includes all given stops" $ do
      let path  = [
                    Segment (Point 0 0) (Point 2 0),
                    Segment (Point 1 0) (Point (-1) 0)
                  ]
      let path1 = [Segment (Point (-1) 0) (Point 1 0)]
      let path2 = [Segment (Point 2 0) (Point 1 0)]
      let paths = [path, path1, path2]

      shortestPathStoppingAt (Point (-1) 0) paths `shouldBe` path1

