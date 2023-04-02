module Day19.FirstSpec (spec) where

import Day19.First
import Test.Hspec

spec :: Spec
spec = do
  blueprints <- parseInput <$> runIO (readFile "data/Day19/testInput")
  let blueprint = head blueprints

  describe "Parsing" $ do
    it "parses without errors" $ do
      length blueprints `shouldBe` 2
      blueprint `shouldBe`
        [[4, 0, 0, 0], [2, 0, 0, 0], [3, 14, 0, 0], [2, 0, 7, 0]]

  describe "Solution" $ do
    it "increments the value in a list at a certain index" $ do
      incrementAt 0 [0] `shouldBe` [1]
      incrementAt 1 [0, 1, 2] `shouldBe` [0, 2, 2]

    it "calculates the required time to reach a certain amount of material" $ do
      requiredTime 10 1 0 `shouldBe` Just 10
      requiredTime 10 2 0 `shouldBe` Just 5
      requiredTime 10 3 1 `shouldBe` Just 3
      requiredTime 10 3 0 `shouldBe` Just 4
      requiredTime 10 0 0 `shouldBe` Nothing
      requiredTime 10 1 11 `shouldBe` Just 0

    it "evolves a certain state for a given amount of time" $ do
      evolve 0 initialState `shouldBe` initialState

      let duration = 5
      evolve duration initialState `shouldBe`
        State
          { resources = [duration, 0, 0, 0]
          , robots = [1, 0, 0, 0]
          , elapsedTime = duration
          }

    it "fast forwards until a specified robot is built" $ do
      let finalState = spawnRobot blueprint 0 initialState 4
      resources finalState `shouldBe` [1, 0, 0, 0]
      robots finalState `shouldBe` [2, 0, 0, 0]
      elapsedTime finalState `shouldBe` 5

    it "determines whether there are too many robots of any kind in a given state" $ do
      let maxOreCost = maximum (map head blueprint)
      let overkillState =
            State
              { resources = undefined
              , robots = [maxOreCost + 1, 0, 0, 0]
              , elapsedTime = undefined
              }
      initialState `shouldNotSatisfy` isOverkill blueprint
      overkillState `shouldSatisfy` isOverkill blueprint

    describe "generates the states that follow a given state" $ do
      let state =
            State
              { resources = [0, 0, 0, 0]
              , robots = [1, 1, 1, 1]
              , elapsedTime = 0
              }

      it "happy case" $ do
        let next = nextStates blueprint 30 state
        let expected =
              [ State
                  { resources = [1, 5, 5, 5]
                  , robots = [2, 1, 1, 1]
                  , elapsedTime = 5
                  }
              , State
                  { resources = [1, 3, 3, 3]
                  , robots = [1, 2, 1, 1]
                  , elapsedTime = 3
                  }
              , State
                  { resources = [12, 1, 15, 15]
                  , robots = [1, 1, 2, 1]
                  , elapsedTime = 15
                  }
              , State
                  { resources = [6, 8, 1, 8]
                  , robots = [1, 1, 1, 2]
                  , elapsedTime = 8
                  }
              ]
        next `shouldBe` expected

      it "discards steps that go over the time limit" $ do
        let next = nextStates blueprint 10 state
        length next `shouldBe` 3

      it "discards unbuildable robots" $ do
        let state' = state {robots = [1, 0, 0, 0]}
        let next = nextStates blueprint 30 state'
        length next `shouldBe` 2

      it "discards building redundant robots" $ do
        let maxOreCost = maximum (map head blueprint)
        let state' = state {robots = [maxOreCost, 0, 0, 0]}
        let next = nextStates blueprint 30 state'
        length next `shouldBe` 1

    it "calculates the maximum amount of geode that can be produced with a given blueprint" $ do
      findMaximumGeode 24 initialState (head blueprints) `shouldBe` 9
      findMaximumGeode 24 initialState (last blueprints) `shouldBe` 12

    it "calculates the total quality level of the blueprints" $ do
      totalQualityLevel 24 blueprints `shouldBe` 33
