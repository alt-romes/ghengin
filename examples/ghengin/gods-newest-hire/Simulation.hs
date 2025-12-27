{-# LANGUAGE RecordWildCards #-}
module Simulation where

import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Sampler.Strict
import Control.Monad (replicateM, foldM)
import Data.List (intercalate)

-- Symptoms that can affect the planet
data Symptom
  = Heat
  | Cold
  | Famine
  | Illness
  | Plague
  | War
  | Overpopulation
  | NaturalDisaster
  | Poverty
  deriving (Show, Eq, Ord, Enum, Bounded)

-- Actions the player can take
data Action
  = MovePlanet        -- Change orbit to fix temperature
  | Meteor            -- Catastrophic population reduction
  | BroadcastVaccine  -- Cure diseases
  | Communism         -- Redistribute resources
  | RaiseSeaLevel     -- Reduce land/population capacity
  | BroadcastTech     -- Advance technology globally
  | NoAction
  deriving (Show, Eq)

-- Planet state
data PlanetState = PlanetState
  { population :: Double           -- Population in millions
  , resources :: Double            -- General resources
  , technology :: Double           -- Tech level (0-10)
  , temperature :: Double          -- -10 (cold) to +10 (hot), 0 is ideal
  , diseaseLevel :: Double         -- 0-100
  , stability :: Double            -- Political stability 0-100
  , landCapacity :: Double         -- Available habitable land
  , wealth :: Double               -- Economic wealth 0-100
  , activeSymptoms :: [Symptom]    -- Current crises
  , turn :: Int
  } deriving (Show, Eq)

-- Initial balanced state
initialPlanet :: PlanetState
initialPlanet = PlanetState
  { population = 8.0
  , resources = 100.0
  , technology = 5.0
  , temperature = 0.0
  , diseaseLevel = 20.0
  , stability = 60.0
  , landCapacity = 100.0
  , wealth = 50.0
  , activeSymptoms = []
  , turn = 0
  }

-- Check which symptoms are currently active
updateSymptoms :: PlanetState -> [Symptom]
updateSymptoms PlanetState{..} =
  [ s | (s, condition) <- conditions, condition ]
  where
    conditions =
      [ (Heat, temperature > 5)
      , (Cold, temperature < -5)
      , (Famine, resources < population * 8)
      , (Illness, diseaseLevel > 40 && diseaseLevel < 70)
      , (Plague, diseaseLevel >= 70)
      , (War, stability < 30)
      , (Overpopulation, population > landCapacity * 0.9)
      , (NaturalDisaster, landCapacity < 70)
      , (Poverty, wealth < 30)
      ]

-- Apply symptom effects to planet
applySymptomEffects :: MonadDistribution m => [Symptom] -> PlanetState -> m PlanetState
applySymptomEffects symptoms state@PlanetState{..} = do
  -- Each symptom has stochastic effects
  foldM applySingleSymptom state symptoms
  where
    applySingleSymptom s@PlanetState{..} symptom = case symptom of
      Heat -> do
        cropLoss <- uniform 0.02 0.05
        return s { resources = resources * (1 - cropLoss) }
      
      Cold -> do
        cropLoss <- uniform 0.03 0.06
        return s { resources = resources * (1 - cropLoss)
                 , wealth = wealth - 2 }
      
      Famine -> do
        starvation <- uniform 0.01 0.04
        unrest <- uniform 1 3
        return s { population = population * (1 - starvation)
                 , stability = max 0 (stability - unrest) }
      
      Illness -> do
        deaths <- uniform 0.005 0.015
        economicImpact <- uniform 1 2
        return s { population = population * (1 - deaths)
                 , wealth = max 0 (wealth - economicImpact) }
      
      Plague -> do
        deaths <- uniform 0.03 0.08
        panic <- uniform 5 15
        economicCollapse <- uniform 5 10
        return s { population = population * (1 - deaths)
                 , stability = max 0 (stability - panic)
                 , wealth = max 0 (wealth - economicCollapse) }
      
      War -> do
        casualties <- uniform 0.02 0.05
        destruction <- uniform 10 20
        resourceLoss <- uniform 0.1 0.3
        return s { population = population * (1 - casualties)
                 , landCapacity = max 50 (landCapacity - destruction)
                 , resources = resources * (1 - resourceLoss)
                 , wealth = max 0 (wealth - 15) }
      
      Overpopulation -> do
        depletion <- uniform 0.05 0.1
        pollution <- uniform 0.5 1.5
        return s { resources = resources * (1 - depletion)
                 , diseaseLevel = min 100 (diseaseLevel + pollution) }
      
      NaturalDisaster -> do
        deaths <- uniform 0.01 0.03
        infrastructureLoss <- uniform 5 10
        return s { population = population * (1 - deaths)
                 , wealth = max 0 (wealth - infrastructureLoss) }
      
      Poverty -> do
        unrest <- uniform 2 5
        healthDecline <- uniform 1 3
        return s { stability = max 0 (stability - unrest)
                 , diseaseLevel = min 100 (diseaseLevel + healthDecline) }

-- Apply a player action
applyAction :: MonadDistribution m => Action -> PlanetState -> m PlanetState
applyAction action state@PlanetState{..} = case action of
  MovePlanet -> do
    -- Move orbit to fix temperature, but risky
    success <- bernoulli 0.7
    if success
      then do
        tempChange <- uniform (-3) (-1.5)
        let newTemp = if temperature > 0 
                      then temperature + tempChange 
                      else temperature - tempChange
        return state { temperature = max (-10) (min 10 newTemp)
                     , stability = max 0 (stability - 10) }  -- Causes panic
      else do
        -- Failed move causes disaster
        catastrophe <- uniform 0.1 0.2
        return state { population = population * (1 - catastrophe)
                     , stability = max 0 (stability - 20) }
  
  Meteor -> do
    -- Massive population reduction but solves overpopulation
    impact <- uniform 0.3 0.5
    traumaticImpact <- uniform 15 25
    return state { population = population * (1 - impact)
                 , landCapacity = max 50 (landCapacity - 10)
                 , stability = max 0 (stability - traumaticImpact) }
  
  BroadcastVaccine -> do
    -- Cures disease but expensive
    effectiveness <- uniform 0.6 0.9
    return state { diseaseLevel = diseaseLevel * (1 - effectiveness)
                 , wealth = max 0 (wealth - 20)
                 , technology = technology + 0.3 }
  
  Communism -> do
    -- Redistributes wealth, affects stability
    wealthGain <- uniform 15 25
    stabilityChange <- uniform (-10) 10  -- Can help or hurt
    return state { wealth = min 100 (wealth + wealthGain)
                 , stability = max 0 (min 100 (stability + stabilityChange))
                 , resources = resources * 0.9 }  -- Less efficient
  
  RaiseSeaLevel -> do
    -- Reduces habitable land to combat overpopulation
    landLoss <- uniform 15 25
    refugeeCrisis <- uniform 5 10
    return state { landCapacity = max 40 (landCapacity - landLoss)
                 , stability = max 0 (stability - refugeeCrisis)
                 , wealth = max 0 (wealth - 10) }
  
  BroadcastTech -> do
    -- Advance technology, helps multiple issues
    techBoost <- uniform 0.8 1.5
    adoptionCost <- uniform 10 15
    return state { technology = min 10 (technology + techBoost)
                 , wealth = max 0 (wealth - adoptionCost) }
  
  NoAction -> 
    return state

-- Natural dynamics that occur each turn
naturalDynamics :: MonadDistribution m => PlanetState -> m PlanetState
naturalDynamics state@PlanetState{..} = do
  -- Population growth (logistic)
  growthNoise <- normal 1.0 0.1
  let carryingCap = landCapacity * 0.8
      growthRate = 0.02 * (1 - population / carryingCap) * (wealth / 50)
      newPop = population * (1 + growthRate * growthNoise)
  
  -- Resource production and consumption
  production <- normal (50 + technology * 5) 10
  let consumption = newPop * 10
      newResources = max 0 (resources + production - consumption)
  
  -- Temperature drift (climate change)
  tempDrift <- normal 0.0 0.3
  let newTemp = temperature + tempDrift + (newPop * 0.05) - (technology * 0.02)
  
  -- Disease dynamics
  diseaseSpread <- uniform (-2) 3
  let hygiene = technology + wealth / 20
      newDisease = max 0 $ min 100 $ diseaseLevel + diseaseSpread - hygiene / 5
  
  -- Stability recovery
  stabilityChange <- uniform (-2) 4
  let newStability = max 0 $ min 100 $ stability + stabilityChange + technology / 5
  
  -- Wealth generation
  wealthChange <- uniform (-3) 5
  let newWealth = max 0 $ min 100 $ wealth + wealthChange + technology * 0.5
  
  -- Land recovery
  landRecovery <- uniform 0 1
  let newLand = min 100 $ landCapacity + landRecovery + technology * 0.1
  
  return state { population = newPop
               , resources = newResources
               , temperature = newTemp
               , diseaseLevel = newDisease
               , stability = newStability
               , wealth = newWealth
               , landCapacity = newLand
               , technology = technology }

-- Simulate one turn with action
simulateTurn :: MonadDistribution m => Action -> PlanetState -> m PlanetState
simulateTurn action state = do
  -- Apply player action first
  afterAction <- applyAction action state
  
  -- Update active symptoms
  let symptoms = updateSymptoms afterAction
  
  -- Apply symptom effects
  afterSymptoms <- applySymptomEffects symptoms afterAction
  
  -- Natural dynamics
  afterDynamics <- naturalDynamics afterSymptoms
  
  return afterDynamics { activeSymptoms = updateSymptoms afterDynamics
                       , turn = turn afterDynamics + 1 }

-- Simulate multiple turns with action sequence
simulateGame :: MonadDistribution m => [Action] -> PlanetState -> m [PlanetState]
simulateGame actions initState = go actions initState []
  where
    go [] _ acc = return (reverse acc)
    go (a:as) state acc = do
      newState <- simulateTurn a state
      go as newState (newState : acc)

-- Check if planet survived
survived :: PlanetState -> Bool
survived PlanetState{..} = 
  population > 0.5 &&
  resources > 0 &&
  stability > 10 &&
  not (Plague `elem` activeSymptoms && War `elem` activeSymptoms)

-- Pretty print state
prettyPrint :: PlanetState -> String
prettyPrint PlanetState{..} = unlines
  [ "=== Turn " ++ show turn ++ " ==="
  , "Population: " ++ show ((population * 10) / 10) ++ "M"
  , "Resources: " ++ show (resources)
  , "Technology: " ++ show ((technology * 10) / 10)
  , "Temperature: " ++ show ((temperature * 10) / 10) ++ "°"
  , "Disease Level: " ++ show (diseaseLevel) ++ "%"
  , "Stability: " ++ show (stability) ++ "%"
  , "Land Capacity: " ++ show (landCapacity) ++ "%"
  , "Wealth: " ++ show (wealth) ++ "%"
  , "Active Symptoms: " ++ if null activeSymptoms 
                           then "None" 
                           else intercalate ", " (map show activeSymptoms)
  ]

-- Run single simulation with action sequence
runSimulation :: [Action] -> IO [PlanetState]
runSimulation actions = sampleIO $ simulateGame actions initialPlanet

-- Example usage:
main :: IO ()
main = do
  let strategy = [BroadcastTech, NoAction, NoAction, BroadcastVaccine, 
                  NoAction, Communism, NoAction, NoAction, MovePlanet]
  
  trajectory <- runSimulation strategy
  mapM_ (putStrLn . prettyPrint) trajectory
  
  putStrLn $ "\nPlanet " ++ if survived (last trajectory) 
                            then "SURVIVED!" 
                            else "COLLAPSED"
