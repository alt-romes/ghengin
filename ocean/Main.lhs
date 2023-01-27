%include polycode.fmt

\chapter{Ocean Simulation: Source}

Contrary to the tutorial, this chapter explains the code as it is written in
the final version instead of building up to it.

\begin{code}
module Main where

import Ghengin

-- | The Ocean World with required component storages
data OWorld = OWorld

-- | The game engine parametrized over our ocean world
type Game = Ghengin OWorld

-- | The function run when the game is initialized
ini :: Game ()
ini = do
  pure ()

-- | The function run when the game is closed
end :: () -> Game ()
end () = liftIO $ putStrLn "Goodbye!"

-- | The function run every game loop step.
-- The returned boolean indicates whether to stop the game
update :: () -> DeltaTime -> Game Bool
update () dt = pure False

main :: IO ()
main = ghengin OWorld ini undefined update end

\end{code}

