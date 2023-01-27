%include polycode.fmt

\chapter{Ocean Simulation Tutorial}

We'll be writing an ocean simulation game with the Ghengin game engine, a
type-heavy, shader-centric, vulkan-based, haskell game engine, hereby only
referenced by name. This book is written as a collection of literate Haskell
files.

The ocean simulation will be founded on ``A simple model of ocean waves''
published in 1986~\cite{10.1145/15886.15894}. However, before the ocean math,
we must setup the game to use Ghengin.

A Ghengin game is run with the @ghengin@ function.
\begin{code}
ghengin
    :: w           -- World
    -> Ghengin w a -- Action to run on init
    -> Ghengin w b -- Action run every simulation fixed step
    -> (a -> DeltaTime -> Ghengin w Bool) -- Action run every game loop step
    -> (a -> Ghengin w c) -- Action run on quit
    -> IO ()
\end{code}

The world is used to define the component stores available in the entity
component system (see Section~\ref{sec:ecs}). The init action is run once at
the start of the game and the quit action once at the end. Finally, the fixed
step simulation is called according to the fixed step simulation loop and the
game loop step function is run once every game loop step.

The world datatype contains @Storage@s for all entity components we want to
support besides the built-in ones. For now, we don't need any extra components
so our world (@OWorld@) has a single constructor that takes no arguments (i.e.
it's a type isomorphic to @()@).

Since we will ever only use one world, we also define a type alias to avoid
writing @Ghengin OWorld@ everywhere, and use it when creating the functions

\begin{code}

import Ghengin

-- | The Ocean World with required component storages
data OWorld = OWorld

-- | The game engine parametrized over our ocean world
type Game = Ghengin OWorld

-- | The function run when the game is initialized
ini :: Game ()
ini = pure ()

-- | The function run when the game is closed
end :: () -> Game ()
end () = liftIO $ putStrLn "Goodbye!"

-- | The function run every game loop step.
-- The returned boolean indicates whether to stop the game
update :: () -> DeltaTime -> Game Bool
update () dt = pure False

\end{code}

We put these all together by calling @ghengin@ in @main@. The fixed update
function is @undefined@ because the engine doesn't support it just yet.

\begin{code}

main :: IO ()
main = ghengin OWorld ini undefined update end

\end{code}

Upon running the 

