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
component system (see Section~\ref{sec:ecs}). The init action will be run once
at the start of the game, for now it does nothing.

\begin{code}
import Ghengin

main :: IO ()
main = do
    pure ()

\end{code}

