%include polycode.fmt

\chapter{Ocean Simulation Tutorial}

We'll be writing an ocean simulation game with the Ghengin game engine, a
type-heavy, shader-centric, vulkan-based, haskell game engine, hereby only
referenced by name. This book is written as a collection of literate Haskell
files.

The ocean simulation will be founded on ``A simple model of ocean waves''
published in 1986~\cite{10.1145/15886.15894}. However, before the ocean math,
we must setup the game to use Ghengin.

\begin{code}
module Main where

main :: IO ()
main = do
    pure ()

\end{code}

