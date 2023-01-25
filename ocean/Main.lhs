\documentclass[a4paper,twocolumn,openright]{memoir}

\title{Ocean Simulation with Ghengin}
\author{Rodrigo Mesquita}

%%%%%%%%%%%%%%  Color-related things   %%%%%%%%%%%%%%

\usepackage[dvipsnames]{xcolor}

%include polycode.fmt

%subst keyword a = "\textcolor{BlueViolet}{\textbf{" a "}}"

\newcommand{\id}[1]{\textsf{\textsl{#1}}}

\renewcommand{\Varid}[1]{\textcolor{Sepia}{\id{#1}}}
\renewcommand{\Conid}[1]{\textcolor{OliveGreen}{\id{#1}}}

%%%%%%%%%%%%  End of Color-related things   %%%%%%%%%%%%

% It might make sence to add pretty formating of individual things
% like "forall", cf.
% https://github.com/goldfirere/thesis/blob/master/tex/rae.fmt

\begin{document}
\maketitle

This post is written as a literate Haskell file.

We'll be writing a game with the Ghengin game engine, a type-heavy, shader-centric, vulkan-based, haskell game engine.

\begin{code}
module Main where

main :: IO ()
main = do
  pure ()
\end{code}

\end{document}
