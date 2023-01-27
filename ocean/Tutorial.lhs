%include polycode.fmt

\chapter{Ocean Simulation Tutorial}

We'll be writing an ocean simulation game with the Ghengin game engine, a
type-heavy, shader-centric, vulkan-based, haskell game engine, hereby only
referenced by name. This book is written as a collection of TeX and literate
Haskell files.

The ocean simulation will be founded on ``A simple model of ocean waves''
published in 1986~\cite{10.1145/15886.15894}. However, before the ocean math,
we must setup the game to use Ghengin.

A Ghengin game starts by importing the engine and calling the main engine
function \href{https://hackage.haskell.org/package/ghengin/}{ghengin} from
@main@. We're making use of a couple of functions which are not yet defined but
that we'll put together below.

\begin{code}
import Ghengin

main :: IO ()
main = ghengin OWorld ini fixedUpdate update end
\end{code}

% A Ghengin game is run with the @ghengin@ function.
% \begin{code}
% ghengin
%     :: w           -- World
%     -> Ghengin w a -- Action to run on init
%     -> Ghengin w b -- Action run every simulation fixed step
%     -> (a -> DeltaTime -> Ghengin w Bool) -- Action run every game loop step
%     -> (a -> Ghengin w c) -- Action run on quit
%     -> IO ()
% \end{code}

All games need a world. A world usually comprises of all component stores
available to the entity component system (see Section~\ref{sec:ecs}), but can
be anything in addition to that since it's a user defined datatype that is
available everywhere in the engine.

In practice, the world datatype contains @Storage@s for all components we want
to support in the Entity Component System besides the built-in ones. For now,
we don't need any extra components so our world (@OWorld@) has a single
constructor that takes no arguments (i.e.  it's a type isomorphic to @()@).

Since we will ever only use one world, we also define a type alias to avoid
writing @Ghengin OWorld@ everywhere, and use it when creating the next functions.

\begin{code}
data OWorld = OWorld

type Game = Ghengin OWorld
\end{code}

The init action (@ini@) is run once at the start of the game and, similarly,
the quit action (@end@) once at the end. For now, we don't need to do anything
particularly interesting in either.

\begin{minipage}{0.47\textwidth}
\begin{code}
ini :: Game ()
ini = pure ()
\end{code}
\end{minipage}
\begin{minipage}{0.47\textwidth}
\begin{code}
end :: () -> Game ()
end () = liftIO $ putStrLn "Goodbye!"
\end{code}
\end{minipage}

The update function is called every game loop step and is where most of the
game logic should be programmed, e.g. inputs, movement, $\dots$. It takes as an
argument the value returned by the init function and the time passed since the
lasts frame, known as delta time ($\Delta t$).
%
The delta time is simply a float (@DeltaTime = Float@), but it is a very
important float in game development. In reality, frames don't happen at a fixed
rate, even more so if we compare framerate across computers.

Consider a computer $A$ which has a framerate of 60 FPS and another computer
$B$ which has a framerate of 30 FPS. Should, e.g., the player move twice as far
in one second in computer $A$ than in $B$? Surely not, in $A$, in each second
there are twice as many frames, so, the player should walk half as much each
frame as the player in $B$. In doing so, the player will end up on the same
place at the end of a second in computer $A$ and $B$. Even then, frame rates
are not consistent, and some frames take longer and some are faster than others
in the same computer.

The key idea to account for the difference in frame rates is to \emph{scale by
the duration of each frame} (i.e. scale by $\Delta t$) both movement and other
varying properties. For example, in $A$, each frame takes on average $0.01666$
seconds, and, in $B$, each frame takes on average $0.03333$ seconds.  If we
define the movement speed of the player to be $1.5m/s$ and scale it by $\Delta
t$, in computer $A$ the player will move $1.5*\Delta t = 1.5*0.01666 \approx
0.025m$ in each frame, while in $B$ the player will move $1.5*\Delta t =
1.5*0.03333 \approx 0.05$ -- the player in $A$ and $B$ are now moving at the
same time! Every frame in $B$ the player moves $0.05m$ and every two frames in
$A$ (which take the same real time as just one in $B$) the player also moves
$0.025*2 = 0.05m$. 

\begin{code}
update :: () -> DeltaTime -> Game Bool
update () dt = pure False
\end{code}

The fixed update function is called according to the fixed step
simulation loop. Fixed updates are important when simulating physics and $\dots$.
We set @undefined@ in the function body because the engine doesn't support it
just yet, and therefore doesn't call it

\begin{code}
update :: () -> DeltaTime -> Game Bool
update () dt = pure False
\end{code}

Upon running the game as is, one should see a bright magenta screen (Figure~\ref{fig:001}). You should
also see a screaming screen of errors. This makes some sense,
% (in the future we might clean it up a bit so you don't get so many errors on an empty project)
we haven't set up a render pipeline, a camera, or anything for that matter.

\begin{figure}[h]
\centering
\includegraphics[width=\textwidth]{images/001.png}
\caption{Screaming magenta\label{fig:001}}
\end{figure}

