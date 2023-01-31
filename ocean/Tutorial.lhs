%include polycode.fmt
%format (QUOTE x) = "\mathop{}''" x

% ^ use the | to represent a tick ' in the code

\chapter{Ocean Simulation Tutorial}

We'll be writing an ocean simulation game with the Ghengin game engine, a
type-heavy, shader-centric, vulkan-based, haskell game engine, hereby only
referenced by name. This book is written as a collection of TeX and literate
Haskell files.

The ocean wave simulation will be based on ``A simple model of ocean waves''
published in 1986~\cite{10.1145/15886.15894}, ``Simulating Ocean Water'' (the
famous Tessendorf waves)~\cite{tessendorfocean} and
\cite{10.1145/2791261.2791267}.  However, before the ocean math, we must setup
the game to use Ghengin.

\section{Setup Ghengin}

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

Finally, the return value of the update function is a @Bool@. This boolean
indicates whether the game should quit. Usually this will be @False@, but,
e.g., an in-game button to quit the game could set the return value to @True@
and in that case the game would quit when the button was pressed.

For now, we don't do anything and always return @False@, that is, the game is
never quit by an action in game (though it can still be closed using the
window's built-in close button)

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


\section{Drawing a plane}

To create an ocean simulation we need a flat plane to define the ocean surface.
An ocean surface is defined by a plane with a number $N$ of equally spaced
particles. We will create all of these particles in their \emph{rest position},
that is, the position they would be in if the ocean was completely flat and
there were no waves. Later on, based on wave math from ``A simple model of
ocean waves''~\cite{10.1145/15886.15894}, every ocean particle's position will
be shifted from its rest position according to time, wind and possibly other
factors.

\subsection{Mesh}

To render an ocean plane we need to create a mesh (see Section~\ref{sec:meshes}
on meshes) that represents one. To create the flat ocean plane we generate $N$
equally spaced \emph{vertices} (see Section~\ref{sec:verts} on vertices). For
now, a vertex is only defined by its position in the world. To make matters
easier, we create type synonyms both for a @Position@ property (which is really
just a @Vec3@), and for the type of @Particle@ vertices (which are vertices
with one property: a position)

\begin{code}
type Position = Vec3
type Particle = Vertex [Position]
\end{code}

Then, we define @particles@, that takes as input the number of particles to
create in a line and generates the list of vertices that represents the flat
ocean (where $y=0$ always).
\begin{code}
particles :: Int -> (QUOTE [Particle])
particles n = [ Sin (vec3 x 0 z) | x <- [1..nf] , z <- [1..nf] ]
  where nf = fromIntegral n :: Float
\end{code}


Finally, we create a mesh from the particle vertices. To create a mesh we use
@createMesh@ which returns a @ParticleMesh@ which is synonym with @Mesh
'[Position]@ in the @Game@ monad. The type of the mesh entails that constituent
vertices have one property which is a @Position@.

\begin{code}
type ParticleMesh = Mesh (QUOTE [Position])

oceanMesh :: Int -> Game ParticleMesh
oceanMesh = createMesh . particles
\end{code}

\subsection{Material}

A mesh defines the form of an entity, but by itself it cannot describe how
something should be rendered. To render an entity, we need a @RenderPacket@. A
render packet is the base unit of renderable entities and each render packet is
formed by a \emph{mesh}, a \emph{material}, and a \emph{render pipeline} which
is compatible with that mesh and material. Section~\ref{sec:renderpacket}
describes render packets in detail.

To get started, we create a material (Section \ref{sec:material}) that simply
defines a base color for all vertices in the mesh associated with that material
in a render packet. The type of the material is parametrized by its properties
which, in this case, are only its color. We define the following synonyms:

\begin{code}
type Color = Vec3
type OceanMaterial = Material (QUOTE [Color])
\end{code}

To create the material we will use the @material@ function as described in
Section~\ref{sec:material}, specifying that the color property is a static
binding which should only be written to explicitly, rather than every frame (in
contrast, a @DynamicBinding@ is written every frame). The @material@ function
takes as an argument a @RenderPipeline@ whose creation we delegate to the
caller of the @oceanMat@ function. Unfortunately, @material@ is still coupled
to the Vulkan render backend, so we must lift the @Renderer@ computation into
@Ghengin@ using @lift@.

\begin{code}
oceanMat :: RenderPipeline -> Color -> Game OceanMaterial
oceanMat rp col = lift $ material (StaticBinding col) rp
\end{code}

Note that the @RenderPipeline@ argument has an underscore in front of it. We'll
see that a @RenderPipeline@ is parametrized by its render information at the
type-level. However, the pipeline info is a complex and large type that
basically describes the whole pipeline, so we do not want to write it out
completely, and the underscore allows us to do just that. The underscore is
called a type hole and makes @oceanMat@ have a so-called partial type signature
which is possible because of the extension @PartialTypeSignatures@. If you
further want to disable warnings regarding partial type signatures you might
add the following lines to the top of your module:

\begin{code}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS\_GHC -Wno-partial-type-signatures #-}
\end{code}

\subsection{Render Pipeline}

To create a render pipeline one must first know and think about how game
engines render entities. A render pipeline is one of the most important
concepts in game development with Ghengin (and with any engine really, despite
it often being out of sight until one reaches a high intermediate level). An in
depth explanation of rendering and render pipelines is given in
Section~\ref{sec:rendering} and Section~\ref{sec:renderpipeline}.

In this section, we focus on implementing a render pipeline for our ocean
simulation. The key idea is that every vertex in our ocean mesh will be
processed by a \emph{vertex shader} that may use the properties specified in
the material associated with the ocean mesh, said vertex shader should displace
the vertex according to a wave function that determines the displacement as a
function on the current time and on the vertex position, and, then, every
fragment in the triangles defined by the vertices is processed by a
\emph{fragment shader} that will color the ocean according to the wave height,
lighting, base hue, and other properties.

With that in mind, let's start by creating a new module @Ocean.Shader@. In this
module we will create both our vertex and fragment shader, and define the
shader pipeline. All shaders (for now?) must be written using the @fir@ shader
language. We need the following extensions to write @fir@ shaders:

\begin{code}

\end{code}


