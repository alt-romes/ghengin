module Ghengin.Scene where

{-

Note [Scene Graph]
~~~~~~~~~~~~~~~~~~

A scene graph is a data structure that organizes the renderable elements of a
scene. It serves many different purposes regarding organization and performance
when rendering the scene.

When rendering, we must take into consideration information regarding the
objects being drawn to do it as efficiently as possible:

(1) Discard large parts of the world that are nowhere near the camera before rendering.
(2) Sort the render packets in an order that guarantees the maximum
    number of objects with the same material are rendered sequentially, so we can
    avoid binding and rebinding the same materials (which results in unnecessary
    work/thrashing?)
(3) Possibly sort the objects in a front to back order to avoid work in
    the fragment shader stage since the fragments with the highest depth value will
    be rendered first.
(4) Organizes the transform hierarchy and makes it possible to create a
    model to world matrix from the model transform defined relatively to the
    parent, i.e. taking into account the relative position of each element
    above in the hierarchy


A scene graph's primary purpose is to organize nodes hierarchically, mainly to
solve (4) and pass down information across nodes

It might be possible to an interface to a structure that manages all 4
individually but under the same abstraction?

I was initially confused and somewhat stuck because I was trying to unify all ideas
into a single graph structure, but after reading http://lspiroengine.com/?p=566 I realized it isn't so.

In L Spiro Engine a scene graph is just to manage node hierarchy, which is the
same idea behind most university level explanations of scene graphs.

In Game Engine Architecture, it is said that a scene graph is the structure
that organizes objects spatially and solves (1).

All in all, we might have separate abstractions to solve the different
problems, or we might tie them behind a single abstraction but keep the
implementation simple by composition rather than trying to do all at the same
                                                              time.

 -}


