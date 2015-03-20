# umwelt
Elm framework for dealing with state maintaining nodes

This is pre-alpha anything. This framework allows for extending events (node state updates),
(event start/start), collision detection, drag & drop, drag handling, and anything else I
thought of at the time to a network of nodes (packets of state including a Graphics.Collage.Form).
It basically started as an approach to drawing a form element element onto the canvas and being
able to interact with it later.

It's pretty rococo; I was throwing code against a wall to see what stuck while attempting to
implement each new interaction I wanted. No work has been put into cleaning any false starts,
removing debug tooling, project orginization, or performace considerations.
The usual caveats (emptor) apply. It is however Elm 0.14 code (ick..that update was annoying).

In fact it might be rather painful to dig into as I haven't put any work into documentation either,
but you can play with it and see if it does anything you like. Feel free to think of it as a parts
bin. If there are pieces that are useful to you, go ahead and pull then into a seperate library and
publish it if you like (possibles for that might be the indexed array module IxArray, or the
collision detection module GJK which implements the Gilbert-Johnson-Keerthi collision detection
algorithm. Atrribution is always nice though :))

This isn't abandonware, it's just I've been sitting on it for a while, it's in an almost first
pass complete state, and I'm setting it aside for a while to recharge/go figure out how to PhoneGap/Cordova
this stuff, and play with purescript for a bit. The plan is to eventually layer some arrowized interface
on top of a core that may or may not look like this code in the end.
Feel free to send ideas/critiques. 

You should be able to just install elm 0.14, clone into a directory, and run elm-reactor in it.
Move the objects around. Notice the collision detection. drag and release the blue dot. Drop a node on
the green dot. drop something on it again. Wheeee!
