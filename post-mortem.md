---
title: My weekend with Elm
authors: Gil
tags: gamedev, functional, frp, elm
---

[Ludum Dare][ld] is an online game jam competition/event that takes place every 4 months.
Each competition starts when it's theme is announced and each participant has 48/72 hours
for compo/jam entry respectively to complete and publish a game based on the theme.

This weekend [LD32][ld32] took place, and I decided to participate while using [Elm][] to build my game - [Banana Bandit][mygame] (source on [github][]).

Edit: [post-jam updates][updates]

Since I didn't have a lot of Elm experience (I wrote at most 100 LOC in it) prior to jam,
I decided that my goal is to learn Elm, FRP and a new way to model games on the fly, during the jam,
and gain real experience working with it,
while somewhat sacrificing the quality of the end result (the game).

In this post I will try and describe my reflection of the weekend, what went wrong, what went well,
and what could have been done better.

---

Starting the work was pretty easy, I was able to install Elm-platform pretty easily,

- There are [many examples][exmps] on the main Elm website that can be run and changes online
- There is an [Elm architecture][archtutor] that explains how to start modeling applications in Elm
- There is a [Game Skeleton][game-skeleton] which I used as a template
- Elm compiles to JavaScript, means it will work on the web - a huge win for LD in my opinion
- There is a vim syntax-highlighting plugin
- elm-reactor saves the trouble of compiling and running every time
- I was able to start modeling and look at the changes to the model over time without drawing anything
- Graphics were _really_ easy to add, change and replace. From simple rectangle place holders to images and animations.

Problem is, since I started from scratch, I had to implement all the elements and logic of a game,
game objects with position, velocity, state, actions, movement logic with acceleration and deceleration,
collision detection, timers, interactions and so on. All those features that comes today with every
game engine were very time consuming and were proving tricky to implement. Which is why such things
need to be done prior to a jam. But I learned a lot implementing all of this things.

### A lot bugs related to logic and arithmetics mistakes started to appear, and were very time consuming to identify and fix:

- Timers changed too quickly since because I was accidentally working with milliseconds instead of seconds
- Movement and acceleration did bizarre things due to incorrect calculations
- AI bugs due to calculation mistakes
- Collision problems
- [Infinite recursion][infrec] by accident that stuck the game
- Maze generator logic bugs

And more.


Elm-reactor proved valuable in some of these bugs. Being able to watch values changing over time,
stop and rewind really helped tackle some of these bugs. If I could also look at the code at certain points when pausing
it would have been even better, much better.
Some mistakes were probably due to sloppiness on my part, the old "if it type checks, it works" saying at the back of my head
might have made me less careful on my variable naming and such. Maybe that infinite recursion could have been avoided.


Second set of problems: working with Elm was not without problems.

- Only syntax highlighting plugin for Vim. I needed to run the game through elm-reactor to see parsing/typing/etc bugs.
- At times Elm felt still an immature platform to work on. Many parsing bugs were cryptic, not so informative and were sometimes not placed close to the actual error.
- I was not able to hot-swap, restart, or unpaused with Elm-reactor.
- There was a [problem][mem-commit] that when trying to compile, elm-make would have or through an "out of memory" error. I really didn't know how I solved that
- Elm 0.15 was announced a few hours before I finished and a package I used no longer appeared on [package.elm-lang.org](http://package.elm-lang.org) so I had to look at the code for documentation.


### Things I didn't have enough time to do:

- Animations
- Integrating audio (didn't find the [elm-audio][] package on package.elm-lang.org) though I recorded some music and sfx.
- Fixing the maze generator -> many levels
- Score, lives and such
- More levels
- Screens

### Conclusion

I had fun, I learned a lot, and I was really happy to finally using functional programming for games.
Even though I got stuck a lot of times, have had to chase and solve many bugs due to faulty logic
and didn't __really__ finish the game, I was a very eye-opening experience and I felt like I gained a lot from it.

Elm is still a little rough around the edges and so it is not so easy to work with, at the moment,
during a 72 hour game jam, from scratch, with minimal prior knowledge on it,
But it is also very young. I have a feeling that if it keeps in that direction - work hard on better tools like elm-reactor,
work on the eco-system and packages, fix bugs and such, for another 3 years, it will become
an incredible programming language and system to work with.

The next time I participate on Ludum Dare, it will most likely be with Elm. This time with better understanding of it
and hopefully, a much better result :)

Thanks for reading!


[ld]: http://ludumdare.com/compo/
[ld32]: http://ludumdare.com/compo/ludum-dare-32/?action=preview
[exmps]: http://elm-lang.org/Examples.elm
[Elm]: http://elm-lang.org/
[archtutor]: https://github.com/evancz/elm-architecture-tutorial#the-elm-architecture
[game-skeleton]: https://github.com/elm-lang/elm-lang.org/blob/258181d16df10d7b51721f1f5005baca03c8b7e7/frontend/public/examples/Intermediate/Bounce.elm
[infrec]: https://github.com/soupi/ld32/blob/c1c6fbb591dc27620935ce7216b7e3c6a9776141/src/Game/Game.elm#L86
[mem-commit]: https://github.com/soupi/ld32/tree/80b80e76124691639a99af23dad4c84b49479d09
[elm-audio]: https://github.com/jcollard/elm-audio
[mygame]: http://ludumdare.com/compo/ludum-dare-32/?action=preview&uid=29243
[github]: https://github.com/soupi/ld32
[updates]: http://www.gilmi.xyz/static/misc/gamejams/ld32/Game/dist/update.html
