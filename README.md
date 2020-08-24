# Haskell Risk Game

A simple Risk Game implemented in Haskell. The game is minimalistic but it is
fully playable (currently limited to two human players).

For fullscreen mode, pass -f or --fullscreen to the program - e.g.
`stack run -- --fullscreen`.

# Building

Can be built easily via [stack](http://haskellstack.org/): `stack build
&& stack run`.

You will need the proper dependencies installed on your system - sdl2
and sdl2-ttf.

# Licensing

All source code is MIT licensed, and the image assets are licensed under
[Creative Commons ShareAlike
3.0](https://creativecommons.org/licenses/by-sa/3.0/). The background image is
modified from Wikipedia.
