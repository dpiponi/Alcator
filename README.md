# Alcator

An Acorn Atom emulator.

(This is actually forked from my Atari 2600 emulator.)

You'll need some ROMs.
You can get them from [here](http://www.acornatom.nl/atom_handleidingen/aw123/acorn_roms.htm).
Unpack them so the roms appear in `acorn_roms` in the top directory.
We'll be needing `Atom_Kernel.rom`, `Atom_Basic.rom` and `Atom_FloatingPoint.rom` in particular.

To run you'll need to install `stack` from [here](.)
Then use:

    stack build
    stack exec alcator

The code uses `glfw`. You may need to install `homebrew` and then run

    brew install glfw3

Screenshots
-----------
![Centipede screenshot](docs/centipede.gif?raw=true "Centipede screenshot")
![Frogger screenshot](docs/frogger.gif?raw=true "Frogger screenshot")
![Atomcalc screenshot](docs/atomcalc.gif?raw=true "Atomcalc screenshot")
