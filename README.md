Ada Fuzzy
=========

This package provides a library to create fuzzy inference systems
in Ada.

For more information on fuzzy inference, see

https://en.wikipedia.org/wiki/Fuzzy_control_system
and
http://fr.mathworks.com/help/fuzzy/fuzzy-inference-process.html

Compiling
==========

To compile this library, you need to install the GNAT compiler
and the GNAT Components Library (gnatcoll) first.

Then type::

   make

Demos
=====

Thermostat
----------

See `examples/example.adb` for a simple demo that shows how
to create a fuzzy inference engine programmatically.
This demo simulates a thermostat that tries to smoothly maintain
the temperature around 20 C, while at period interval we randomly
nudge it.

Basically, we give the system rules like:

   if temperature is COLD then power is FULL
   if temperature is MEDIUM then power is MEDIUM
   if temperature is HOT then power is LOW

then provide a temperature, and let the system compute what the
output should be.
We need a bit more setup, though, to define what the terms
COLD, MEDIUM, HOT, FULL and LOW mean.

Type::

    make
    ./examples/obj/example

to compile and run this demo

Robot
-----

Another demo is provided. The goal is to control a robot so that
it reaches its target while avoiding obstacles. The rules are
described in an external file `examples/robot.fcl`, which
you can modify before restarting the demo.

This file is written in the Fuzzy Controller Language, for which
you can find a description at
http://www.fuzzytech.com/binaries/ieccd1.pdf

This demo requires GtkAda on your system.

Type::

     make all robot

to compile and run this demo.
