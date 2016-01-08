Ada Fuzzy
=========

This package provides a library to create fuzzy inference systems
in Ada.

For more information on fuzzy inference, see

https://en.wikipedia.org/wiki/Fuzzy_control_system
and
http://fr.mathworks.com/help/fuzzy/fuzzy-inference-process.html

Basically, you can give the system rules like:

   if temperature is COLD then power is FULL
   if temperature is MEDIUM then power is MEDIUM
   if temperature is HOT then power is LOW

then provide a temperature, and let the system compute what the
output should be.
You will need a bit more setup, though, to define what the terms
COLD, MEDIUM, HOT, FULL and LOW mean.

See the example/example.adb as a starting point.
