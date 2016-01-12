all:
	gprbuild -p -j0 -m -Pexamples/examples.gpr

	# The following requires GtkAda, but is an optional example
	gprbuild -p -j0 -m -Pexamples/guiexamples.gpr

clean:
	gprclean -q -r -Pdefault

robot:
	cd examples; obj/robot
