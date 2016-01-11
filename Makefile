all:
	gprbuild -p -j0 -m -Pdefault

clean:
	gprclean -q -r -Pdefault

robot:
	cd examples; ../obj/robot
