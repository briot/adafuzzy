all:
	gprbuild -p -j0 -m -Pdefault

clean:
	gprclean -q -r -Pdefault
