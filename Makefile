all: $(patsubst %.m,%.exe,$(wildcard day*.m))

day%.exe: day%.m day%
	mmc -E --use-subdirs -o $@ $<

# disable implicit rule
day%: ;

clean: FRC
	rm -f *.exe *.mh
	rm -rf Mercury

FRC:
