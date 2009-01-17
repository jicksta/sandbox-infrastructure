all:    subdirs

subdirs: 
	mkdir ebin
	cd log4erl; make
	cd src; make

# remove all the code
clean:
	cd log4erl; make clean
	rm -rf ebin/*.beam erl_crash.dump
	rm -f *~
	rm -f src/*~
	rm -f ebin/*~


#       cd dir1; make clean
#install:
#        cp -f ebin/* ../../www/ebin
