default: lib

lib:
	( cd src; $(MAKE); cd - )

clean:
	( cd src; $(MAKE) clean; cd - )
