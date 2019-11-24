.PHONY : clean hindent run test

HINDENT = hindent --line-length 76 --sort-imports

run :
	stack build
	-rm -rf target-directory
	stack exec framer-exe -- target-directory
	find target-directory
	cd target-directory && \
	    stack build && \
	    stack exec sample-exe && \
	    stack test

hindent :
	find app src test -name '*.hs' -exec $(HINDENT) \{} \;

clean : # hindent
	stack clean
	-rm -rf target-directory
	find . -name '*~' -delete
	find . -name '#*' -delete

