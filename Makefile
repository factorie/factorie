# Some simple processing for source code maintenance.
# To compile FACTORIE use Maven2, by typing "mvn compile"

default:
	@echo To compile FACTORIE use Maven2, "mvn compile"


strip-first-comment:
	#perl -0777 -i -p -e 's,^/\*.*Copyright[^\*]*\*/\n,,sm' `find src -name '*.java'`
	perl -0777 -i -p -e 's,^/\*.*Copyright[^\*]*\*/\n,,sm' `find src -name '*.scala'`

update-license-year:
	perl -0777 -i -p -e 's,2008-2014,2008-2014,sm' `find src -name '*.java'`
	perl -0777 -i -p -e 's,2008-2014,2008-2014,sm' `find src -name '*.scala'`

prepend-license-comment:
	for f in `find -E src -regex '.*\.(java|scala)'` ; do \
	  mv $$f $$f.orig ; \
	  cat doc/LICENSE-HEADER.txt $$f.orig > $$f ; \
	  rm $$f.orig ; \
	done
