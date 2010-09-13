# Some simple processing for source code maintenance.
# To compile FACTORIE use Maven2, by typing "mvn compile"

default:
	echo To compile FACTORIE use Maven2, "mvn compile"

# CLASSPATH
CP=`echo $$HOME/workspace/factorie/target $$HOME/workspace/lib/* $$CLASSPATH | sed -e 's/ /:/g'`
fsc:
	mkdir -p target/classes
	javac -d target/classes `find ./src -name '*.java' -print`
	JAVA_OPTS="-Xmx1800M -Xms256M -Xss16M" $(SCALA_HOME)/bin/fsc -deprecation -unchecked -cp $(CP) -d target/classes `find src -name '*.scala'` `find src -name '*.java'`

scalac:
	mkdir -p target/classes
	javac -d target/classes `find ./src -name '*.java' -print`
	JAVA_OPTS="-Xmx1800M -Xms256M -Xss16M" $(SCALA_HOME)/bin/scalac -Xmx2048m -deprecation -unchecked -cp $(CP) -d target/classes `find src -name '*.scala'` `find src -name '*.java'`

lda:
	JAVA_OPTS="-Xmx1024M" $(SCALA_HOME)/bin/scala -cp target/classes cc.factorie.example.LDADemo
clda:
	JAVA_OPTS="-Xmx1024M" $(SCALA_HOME)/bin/scala -cp target/classes cc.factorie.example.ClusterLDADemo
hmm:
	JAVA_OPTS="-Xmx1024M" $(SCALA_HOME)/bin/scala -cp target/classes cc.factorie.example.HMMDemo
coref:
	JAVA_OPTS="-Xmx1024M" $(SCALA_HOME)/bin/scala -cp target/classes cc.factorie.example.CorefMentionsDemo
seg:
	JAVA_OPTS="-Xmx1024M" $(SCALA_HOME)/bin/scala -cp target/classes cc.factorie.example.WordSegmenterDemo
chainner3:
	JAVA_OPTS="-Xmx1024M" $(SCALA_HOME)/bin/scala -cp target/classes cc.factorie.example.ChainNER3 ~/research/data/ie/ner2003/eng.train ~/research/data/ie/ner2003/eng.testa

example:
	JAVA_OPTS="-Xmx1024M" $(SCALA_HOME)/bin/scala -cp target/classes cc.factorie.example.$(E)

# Use emacs' default indenter to fix indentation on all *.scala files in src.
# Not yet working!
# TODO: This could be made more efficient by having emacs loop through the files instead of the shell
fixindent:
	for f in `find src -name '*.scala' -print` ; do \
	  /usr/bin/emacs -q -batch \
	    -eval "(prefer-coding-system 'utf-8)" \
	    -eval "(add-to-list 'load-path \"lib/emacs\")" \
	    -eval "(require 'scala-mode-auto)" \
	    $$f \
	    -eval '(indent-region (point-min) (point-max))' \
	    -f save-buffer ; \
	done

# Indentation works when interactive...
fixindenttest:
	for f in `find src/main/scala/cc/factorie/model -name '*.scala' -print` ; do \
	  /usr/bin/emacs -q \
	    -eval "(add-to-list 'load-path \"/Users/mccallum/workspace/factorie/lib/emacs\")" \
	    -eval "(require 'scala-mode-auto)" \
	    $$f \
	    -eval '(scala-mode)' \
	    -eval '(goto-char (point-min))' \
	    -eval '(scala-indent-line)' \
	    -eval '(next-line)' \
	    -eval '(scala-indent-line)' \
	    -eval '(indent-region (point-min) (point-max) nil)' \
	    -eval '(goto-char (point-min))' \
	    -eval '(defun indent-all () (interactive) (indent-region (point-min) (point-max) nil))' \
	    -eval "(global-set-key (kbd \"C-d\") 'indent-all)" \
	    -eval '(indent-all)' \
	  ; \
	done

# ... but not here.  What is going on?
fixindenttest2:
	for f in `find src/main/scala/cc/factorie/model -name '*.scala' -print` ; do \
	  /usr/bin/emacs -q -batch \
	    -eval "(add-to-list 'load-path \"/Users/mccallum/workspace/factorie/lib/emacs\")" \
	    -eval "(require 'scala-mode-auto)" \
	    -eval '(load "/Users/mccallum/workspace/factorie/lib/emacs/scala-mode-auto")' \
	    $$f \
	    -eval '(scala-mode)' \
	    -eval '(indent-region (point-min) (point-max) nil)' \
	    -eval '(goto-char (point-min))' \
	    -eval '(insert "foo")' \
	    -f save-buffer ; \
	done

untabify:
	for f in `find src/main/scala -name '*.scala' -print` ; do \
	  /usr/bin/emacs -q -batch \
	    $$f \
	    -eval "(setq indent-tabs-mode nil)" \
	    -eval "(setq tab-width 2)" \
	    -eval '(untabify (point-min) (point-max))' \
	    -f save-buffer \
	  ; \
	done



cleanfiles:
	find -E src -regex '.*(~|\.orig)' -print

clean:
	find -E src -regex '.*(~|\.orig)' -exec rm {} \;

strip-first-comment:
	perl -0777 -i.orig -p -e 's,^/\*[^\*]*\*/,,sm' `find src -name '*.java'`
	perl -0777 -i.orig -p -e 's,^/\*[^\*]*\*/,,sm' `find src -name '*.scala'`

update-license-year:
	perl -0777 -i.orig -p -e 's,2008-2009,2008-2010,sm' `find src -name '*.java'`
	perl -0777 -i.orig -p -e 's,2008-2009,2008-2010,sm' `find src -name '*.scala'`

prepend-license-comment:
	for f in `find -E src -regex '.*\.(java|scala)'` ; do \
	  mv $$f $$f.orig ; \
	  cat doc/LICENSE-HEADER.txt $$f.orig > $$f ; \
	done
