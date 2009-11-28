# Some simple processing for source code maintenance.
# To compile FACTORIE use Maven2, by typing "mvn compile"

all:
	echo To compile FACTORIE use Maven2, "mvn compile"

# Use emacs' default indenter to fix indentation on all *.scala files in src.
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

