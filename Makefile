# Some simple processing for source code maintenance.
# To compile FACTORIE use Maven2, by typing "mvn compile"

all:
	echo To compile FACTORIE use Maven2, "mvn compile"

# Use emacs' default indenter to fix indentation on all *.scala files in src.
# TODO: This could be made more efficient by having emacs loop through the files instead of the shell
fixindent:
	for f in `find src -name '*.scala' -print` ; do \
	  /usr/bin/emacs -batch $$f \
	    -eval "(prefer-coding-system 'utf-8)" \
	    -eval "(add-to-list 'load-path \"lib/emacs\")" \
	    -eval "(require 'scala-mode-auto)" \
	    -eval '(indent-region (point-min) (point-max))' \
	    -f save-buffer ; \
	done
