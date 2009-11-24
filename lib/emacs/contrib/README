In Emacs, this file should be read in -*- Outline -*- mode.

* Contribution

** Exuberant Ctags 

To let ctags know how to parse scala files, put the content of
dot-ctags into your $HOME/.ctags file. This will parse scala files and
give the following kinds.

Scala
    c  classes 
    o  objects 
    t  traits 
    m  case-classes 
    a  abstract-classes 
    f  functions 
    V  values 
    v  variables 
    T  types 

The default in the scala mode is to parse scala files for all the
above kinds to produce tags. This can give a rather huge amount of
tags in speedbar for a scala file. This can be reduced by adding 

--Scala-kinds=[+|-]kinds

where kinds are the one letter abbrevs above.
