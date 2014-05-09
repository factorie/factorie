/*& NLP on the Command-line */
/*&
### NLP on the Command-line

If you also have the Maven-supplied factorie-nlp-resources JAR in your classpath, you can run many natural language processing tools.  For example,

```
$ bin/fac nlp --wsj-forward-pos --transition-based-parser --conll-chain-ner
```

will launch an NLP server that will perform part-of-speech tagging, dependency parsing and named entity recognition on its input.  
The server listens for text on a socket, and spawns a parallel document processor on each request.  
To feed it input, type in a separate shell

```
$ echo "Mr. Jones took a job at Google in New York.  He and his Australian wife moved from New South Wales on 4/1/12." | nc localhost 3228
```

which then produces the output:

```
1       1       Mr.             NNP     2       nn      O
2       2       Jones           NNP     3       nsubj   U-PER
3       3       took            VBD     0       root    O
4       4       a               DT      5       det     O
5       5       job             NN      3       dobj    O
6       6       at              IN      3       prep    O
7       7       Google          NNP     6       pobj    U-ORG
8       8       in              IN      7       prep    O
9       9       New             NNP     10      nn      B-LOC
10      10      York            NNP     8       pobj    L-LOC
11      11      .               .       3       punct   O

12      1       He              PRP     6       nsubj   O
13      2       and             CC      1       cc      O
14      3       his             PRP$    5       poss    O
15      4       Australian      JJ      5       amod    U-MISC
16      5       wife            NN      6       nsubj   O
17      6       moved           VBD     0       root    O
18      7       from            IN      6       prep    O
19      8       New             NNP     9       nn      B-LOC
20      9       South           NNP     10      nn      I-LOC
21      10      Wales           NNP     7       pobj    L-LOC
22      11      on              IN      6       prep    O
23      12      4/1/12          NNP     11      pobj    O
24      13      .               .       6       punct   O
```

*/
