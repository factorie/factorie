/*& Classification from the Command-line */
/*&

### Document Classification

You can easily train a document classifier from the command line using FACTORIE. 
If "sportsdir" and "politicsdir" are each directories that contain plan text files in the categories sports and politics, then typing

```
$ bin/fac classify --read-text-dirs sportsdir politicsdir --write-classifier mymodel.factorie
```

will train a log-linear classifier by maximum likelihood (same as maximum entropy) and save it in the file "mymodel.factorie".

*/
