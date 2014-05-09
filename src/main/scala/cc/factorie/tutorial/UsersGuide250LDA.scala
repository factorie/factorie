/*& Topic Modeling form the Command-line */
/*&

### Topic Modeling

FACTORIE comes with a pre-built implementation of the [latent Dirichlet allocation (LDA)](https://en.wikipedia.org/wiki/Latent_Dirichlet_allocation) topic model. 
If "mytextdir" is a directory name containing many plain text documents each in its own file, then typing 

```
$ bin/fac lda --read-dirs mytextdir --num-topics 20 --num-iterations 100
```

will run 100 iterations of a sparse collapsed Gibbs sampling on all the documents, and print out the results every 10 iterations. 
FACTORIE's LDA implementation is faster than [MALLET](http://mallet.cs.umass.edu)'s.

*/
