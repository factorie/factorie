---
title: Factorie 1.1 released
layout: default
group: news
categories: news
---

## Factorie 1.1 released
 &raquo; by {{ site.author_luke }} on {{ page.date | date_to_string }}

This is the official 1.1 release of Factorie. This version includes an upgrade to Scala 2.11 and deployment to Sonatype, many improvements to the NLP pipeline, new models, learning algorithms, performance improvements and bug fixes. Detailed changelog attached.

* Executable Jar (2.11): **[factorie_2.11-1.1.jar](https://github.com/factorie/factorie/releases/download/factorie_2.11-1.1/factorie_2.11-1.1.jar)**
* Source files (2.11): [factorie_2.11-1.1-sources.jar](https://github.com/factorie/factorie/releases/download/factorie_2.11-1.1/factorie_2.11-1.1-sources.jar)
* Executable Jar (2.10): **[factorie_2.10-1.1.jar](https://github.com/factorie/factorie/releases/download/factorie_2.10-1.1/factorie_2.10-1.1.jar)**
* Source files (2.10): [factorie_2.10-1.1-sources.jar](https://github.com/factorie/factorie/releases/download/factorie_2.10-1.1/factorie_2.10-1.1-sources.jar)


New in version 1.1
---

* Overall
    - Factorie is now on Sonatype
    - Factorie has switched to Scala 2.11
    - Many miscellaneous improvements and fixes

* NLP
    - Many improvements to hierarchical coref
    - Large improvements and fixes to within-doc coref features
    - Chinese POS tagger
    - New word embeddings package
    - New surface-form based relation finder
    - Improved Document APIs
    - Improved mention finding
    - Hierarchical coref demo
    - Date phrase finder
    - Faster data structures for lexicons
    - Bug fixes to mention finding and tagging
    - Bug fixes to tokenizer
    - Fixes to IOB/BILOU boundaries
    - More efficient parse trees
    - Improved storage of Documents in Mongo
    - Speed improvements to CategoricalDomain
    - Improved pronoun noun phrase labeling

* Linear Algebra
    - Fix to some corner case bugs in sparse/singleton tensors
    - Various utility methods such as fill, Hadamard product, l2 projections

* Learning
    - Added FTRLProximal online regularized learning algorithm
    - Fix to switch hyperparams in AdaGradRDA
    - Added implementation of Group Lasso
    - Fixes to gradient testing code

* Miscellaneous
    - Alias sampling implementation for e.g. efficient generation of negative training data
    - Improvements to hyperparameter optimization