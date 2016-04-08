---
title: Factorie 1.2 released
layout: default
group: news
categories: news
---

## Factorie 1.2 released
 &raquo; by {{ site.author_emma }} on {{ page.date | date_to_string }}

This is the official 1.2 release of Factorie. This version includes major rewrites to the NLP pipeline as well as many various bugfixes and improvements. Detailed changelog attached.

* Executable Jar (2.11): **[factorie_2.11-1.2.jar](https://github.com/factorie/factorie/releases/download/factorie_2.11-1.2/factorie_2.11-1.2.jar)**
* Source files (2.11): [factorie_2.11-1.2-sources.jar](https://github.com/factorie/factorie/releases/download/factorie_2.11-1.2/factorie_2.11-1.2-sources.jar)


New in version 1.2
---

* Overall
    - Major rewrite of NLP pipeline
    - Many miscellaneous improvements and fixes

* NLP
    - Many performance and speed improvments improvements to NER
    - Rewritten, ~2x faster dependency parser
    - Rewritten hierarchical cross-document coreference
    - Rewritten universal schema relation extraction model and epistemological database
    - Faster tokenization with JFlex (50x faster, ~500k tokens/second on modest machine)
    - Improvements to model, lexicon, and other resource finding and loading
    - New name parser

* Linear Algebra
    - Several bug fixes to sparse tensors

* Learning
    - Support for simple constraints in optimization (projected gradient)
    - Support for different constraints, regularization, and optimizers over different feature template subsets
    - Added support for grid-searching subsets when optimizing hyperparameters
    - Bug fixes to Exponentiated Gradient

* Inference
    - LiteChainModel class for simple chain CRFs

* Miscellaneous
    - Improved unit- and integration-testing coverage
    - Improved serialization

