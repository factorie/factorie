---
title: Factorie 1.0.0-M3 released
layout: default
group: news
categories: news
---

## Factorie 1.0.0-M3 released
 &raquo; by {{ site.author_sameer }} on {{ page.date | date_to_string }}

This is the first milestone release for Factorie 1.0. This version comes with many core API changes, complete rewrite of the factorie.la package, reimplemented version of BP, modification to the optimization package, and so on. Detailed changelog attached.

* Executable Jar: **[factorie-1.0.0-M3.jar](http://factorie.googlecode.com/files/factorie-1.0.0-M3.jar)**
* Source files: [factorie-1.0.0-M3-src.tar.gz](http://factorie.googlecode.com/files/factorie-1.0.0-M3-src.tar.gz)


New in version 1.0.0-M3:
---

* Documentation
	- improved existing tutorials
	- new tutorial on Inference and Learning
	- better TUI
	- better comments and error messages
	- Parser Demo
	- site can be generated at the users' end

* Models and Templates
	- support for feature hashing
	- Massive renaming of Variables and Domains

* NLP
	- Classifier based POS tagger
	- added port of ClearNLP tokenizer/segmenter
	- Faster Bibtex parser
	- REST API for Parsers

* Inference
	- support efficient inference for ChainModels
	- Sampler can return a DiffList of all the changes
	- bugfixes in MHSampler
	- BP logZ implemented to enable likelihood learning

* Optimization and Training
	- Removed redundant SampleRank
	- Added Pegasos. Pseudo-likelihood, Contrastive Divergence, StructSVM, AdaGrad
	- new ClassifierTrainer to support all types of losses, trainers and optimizers
	- better multi-threaded support
	- bugfixes and efficiency improvements

* Tensors
	- speed enhacements and bugfixes
	- more operations implemented
	- new tests for Tensors

* Serialization
	- all new serialization based on Cubbies
