---
title: Factorie 0.10.2 released
layout: default
group: news
categories: news
---

## Factorie 0.10.2 released
 &raquo; by {{ site.author_sameer }} on {{ page.date | date_to_string }}

This release comes with a number of enhancements to the inference techniques, a developed NLP package, a flexible persistence layer (Cubbie), and a novel hierarchical model. Detailed changelog attached.

* Executable Jar: **[factorie-0.10.2.jar](http://factorie.googlecode.com/files/factorie-0.10.2.jar)**
* Source files: [factorie-0.10.2-src.tar.gz](http://factorie.googlecode.com/files/factorie-0.10.2-src.tar.gz)

## New in version 0.10.2:


* NLP
	- Customized forward-backward and viterbi for chain models
	- changes to the coreference data structures that support hierarchical models
	- new data loaders
	- models can be loaded from JARs (POS model in IESL Nexus)
	- initial dependency parser

* BP
	- Refactoring to be faster and cleaner interface, with bugfixes
	- Caching of scores and values
	- MaxProduct works even when multiple MAP states
	- TimingBP to compare performance of the different variants of BP in the codebase
	- maxMarginal with threshold, to support PR curves
	- some initial parallelization

* Max likelihood training
	- convenience constructors for selecting which families to update
	- pieces can use families for inference that are not updated

* Trainer that uses Stochastic gradient descent

* Cubbie
	- new united interface for serialization/persistence (including mongodb support)

* Hierarchical Coref Model
	- added model that supports arbitrarily deep and wide hierarchy of entites, aka Wick, Singh, McCallum, ACL 2012

* Gzip saving/loading of models
* Data loaders for bibtex, dblp, etc.
* Better support for limitedValues and sparse domains on factors
* Code cleanup, including deletion of inner/outer factors


