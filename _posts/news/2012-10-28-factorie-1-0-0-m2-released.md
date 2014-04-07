---
title: Factorie 1.0.0-M2 released
layout: default
group: news
categories: news
---

## Factorie 1.0.0-M2 released
 &raquo; by {{ site.author_sameer }} on {{ page.date | date_to_string }}

This is the first milestone release for Factorie 1.0. This version comes with many core API changes, complete rewrite of the factorie.la package, reimplemented version of BP, modification to the optimization package, and so on. Detailed changelog attached.

* Executable Jar: **[factorie-1.0.0-M2.jar](http://factorie.googlecode.com/files/factorie-1.0.0-M2.jar)**
* Source files: [factorie-1.0.0-M2-src.tar.gz](http://factorie.googlecode.com/files/factorie-1.0.0-M2-src.tar.gz)


## New in version 1.0.0-M2:


* Documentation
	- markdown based website, the source for which is checked into the repository
	- Tutorial on Domains
	- more assertions throughout the code (including tutorials)
	- better Tutorial prettifier

* Models and Templates
	- Factors can provide statistics and scores on any Assignment and valueTensors
	- trait Model independent of context, ModelWithContext[C] can unroll given any context

* NLP

* Inference
	- BPSummary is more efficient, includes an abstract version

* Optimization and Training
	- Pieces are now Examples, Learners are Trainers
	- MaxlikelihoodExample is efficient in computing constraints
	- SampleRankExample replaces old trainer, almost as efficient

* Tensors
	- Filled in more of the missing cases in Tensors
	- Fixed indexing bugs in a few Tensor types
	- OuterTensors that efficiently represent the outer product between Tensors

* Serialization
	- gzip support
