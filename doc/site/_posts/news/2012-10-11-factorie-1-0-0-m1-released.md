---
title: Factorie 1.0.0-M1 released
layout: default
group: news
categories: news
---

## Factorie 1.0.0-M1 released
 &raquo; by {{ site.author_sameer }} on {{ page.date | date_to_string }}

This is the first milestone release for Factorie 1.0. This version comes with many core API changes, complete rewrite of the factorie.la package, reimplemented version of BP, modification to the optimization package, and so on. Detailed changelog attached.

* Executable Jar: **[factorie-1.0.0-M1.jar](http://factorie.googlecode.com/files/factorie-1.0.0-M1.jar)**
* Source files: [factorie-1.0.0-M1-src.tar.gz](http://factorie.googlecode.com/files/factorie-1.0.0-M1-src.tar.gz)

## New in version 1.0.0-M1:


* Models and Templates
	- All templates are now Models
	- Models are now parameterized by the type of things they can score
	- It is possible to write code that does not deduplicate factors

* NLP
	- new Ontonotes Loader
	- new Nonprojective Dependency parser

* Inference
	- Summary class now maintains the marginals, and is common to Samplers and BP
	- Reimplementation of BP to be more efficient

* Optimization & Training
	- efficient L2-regularized SVM training
	- integration with app.classify
	- support for parallel batch and online training with a Piece API
	- support for Hogwild (including Hogwild SampleRank)

* Tensors
	- all new la package that replaces the earlier Vector classes with Tensors
	- Tensors can be multi-dimensional, with implementations that independently choose sparsity/singleton for each dimension
	- weights and features now use Tensors

* Serialization
	- Serialization exists in a different class

* Misc
	- Added Tutorials to walkthrough model construction
	- Cleaned examples so that they work (added a test that makes sure they do)
