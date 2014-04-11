---
title: Factorie 1.0.0-M5 released
layout: default
group: news
categories: news
---

## Factorie 1.0.0-M5 released
 &raquo; by {{ site.author_sameer }} on {{ page.date | date_to_string }}

This is another milestone release for Factorie 1.0. This version comes with many core API changes, complete rewrite of the factorie.la package, reimplemented version of BP, modification to the optimization package, and so on. Detailed changelog attached.

* Executable Jar: **[factorie-1.0.0-M5.jar](http://factorie.googlecode.com/files/factorie-1.0.0-M5.jar)**
* Source files: [factorie-1.0.0-M5-src.tar.gz](http://factorie.googlecode.com/files/factorie-1.0.0-M5-src.tar.gz)

New in version 1.0.0-M5:
---

* Overall
    - Move to Scala 2.10.1
    - Migration to github
    - Better handling of Implicits
    - Hyperparameter optimization
    - support for conditional dependencies as profiles in pom
    - improved tutorials

* NLP
    - Command line interface (see README.txt)
    - Documents contain Sections
    - Support for reading models from a variety of sources (classpath, files, urls, etc.)
    - Default annotators that load models from the classpath
    - Overhauled lexicons handling
    - new annotators for mention type, gender, number, etc.
    - better support for OntoNotes and all its annotations (parsing, coreference, etc)
    - better support for ACE and relations
    - much improved parse-based mention finding
    - improvements to Tokenizers and Segmenters
    - addition of SparseLDA
    - more unification of data structures across different tasks
    - bugfixes and speed improvements

* Variables and values
    - Refactoring of Assignment

* Inference
    - support for arbitrary number of neighbors in MPLP
    - bugfixes and speed improvements

* Learning
    - regularized dual averaging (RDA) added
    - exponentiated gradient optimizer

* Serialization
    - major bugfixes and speed improvements

* Linear Algebra
    - bugfixes and major speed improvements