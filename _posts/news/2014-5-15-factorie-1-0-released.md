---
title: Factorie 1.0 released
layout: default
group: news
categories: news
---

## Factorie 1.0 released
 &raquo; by {{ site.author_luke }} on {{ page.date | date_to_string }}

This is the official 1.0 release of Factorie. This version includes performance improvements, new NLP components, refactoring of the NLP pipeline, fixes, and code cleanup. Detailed changelog attached.

* Executable Jar: **[factorie-1.0.jar](https://github.com/factorie/factorie/releases/download/factorie-1.0/factorie-1.0.jar)**
* Source files: [factorie-1.0-sources.jar](https://github.com/factorie/factorie/releases/download/factorie-1.0/factorie-1.0-sources.jar)

New in version 1.0
---

* Overall
    - Miscellaneous code cleanup (launchers, app)
    - Expanded tutorials and documentation, including a new User's Guide and javadoc
    - Performance improvements to hashmaps (such as domains and lexicons)
    - Many miscellaneous bug fixes and improvements
    - Compatibility with Scala 2.11 (optional)

* NLP
    - Removed old hcoref package and added rewritten hierarchical coreference system in the xcoref package
    - Removed old relation package
    - Added high performance word embedding trainer based on Google's word vectors
    - New Chinese word segmenter
    - Rewritten dependency parser
    - New within-document coreference system
    - Added CRF based mention finding
    - Rewrite of Phrases/Mentions
    - Usability improvements to command line NLP tool
    - Performance improvement to Sparse LDA

* Classifiers
    - Added smoothed hinge loss with adjustable margin, costs, and smoothing
    - Fixed bug in scaled hinge loss

* Learning
    - Fix to (Parallel)BatchTrainer to use maxIterations

* Inference
    - Refactored ChainModel to make fast inference and marginals more available for posterior regularization, etc