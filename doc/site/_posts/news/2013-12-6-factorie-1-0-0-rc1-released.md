---
title: Factorie 1.0.0-RC1 released
layout: default
group: news
categories: news
---

## Factorie 1.0.0-RC1 released
 &raquo; by {{ site.author_luke }} on {{ page.date | date_to_string }}

This is the first release candidate for Factorie 1.0. This version includes many performance improvements, fixes, simplification of APIs, and reorganization of packages. Detailed changelog attached.

* Executable Jar: **[factorie-1.0.0-RC1.jar](https://github.com/factorie/factorie/releases/download/factorie-1.0.0-RC1/factorie-1.0.0-RC1.jar)**
* Source files: [factorie-1.0.0-RC1-sources.jar](https://github.com/factorie/factorie/releases/download/factorie-1.0.0-RC1/factorie-1.0.0-RC1-sources.jar)

New in version 1.0.0-RC1
---

* Overall
    - Improved tutorials and documentation
    - Switched many classifiers and factors to score using left-multiplication which gives >3x speedup in many cases
    - Refactored usage of Var/Value type members, making Assignments nice to use, among other things
    - Moved many files into separate subpackages, new "Factorie" object provides default imports
    - Added automated performance testing of various models
    - Simplified labeled variables by removing several varieties

* NLP
    - Renaming of many NLP components
    - Simplified Spans with no self-types
    - Refactored Spans and Tags
    - New Phrase classes that generalizes mentions
    - Performance improvements for parsing
    - Fixes and accuracy improvements for POS tagging
    - Fixes, refactoring, improvements for mention finding
    - Fixes to mention entity type prediction
    - Fixes to tokenizer, coreference
    - Fixes and improved features / accuracy for NER
    - Cleanup of ACE and Ontonotes loaders
    - Efficiency improvements and fixes to app.chain command line tool

* Classifiers
    - New app.classify.backend package with enhanced and simplified support for GLMs, etc.
    - Fix to squared epsilon insensitive loss

* Learning
    - Many efficiency improvements to online and batch optimizers
    - Fix to BackTrackLineOptimizer that greatly speeds up BFGS and CG
    - API for initialization/finalization of weights added to GradientOptimizers
    - Speedup to parallel trainers by avoiding excess locking

* Inference
    - Greatly improved efficiency for inference and learning in chains using ChainModel
    - Big refactoring/cleanup and fixes to BP

* Linear Algebra
    - Fixes and performance improvements to many tensor operations
    - Fixes and speed/safety improvements to smart tensor accumulators

* Serialization
    - Added version numbers and IDs to Cubbie serialization
    - Added buffering for speed improvements