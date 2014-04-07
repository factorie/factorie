---
title: Factorie 1.0.0-M7 released
layout: default
group: news
categories: news
---

## Factorie 1.0.0-M7 released
 &raquo; by {{ site.author_luke }} on {{ page.date | date_to_string }}

This is the seventh milestone release for Factorie 1.0. This version includes an improved NLP pipeline with many new components and fixes. Detailed changelog attached.

* Executable Jar: **[factorie-1.0.0-M7.jar](http://factorie.googlecode.com/files/factorie-1.0.0-M7.jar)**
* Source files: [factorie-1.0.0-M7-sources.jar](http://factorie.googlecode.com/files/factorie-1.0.0-M7-sources.jar)

New in version 1.0.0-M7:
---

* Overall
    - Removed deprecated code
    - Improved tutorials and documentation
    - Improved command line tools

* NLP
    - New tokenizers and sentence segmenter
    - Reworked DocumentAnnotator annotation pipeline
    - Parallel LDA implementation
    - Improved NER
    - Conll2000 loader
    - Support for loading NER3 models from classpath (NER3 requires dependency on factorie-nlp-resources-ner project)
    - Added support for word embeddings in NER3
    - Bugfixes and improvement to mention finders, new NerAndPronounMentionFinder

* Learning
    - Efficiency improvements to accumulators, trainers, and weights maps
    - Small bugfixes to OnlineTrainer and hyperparameter optimization

* Inference
    - Changed Infer API
    - Bugfix to dual decomposition