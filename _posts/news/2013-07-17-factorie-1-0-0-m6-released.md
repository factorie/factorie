---
title: Factorie 1.0.0-M6 released
layout: default
group: news
categories: news
---

## Factorie 1.0.0-M6 released
 &raquo; by {{ site.author_sameer }} on {{ page.date | date_to_string }}

This is the sixth milestone release for Factorie 1.0. This version comes with many core API changes, complete rewrite of the factorie.la package, reimplemented version of BP, modification to the optimization package, and so on. Detailed changelog attached.

* Executable Jar: **[factorie-1.0.0-M6.jar](http://factorie.googlecode.com/files/factorie-1.0.0-M6.jar)**
* Source files: [factorie-1.0.0-M6-src.tar.gz](http://factorie.googlecode.com/files/factorie-1.0.0-M6-src.tar.gz)

New in version 1.0.0-M6:
---

* Overall
    - Website hosted on github
    - removing deprecated code

* NLP
    - much improved mention annotators
    - classifier-based mention entity type predictor
    - deprecated annotators removed: DepParser1, WithinDocCoref1
    - CorefGazetteers removed
    - new pronoun lexicons
    - bugfixes and improvements to coreference

* Learning
    - removing broken optimizers
    - bugfix in SampleRank when proposals have same score