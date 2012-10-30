---
title: Home
layout: default
weight: 1
group: prefix
---

FACTORIE is a toolkit for deployable probabilistic modeling, implemented as a software library in [Scala](http://www.scala-lang.org). It provides its users with a succinct language for creating relational [factor graphs](http://en.wikipedia.org/wiki/Factor_graph), estimating parameters and performing inference.  Key features:

* It is **object-oriented**, enabling encapsulation, abstraction and inheritance in the definition of random variables, factors, inference and learning methods.
* It is **scalable**, with demonstrated success on problems with many millions of variables and factors, and on models that have changing structure, such as case factor diagrams.  It has also been plugged into a database back-end, representing a new approach to probabilistic databases capable of handling billions of variables.
* It is **flexible**, supporting multiple modeling and inference paradigms.  Its original emphasis was on conditional random fields, undirected graphical models, MCMC inference, online training, and discriminative parameter estimation.  However, it now also supports directed generative models (such as latent Dirichlet allocation), and has preliminary support for variational inference, including belief propagation and mean-field methods.
* It is embedded into a **general purpose programming language**, providing model authors with familiar and extensive resources for implementing the procedural aspects of their solution, including the ability to beneficially mix data pre-processing, diagnostics, evaluation, and other book-keeping code in the same files as the probabilistic model specification.
* It allows the use of **imperative** (procedural) constructs to define the factor graph---an unusual and powerful facet that enables significant efficiencies and also supports the injection of both declarative and procedural domain knowledge into model design.

The structure of generative models can be expressed as a program that describes the generative storyline.  The structure undirected graphical models can be specified in an entity-relationship language, in which the factor templates are expressed as compatibility functions on arbitrary entity-relationship expressions; alternatively, factor templates may also be specified as formulas in first-order logic.  However, most generally, data can be stored in arbitrary data structures (much as one would in deterministic programming), and the connectivity patterns of factor templates can be specified in a Turing-complete imperative style.  This usage of imperative programming to define various aspects of factor graph construction and operation is an innovation originated in FACTORIE; we term this approach imperatively-defined factor graphs.  The above three methods for specifying relational factor graph structure can be mixed in the same model.

FACTORIE has been successfully applied to various tasks in natural language processing and information integration, including

*  named entity recognition
*  entity resolution
*  relation extraction
*  parsing
*  schema matching
*  ontology alignment
*  latent-variable generative models, including latent Dirichlet allocation.

The current recommended FACTORIE source code is version **1.0.0-M2**, available for download [here](download.html).  You can also obtain our latest code changes through the [Mercurial repository](http://code.google.com/p/factorie/source/checkout). Although pre-1.0, it is already extremely useful and quite stable. You can also browse our [list of outstanding issues](http://code.google.com/p/factorie/issues/list). 

Citation
---

Factorie has been released under the [Apache License 2.0](http://www.apache.org/licenses/LICENSE-2.0), and is free to use for commercial or academic purposes. However, please acknowledge its use with a citation:

Andrew McCallum, Karl Schultz, Sameer Singh. "[FACTORIE: Probabilistic Programming via Imperatively Defined Factor Graphs](http://people.cs.umass.edu/~mccallum/papers/factorie-nips09.pdf)". Neural Information Processing Systems (NIPS), 2009.

```scala
@inproceedings{mccallum09:factorie:,
  Author = {Andrew McCallum and Karl Schultz and Sameer Singh}, 
  Booktitle = {Neural Information Processing Systems (NIPS)}, 
  Title = { {FACTORIE}: Probabilistic Programming via Imperatively Defined Factor Graphs}, 
  Year = {2009}}
```

Sponsors
---

Research and development of FACTORIE is supported in part by the UMass Center for Intelligent Information Retrieval; in part by Google, in part by the National Science Foundation under NSF grant #IIS-0803847, #IIS-0326249 and #CNS-0551597; in part by Army prime contract number W911NF-07-1-0216 and University of Pennsylvania subaward number 103-548106; and in part by SRI International subcontract #27-001338 and ARFL prime contract #FA8750-09-C-0181. Any opinions, findings and conclusions or recommendations expressed in this material are the authors' and do not necessarily reflect those of the sponsors.
