#!/usr/bin/env python

from sys import argv, exit
from os import path

if len(argv) != 2:
    print "Usage: %s <md-to-headerize>" % argv[0]
    exit(1)

def makeHeader(title):
    return """---
title: "%s"
layout: default
group: tutorial
---

<a href="{{ site.baseurl }}/tutorial.html">Tutorials</a> &gt;


""" % (title)

title_map = {
    'Tutorial01Introduction.scala.md' : 'Tutorial 0: Introduction',
    'Tutorial10Variables.scala.md': 'Tutorial 1: Variables',
    'Tutorial11Domain.scala.md': 'Tutorial 1.1: Domain',
    'Tutorial20Factors.scala.md': 'Tutorial 2: Factor',
    'Tutorial21Family.scala.md': 'Tutorial 2.1: Family',
    'Tutorial30Model.scala.md': 'Tutorial 3: Model',
    'Tutorial32Template.scala.md': 'Tutorial 3.2: Templates',
    'Tutorial40InferenceAndLearning.scala.md': 'Tutorial 4: Inference and Learning',
    'Tutorial060Learning.scala.md': 'Tutorial 6: Optimization and Learning',
    'Tutorial090ParallelismAndHyperparameters.scala.md': 'Tutorial 9: Parallelism and Hyperparameter Optimization',
    'UsersGuide01Introduction.scala.md': "Users Guide 01: Introduction",
    'UsersGuide02Installation.scala.md': "Users Guide 02: Installation",
    'UsersGuide03Overview.scala.md': "Users Guide 03: Overview",
}

print makeHeader(title_map[path.basename(argv[1])])
