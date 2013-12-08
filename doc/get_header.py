#!/usr/bin/env python

from sys import argv, exit
from os import path

if len(argv) != 2:
    print "Usage: %s <md-to-headerize>" % argv[0]
    exit(1)

def makeHeader(title):
    digits = int(''.join(c for c in title if c.isdigit()))
    if title[0] == "T":
        group = "tutorial"
    else:
        group = "usersguide"
    return """---
title: "%s"
layout: default
group: %s
weight: %s
---

<a href="{{ site.baseurl }}/tutorial.html">Tutorials</a> &gt;


""" % (title,group,digits)

title_map = {
    'Tutorial01Introduction.scala.md' : 'Tutorial 0: Introduction',
    'Tutorial10Variables.scala.md': 'Tutorial 1: Variables',
    'Tutorial11Domain.scala.md': 'Tutorial 1.1: Domain',
    'Tutorial20Factors.scala.md': 'Tutorial 2: Factor',
    'Tutorial21Family.scala.md': 'Tutorial 2.1: Family',
    'Tutorial30Model.scala.md': 'Tutorial 3: Model',
    'Tutorial32Template.scala.md': 'Tutorial 3.2: Templates',
    'Tutorial40InferenceAndLearning.scala.md': 'Tutorial 4: Inference and Learning',
    'Tutorial60Learning.scala.md': 'Tutorial 6: Optimization and Learning',
    'Tutorial90ParallelismAndHyperparameters.scala.md': 'Tutorial 9: Parallelism and Hyperparameter Optimization',
    'UsersGuide01Introduction.scala.md': "Users Guide 01: Introduction",
    'UsersGuide02Installation.scala.md': "Users Guide 02: Installation",
    'UsersGuide03Overview.scala.md': "Users Guide 03: Overview",
    'UsersGuide06Inference.scala.md': "Users Guide 06: Inference",
    'UsersGuide07LearningAndOptimization.scala.md': "Users Guide 07: Learning and optimization",
}

print makeHeader(title_map[path.basename(argv[1])])
