---
title: "NLP REST API"
layout: default
weight: 1
group: demo
---

NLP REST API
====================

The REST service which powers the POS/Parsing demonstration is publicly
available.

It can be accessed via:

```sh
$ curl http://localhost:8888/sample/

{"tokens":["This","is","a","sample","sentence","."]
,"pos":["DT","VBZ","DT","JJ","NN","."]
,"deps":[[-1,1],[1,0],[1,4],[1,5],[4,2],[4,3]]}

$ curl http://localhost:8888/sentence/ --data "The dog ran over the car."

{"tokens":["The","dog","ran","over","the","car","."]
,"pos":["DT","NN","VBD","IN","DT","NN","."]
,"deps":[[-1,2],[1,0],[2,1],[2,3],[2,6],[3,5],[5,4]]}
```

