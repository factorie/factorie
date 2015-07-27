#!/bin/bash

MEMORY="30g"
MODELNAME="CtbChainPosTagger.factorie"

trainDir="--train-dir=/iesl/canvas/hosinger/data/CTB7/data/utf-8/postagged/"
testDir="--test-dir=/iesl/canvas/hosinger/data/CTB7/data/utf-8/postagged/"
l1="--l1=0.07379024017066312"
l2="--l2=1.1481182114789412E-8"
hinge="--use-hinge-loss=false"
rate="--rate=1.1296435034705585E-4"
#rate="--rate=0.01"
delta="--delta=6.090780307103741"
#delta="--delta=0.1"
save="--save-model=true"
iters="--num-iterations=5"
cutoff="--cutoff=2"
model="--model=$MODELNAME"


java -classpath target/factorie-1.2-SNAPSHOT-nlp-jar-with-dependencies.jar -Xmx$MEMORY -Dfile.encoding=UTF-8 cc.factorie.app.nlp.pos.CtbChainPosTrainer $trainDir $testDir $hinge $save $iters $cutoff $model
