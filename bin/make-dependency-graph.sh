#!/bin/sh

echo "val (_, analysis) = runTask(compile in Compile, currentState); DotGraph.sources(analysis.relations, file(\"dot\"), Seq(new java.io.File(\"src/main/scala\")))" | sbt console-project

echo Generated dot file dot/int-source-deps