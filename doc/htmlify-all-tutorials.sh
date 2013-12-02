#!/bin/bash

OUTPUT_DIR="./html"
EXAMPLE_DIR="../src/main/scala/cc/factorie/tutorial/"

mkdir -p $OUTPUT_DIR

find $EXAMPLE_DIR -name "Tutorial*" -type f | xargs -n 1 ./htmlify.sh
find $EXAMPLE_DIR -name "UsersGuide*" -type f | xargs -n 1 ./htmlify.sh

mv *.html $OUTPUT_DIR/
mv *.md $OUTPUT_DIR/
