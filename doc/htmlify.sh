#!/bin/bash

TUT_FILE=$1
MD_FILE=./`basename $1`.md
HTML_FILE=./`basename $1`.html

rm $MD_FILE
rm $MD_FILE.html
touch $MD_FILE

echo -e "\`\`\`scala\n" >> $MD_FILE

cat $TUT_FILE | \
sed -e 's/[[:space:]]*\/\*\*/```\
/'  -e 's/[[:space:]]*\/\*/```\
/'  -e 's/^[[:space:]]*\* \(.*\)/\1/' \
    -e 's/[[:space:]]*\*\//\
```scala\
/'  -e 's/\s*```/\
```/' >> $MD_FILE

echo -e "\`\`\`\n" >> $MD_FILE

pandoc $MD_FILE -o $HTML_FILE -s --highlight-style=pygments

