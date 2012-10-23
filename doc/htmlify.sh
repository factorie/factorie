#!/bin/bash

TUT_FILE=$1
MD_FILE=./`basename $1`.md
HTML_FILE=./`basename $1`.html

rm $MD_FILE
rm $HTML_FILE
touch $MD_FILE

echo -e "\`\`\`scala\n" >> $MD_FILE
cat $TUT_FILE | perl ./preprocess_tutorial.pl >> $MD_FILE
echo -e "\`\`\`\n" >> $MD_FILE

pandoc $MD_FILE -o $HTML_FILE -s --highlight-style=pygments

