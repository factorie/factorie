# re-generate scaladocs
mvn scala:doc && \

# re-generate tutorial markdown files from scala
cd doc && \
mkdir html && \
./scala2md-usersguide.scala && \
cd .. && \

# checkout site branch
git checkout gh-pages && \
git pull && \

# update tutorial files
git rm usersguide/*md && \
mkdir usersguide && \
cp doc/html/*md usersguide/ && \
git add usersguide/ && \

# update scaladocs
git rm -r scaladocs/ && \
mv target/site/scaladocs . && \
git add scaladocs/ && \

# push changes and return to master branch
git commit -m "updating users guide and scaladocs" && \
git push
git checkout master
