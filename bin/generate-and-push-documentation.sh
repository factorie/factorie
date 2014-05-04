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
git rm tutorials/*md && \
mkdir tutorials && \
cp doc/html/*md tutorials/ && \
git add tutorials/ && \

# update scaladocs
git rm -r scaladocs/ && \
mv target/site/scaladocs . && \
git add scaladocs/ && \

# push changes and return to master branch
git commit -m "updating tutorials and scaladocs" && \
git push
git checkout master
