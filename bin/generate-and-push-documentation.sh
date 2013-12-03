mvn scala:doc && \
cd doc && \
rm -rf html && \
sh htmlify-all-tutorials.sh && \
cd .. && \
git checkout gh-pages && \
git rm tutorials/*md && \
mkdir tutorials && \
cp doc/html/*md tutorials/ && \
git add tutorials/ && \
git rm -r  scaladocs/ && \
mv target/site/scaladocs . && \
git add scaladocs/ && \
git commit -m "updating tutorials and scaladocs" && \
git push

git checkout master