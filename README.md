README
===

This file describes the process of generating the Factorie website from the markdown files in this directory.

You should only need to do this if you are working on the design of the website and would like to test your work.

To update documentation and content, you should make the desired changes and then run `generate-and-push-documentation.sh` in the `bin` directory. 

Prerequisites
---

* [Jekyll](http://jekyllrb.com/docs/home/)

How to generate the website (for uploading)
---

Run the following command (from `doc/site` directory):

    $ jekyll build

The output site will be created in `./_site`.

Trying out the website
---

Instead of navigating through the html files in `_site` (which will look wrong), you should test the server by running

    $ jekyll serve

and visiting <http://localhost:4000>.
