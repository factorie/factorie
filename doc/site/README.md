README
===

This file describes the process of generating the factorie website, from the markdown files in this directory.

Prerequisites
---

* [jekyll](https://github.com/mojombo/jekyll)
* [redcarpet2](https://github.com/vmg/redcarpet) for Github flavored markdown

How to generate the website (for uploading)
---

Run the following command (from `doc/site` directory):

    $ jekyll --base-url ""

The output site will be created in `./_site`.

Trying out the website
---

Instead of navigating through the html files in `_site` (which will look wrong), you should test the server by running

    $ jekyll --server --base-url ""

and visiting <http://localhost:4000>.
