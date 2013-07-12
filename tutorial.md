---
title: "Tutorials"
layout: default
weight: 2
group: doc
---

# Tutorials

<ul>
  {% assign pages_list = site.pages %}
  {% assign group = 'tutorial' %}
  {% include pages_list %}
  <li class="divider"></li>  
  {% for page in site.pages %}
  {% if page.group == "tutorial" %}
    <li> <a href="{{ site.baseurl }}{{ page.url }}">{{ page.title }}</a></li>
  {% endif %}
  {% endfor %}
</ul>
