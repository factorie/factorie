---
title: News
layout: default
weight: 3
group: prefix
---

# News

<ul class="posts">
  {% for post in site.posts %}
  {% if categories == "news" or group == node.group %}
    <li><span>{{ post.date | date_to_string }}</span> &raquo; <a href="{{ post.url }}">{{ post.title }}</a></li>
  {% endif %}
  {% endfor %}
</ul>


{% for post in site.posts %}
{% if categories == "news" or group == node.group %}
--------
{{ post.content }}
&raquo; (<a href="{{ BASE_PATH }}{{ post.url }}">permalink</a>)
{% endif %}
{% endfor %}
