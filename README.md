# Introduction

Since we no longer "read the news" but research it, here's a handy Emacs
org-mode tool to expedite a lot of the process. Basically, it's a
three-step process:

1. Add `dsf-news.el` to your `~/.emacs` file
2. Create a `~/news/` directory (or set `news-dir` to point to wherever
   you are saving the news articles to)
3. In your running notes on the news (for me, `~/org/news.org`), just
   write down the list of articles you'd like to cite. For example:

```
* killer flying robots
- http://www.politico.com/story/2016/12/drones-military-technology-trump-232933
* Cartoons
- https://www.washingtonpost.com/lifestyle/the-story-behind-the-sudden-cancellation-of-adult-swims-trump-loving-comedy-show/2016/12/23/ed9e2e3a-c3c8-11e6-8422-eac61c0ef74d_story.html?utm_term=.873331ea8217
```

After running `expand-citations`, this becomes

```
* killer flying robots
- [[http://www.politico.com/story/2016/12/drones-military-technology-trump-232933][Killer robots await Trump’s verdict]] (politico.com) <2016-12-25T07:38-0500>
* Cartoons
- [[https://www.washingtonpost.com/lifestyle/the-story-behind-the-sudden-cancellation-of-adult-swims-trump-loving-comedy-show/2016/12/23/ed9e2e3a-c3c8-11e6-8422-eac61c0ef74d_story.html][The story behind the sudden cancellation of Adult Swim’s Trump-loving comedy show]] (washingtonpost.com) <2016-12-23T02:34-500>
```

It will download the articles (for a local copy), extract out the titles
and published timestamp, then prettify the links in your org-mode file.

# License

This code is released under the MIT license, included in the file
`MIT_License`. 
