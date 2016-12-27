# Introduction

Since we no longer "read the news" but research it, here's a handy Emacs
org-mode tool to expedite a lot of the process. Basically, it's a
three-step process:

1. Add to your `~/emacs` lines (1) file loading `dsf-news.el` and
   then (2) add `(require 'dsf-news)`
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

# Warning to Ubuntu Users

This library requires `dom.el` which Ubuntu apparently doesn't package
alongside its other emacs. You'll have to [download it](https://github.com/emacs-mirror/emacs/blob/master/lisp/dom.el)
and stick it where Emacs can find it. (Ubuntu's Emacs 24 package
contains `dom.el`'s dependencies, which is good.)

# Adding more news sources

Adding a new source is easy now, there's a simple macro to use: `dsf-defsource`.
It expects 2 arguments:

```elisp
(dsf-defsource <domain>
  <list-of-methods>)
```

The list of methods handles `:title`, `:published`, and `:tags`. (If any
are missing, it defaults to finding the `og:title` contents for the
title, similar metadata for the published time, and `nil` for the tags.)

For example:

```elisp
(dsf-defsource "economist.com"
  ((:published (dom)
    (or (dom-attr (dom-by-tag dom 'time) 'datetime) ; published articles
        (sailthru-date dom))))) ; blog articles
```

Under the hood, it handles modifying the `url->source` function, and
uses the appropriate methods for determining (i) the publication date, 
(ii) the article title, (iii) the tags, if any.

# Lazy Usage

My `.emacs` file includes the lines

```elisp
(load-file "~/src/dsf-news/dsf-news.el")
(require 'dsf-news)

(add-hook 'org-mode-hook
          '(lambda ()
             (local-set-key [f2] 'expand-citations)))
```

This way, I can add my plain links, and when I'm done just hit
<kbd>f2</kbd> to expand all the citations.

# License

This code is released under the MIT license, included in the file
`MIT_License`. 
