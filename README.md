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

# Peeking Under the Hood

If you want to extend this to an arbitrary news source, there are 2
things you need to do.

## Step 1: Add a New Source Class for it

We'll need to create a class that extends `news--source`:

```elisp
(defclass my-awesome-news-source (news--source)
  ())
```

You'll also want to add custom methods for determining the article
title, publication date, and optionally the tags.
  
## Step 2: Update `url->source`

To use the class we just designed, we need to update `url->source` to
match on the domain, then return an instance of the news source class we
just defined. I'm certain there's some fancy pants macro that
would allow me to write a `(def-news-source ...)` macro, but I'm too
lazy at the moment to do that.

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
