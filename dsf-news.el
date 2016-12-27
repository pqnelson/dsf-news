;;; dsf-news.el --- Support for researching news in org-mode.

;;; Version: 0.0.1
;;; Author: Alex Nelson <pqnelson@gmail.com>
;;; Created: 25 December 2016
;;; Copyright: MIT License

;; Keywords: org-mode, news
;; Homepage: http://github.com/pqnelson/dsf-news

;;; Commentary:
;; Used in conjunction with org-mode, simply copy/paste raw URLs into
;; an org-mode file, then run this code to transform naked URLs into
;; org-mode links pointing at the URL with the article title as the
;; link-name.
;;
;; So you may just copy/paste a bunch of links into your org-file, e.g.
;;
;; * killer flying robots
;; - http://www.politico.com/story/2016/12/drones-military-technology-trump-232933
;; * Cartoons
;; - https://www.washingtonpost.com/lifestyle/the-story-behind-the-sudden-cancellation-of-adult-swims-trump-loving-comedy-show/2016/12/23/ed9e2e3a-c3c8-11e6-8422-eac61c0ef74d_story.html?utm_term=.873331ea8217
;;
;; Then running the magic `expand-citations' command will transform this
;; into
;;
;; * killer flying robots
;; - [[http://www.politico.com/story/2016/12/drones-military-technology-trump-232933][Killer robots await Trump’s verdict]] (politico.com) <2016-12-25T07:38-0500>
;; * Cartoons
;; - [[https://www.washingtonpost.com/lifestyle/the-story-behind-the-sudden-cancellation-of-adult-swims-trump-loving-comedy-show/2016/12/23/ed9e2e3a-c3c8-11e6-8422-eac61c0ef74d_story.html][The story behind the sudden cancellation of Adult Swim’s Trump-loving comedy show]] (washingtonpost.com) <2016-12-23T02:34-500>
;;
;; It is idempotent, and will not download articles already saved into
;; the `news-dir'.

;; TODO: include tags

;;; Code:

(require 'url)
(require 'dom)
(defvar dsf-news-version "0.0.1")
(defvar news-dir "~/news/"
  "The directory where the news-source subdirectories live.")

(defun url-domain (url)
  (let ((host (url-host (if (url-p url)
                            url
                          (url-generic-parse-url url)))))
    (if (string-prefix-p "www." host)
        (substring host 4)
      host)))

(ert-deftest url-domain-test ()
  (should (equal (url-domain (url-generic-parse-url "https://www.google.com"))
                 "google.com"))
  (should (equal (url-domain "http://www.nytimes.com/2016/12/21/us/politics/kansas-republicans-democrats-elections.html")
                 "nytimes.com"))
  (should (equal (url-domain "https://www.washingtonpost.com/news/post-politics/wp/2016/12/25/planned-parenthood-focus-groups-suggest-that-lack-of-attention-on-social-issues-helped-trump/?utm_term=.6bd1551fe0df")
                 "washingtonpost.com")))

(defun download-file (&optional url download-dir download-name)
  "Download a given URL into a DOWNLOAD-DIR (defaults to ~/downloads/).
May rename the file using DOWNLOAD-NAME parameter."
  (interactive)
  (let ((url (or url
                 (read-string "Enter download URL: "))))
    (let ((download-buffer (url-retrieve-synchronously url)))
      (save-excursion
        (set-buffer download-buffer)
        ;; we may have to trim the http response
        (goto-char (point-min))
        (re-search-forward "^$" nil 'move)
        (forward-char)
        (delete-region (point-min) (point))
        (write-file (concat (or download-dir
                                "~/downloads/")
                            (or download-name
                                (car (last (split-string url "/" t))))))))))

(defun download-article (url)
  "Downloads an article given the URL to `news-dir'. If the file
has already been downloaded, then *do not* download it again."
  (let ((dir (concat news-dir (url-domain url) "/"))
        (file-name (car (last (split-string url "/" t)))))
    (if (file-exists-p (concat dir file-name))
        nil
      (download-file url dir file-name))))

;;; helper functions
; http://stackoverflow.com/a/1942422
(defun dictionary-lessp (str1 str2)
  "return t if STR1 is < STR2 when doing a dictionary compare
(splitting the string at numbers and doing numeric compare with them)"
  (let ((str1-components (dict-split str1))
        (str2-components (dict-split str2)))
    (dict-lessp str1-components str2-components)))

(defun dict-lessp (slist1 slist2)
  "compare the two lists of strings & numbers"
  (cond ((null slist1)
         (not (null slist2)))
        ((null slist2)
         nil)
        ((and (numberp (car slist1))
              (stringp (car slist2)))
         t)
        ((and (numberp (car slist2))
              (stringp (car slist1)))
         nil)
        ((and (numberp (car slist1))
              (numberp (car slist2)))
         (or (< (car slist1) (car slist2))
             (and (= (car slist1) (car slist2))
                  (dict-lessp (cdr slist1) (cdr slist2)))))
        (t
         (or (string-lessp (car slist1) (car slist2))
             (and (string-equal (car slist1) (car slist2))
                  (dict-lessp (cdr slist1) (cdr slist2)))))))

(defun dict-split (str)
  "split a string into a list of number and non-number components"
  (save-match-data 
    (let ((res nil))
      (while (and str (not (string-equal "" str)))
        (let ((p (string-match "[0-9]*\\.?[0-9]+" str)))
          (cond ((null p)
                 (setq res (cons str res))
                 (setq str nil))
                ((= p 0)
                 (setq res (cons (string-to-number (match-string 0 str)) res))
                 (setq str (substring str (match-end 0))))
                (t
                 (setq res (cons (substring str 0 (match-beginning 0)) res))
                 (setq str (substring str (match-beginning 0)))))))
      (reverse res))))

(defun subset? (subset superset)
  (if (<= (length subset) (length superset))
      (if (null subset)
          t
        (when (member (car subset) superset)
          (subset? (cdr subset) superset)))))

(ert-deftest subset?-test ()
  (should (equal (subset? '(1 2) '(1 2 4 8))
                 t))
  (should (equal (subset? '(1 2 3) '(1 2 4 8))
                 nil))
  (should (equal (subset? nil '(1 2 4 8))
                 t)))

(defun distinct (lst)
  "Remove duplicate elements from a list, also remove any nil elements."
  (if (and lst
           (listp lst))
      (delq nil (delete-dups lst))))

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                    (: (* (any " \t\n")) eos)))
                            ""
                            str))

(defun string-index-of (needle s &optional ignore-case)
  "Returns first index of NEEDLE in S, or nil.
If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences."
  (let ((case-fold-search ignore-case))
    (string-match-p (regexp-quote needle) s)))

(defun last-index-of (regex str &optional ignore-case)
  (let ((start 0)
        (case-fold-search ignore-case)
        idx)
    (while (string-match regex str start)
      (setq idx (match-beginning 0))
      (setq start (match-end 0)))
    idx))

(defun kill-unacceptable-keyword-chars (s)
  "Remove characters that are not allowed in an org-mode tag"
  (replace-regexp-in-string "[^a-zA-Z0-9_@]" "" s))

(defun string->tag (s)
  (kill-unacceptable-keyword-chars
   (replace-regexp-in-string "[-\s]+"
                             "_"
                             (downcase s))))

(defun string->keyword (s)
  "Turn a string into an org-mode keyword"
  (concat ":"
          (string->tag s)
          ":"))

(ert-deftest string->keyword-test ()
  (should (= (string->keyword "Republican Party")
             ":republican_party"))
  (should (= (string->keyword "Apple, R. W. Jr")
             ":apple_r_w_jr:")))

(defun roman-numeral? (s)
  (= 0 (string-match "M*\\(CM\\|CD\\|D?C\\{0,3\\}\\)\\(XC\\|XL\\|L?X\\{0,3\\}\\)\\(IX\\|IV\\|V?I\\{0,3\\}\\)$"
                     s)))

(ert-deftest roman-numeral-test ()
  (should (roman-numeral? "XVI"))
  (should (not (roman-numeral? "IVX"))))

(defun humanize-suffix (suffix-str)
  " Given a person's name suffix, we normalize it as either 'Jr.', 'Sr.',
or a roman numeral -- no other suffixes are acceptable."
  (if (string-prefix-p "JR" suffix-str t)
      "Jr."
    (if (string-prefix-p "SR" suffix-str t)
        "Sr."
      (if (roman-numeral? suffix-str)
          suffix-str))))

; A given tag seems to be a triple
;   <tag> ::= (<tag-name> <attribute> <contents>)
;   <contents> ::= nil
;                | <string> <contents>
;                | <tag> <contents>
;   <attribute> ::= nil
;                 | (cons (<attribute-key> . <attribute-value>) <attribute>)
; For more about the DOM, read the fine documentation
; https://www.gnu.org/software/emacs/manual/html_node/elisp/Document-Object-Model.html#Document-Object-Model
(defun html-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (libxml-parse-html-region (point-min) (point-max))))

(defun meta-tag/content (node)
  (dom-attr node 'content))

(defun meta-tags (dom)
  (dom-by-tag dom 'meta))

(defun og-title (dom)
  (dom-attr (dom-elements dom 'property "og:title") 'content))

(defun og-published (dom)
  (or (dom-attr (dom-elements dom 'property "article:published") 'content)
      (dom-attr (dom-elements dom 'property "og:article:published_time") 'content)
      (dom-attr (dom-elements dom 'property "og:article:published") 'content)))
      
(defun sailthru-date (dom)
  (dom-attr (dom-elements dom 'name "sailthru.date")
            'content))

(defun sailthru-tags (dom)
  "The Economist uses sailthru.tags to keep track of their tags. They
usually aren't that good, though."
  (dom-attr (dom-elements dom 'name "sailthru.tags")
            'content))

;;; source-specific helper functions
;; Each news source should have a corresponding class, which provides
;; methods for extracting out
;;
;; (1) the article title
;; (2) the article's publication datetime
;; (3) the tags, if any
;;
;; Since they all follow the same series of manipulations, there should
;; be a generic baseclass which will provide default methods to extract
;; out these pieces of data.


;; nytimes.com
(defun nytimes/tags (dom)
  (mapcar 'meta-tag/content
          (dom-elements dom 'property "article:tag")))

(defun nytimes.person/last-name (s)
  (let ((idx (string-match-p "," s)))
    (if idx
        (substring s 0 idx))))

(ert-deftest nytimes.person/last-name-test ()
  (should (equal (nytimes.person/last-name "foo, bar")
                 "foo"))
  (should (equal (nytimes.person/last-name "Braun, Werner von")
                 "Braun"))
  (should (equal (nytimes.person/last-name "Apple, R. W. Jr.")
                 "Apple")))

(defun nytimes.person/name-parts (s)
  (let ((start-idx (string-index-of "," s)))
    (if start-idx
        (chomp (substring s (1+ start-idx))))))

(ert-deftest nytimes.person/name-parts-test ()
  (should (equal (nytimes.person/name-parts "foo, bar c. iii")
                 "bar c. iii"))
  (should (equal (nytimes.person/name-parts "Apple, R. W. Jr")
                 "R. W. Jr")))

(defun nytimes.person/first-name (name-parts)
  (let ((idx (string-index-of "." name-parts)))
    (if (and idx
             (numberp idx))
        (if (> idx 4)
            (substring name-parts 0 (- idx 2))
          (substring name-parts 0 idx))
      ;; test if there is a suffix or middle initial
      name-parts)))

(ert-deftest nytimes.person/first-name-test ()
  (should (equal (nytimes.person/first-name "Werner von")
                 "Werner von"))
  (should (equal (nytimes.person/first-name "bar c.")
                 "bar"))
  (should (equal (nytimes.person/first-name "R. W. Jr.")
                 "R")))

(defun nytimes.person/middle-initial (name-parts)
  (let* ((space-idx (string-index-of " " name-parts))
         (period-idx (string-index-of "."
                                      (if space-idx
                                          (substring name-parts
                                                     space-idx)
                                        name-parts)))
         (idx (when period-idx
                (if space-idx
                    (+ space-idx period-idx)
                  period-idx))))
    (if idx
        (substring name-parts (1- idx) (1+ idx)))))



(ert-deftest nytimes.person/middle-initial-test ()
  (should (equal (nytimes.person/middle-initial "R. W. Jr.")
                 "W."))
  (should (equal (nytimes.person/middle-initial "bar c.")
                 "c."))
  (should (equal (nytimes.person/middle-initial "Richard Milhous")
                 nil)))

(defun nytimes.person/suffix (name-parts)
  (let* ((period-idx (string-index-of "." name-parts))
         (idx (last-index-of " " name-parts)))
    (if (and idx
             (or (and (numberp period-idx)
                      (> idx period-idx))
                 (null period-idx)))
        (humanize-suffix
         (upcase (substring name-parts (1+ idx)))))))

(ert-deftest nytimes.person/suffix-test ()
  (should (equal (nytimes.person/suffix "Bar C. III")
                 "III"))
  (should (equal (nytimes.person/suffix "Frank Jr")
                 "Jr."))
  (should (equal (nytimes.person/suffix "Bar Sr")
                 "Sr."))
  (should (equal (nytimes.person/suffix "Bar C.")
                 nil))
  (should (equal (nytimes.person/suffix "Richard Milhous")
                 nil)))

(defun nytimes-person-tag-cleanup (s)
  (let* ((last-name (nytimes.person/last-name s))
         (name-parts (nytimes.person/name-parts s))
         (first-name (nytimes.person/first-name name-parts))
         (middle-initial (nytimes.person/middle-initial name-parts))
         (suffix (nytimes.person/suffix name-parts)))
    (concat first-name
            (if middle-initial
                (concat " " middle-initial))
            " "
            last-name
            (if suffix
                (concat " " suffix)))))

(ert-deftest nytimes-person-tag-cleanup-test ()
  (should (equal (nytimes-person-tag-cleanup "Apple, R. W. Jr")
                 "R W. Apple Jr."))
  (should (equal (nytimes-person-tag-cleanup "Nixon, Richard Milhous")
                 "Richard Milhous Nixon")))
;;  "Trump, Donald J" "Houston (Tex)" "Restaurants" "United States Politics and Government")

(defun firm-suffix? (s)
  (or (string-suffix-p "Inc." s)
      (string-suffix-p "Incorporated" s)
      (string-suffix-p "LLC." s)
      (string-suffix-p "LLC." s)
      (string-suffix-p "Limited" s)
      (string-suffix-p "Ltd." s)))

(defun nytimes.tag/person? (s)
  "Tests if the given string is a person or not"
  ;; there is exactly 1 comma
  (let ((idx (string-match-p "," s)))
    (if idx
        (and
         (null (string-match-p "," s (1+ idx)))
         (null (string-match-p "&" s))
         (not (firm-suffix? s))))))

(ert-deftest nytimes.tag/person-p-test ()
  (should (equal (nytimes.tag/person? "Apple, R. W. Jr.")
                 t))
  (should (equal (nytimes.tag/person? "Republican Party")
                 nil))
  (should (equal (nytimes.tag/person? "Benedict XVI")
                 nil))
  (should (equal (nytimes.tag/person? "Cher")
                 nil))
  (should (equal (nytimes.tag/person? "1st United Bancorp, Incorporated")
                 nil))
  (should (equal (nytimes.tag/person? "acme & sons")
                 nil)))

(defun nytimes.tag/normalize (s)
  (string->tag
   (if (nytimes.tag/person? s)
       (nytimes-person-tag-cleanup s)
     s)))

(defun nytimes.tag/keyword (s)
  (string->keyword
   (if (nytimes.tag/person? s)
       (nytimes-person-tag-cleanup s)
     s)))

(ert-deftest nytimes.tag/keyword-test ()
  (should (equal (nytimes.tag/keyword "Republican Party")
                 ":republican_party:"))
  (should (equal (nytimes.tag/keyword "Apple, R. W. Jr.")
                ":r_w_apple_jr:"))
  (should (equal (nytimes.tag/keyword "Braun, Werner von")
                 ":werner_von_braun:")))

;;; politico tags
(defun politico/tags (dom)
  (let ((match ()))
    (dolist (child (dom-by-tag (dom-by-class dom "categories-list") 'a))
      (push (string->tag (chomp (dom-text child))) match))
    match))

(defun csv->tags (dom meta-tag-name)
  (mapcar 'string->tag
          (split-string (dom-attr (dom-elements dom 'name meta-tag-name)
                                  'content)
                        ",")))

;;; rollcall tags
(defun rollcall/tags (dom)
  (csv->tags dom "news_keywords"))

(defun wsj/tags (dom)
  (csv->tags dom "keywords"))

;;; news source
; Dispatch the methods to determine the title, published date, etc.,
; based on the news source...which are implemented as subclasses of
; `news--source' because I have no better alternative.
(defclass news--source () ; No superclasses
  ())

;; Generically, the title is given by the og:title meta tag
(defmethod dsf-title ((s news--source) dom)
  (og-title dom))

;; Generically, most news sources don't have any tags
(defmethod dsf-tags ((s news--source) dom)
  nil)

;; get the ISO 8601 date-timestamp when the article was published
(defmethod dsf-published ((s news--source) dom)
  (or (og-published dom)
      (dom-attr (dom-by-tag dom 'time) 'datetime)))

(defvar dsf-registered-sources '()
  "A running list of the news sources registered by the program")


(defun dsf-url->source (url)
  "Produce a `news--source' object for the given URL"
  (let* ((host (url-domain url))
         (kv-pair (assoc host dsf-registered-sources)))
    (if kv-pair
        (cadr kv-pair)
      (news--source)))) ; defaults to generic news source

(defconst afp-month-lookup '(("Jan" . 1) ("Feb" . 2) ("Mar" . 3)
                             ("Apr" . 4) ("May" . 5) ("June" . 6)
                             ("July" . 7) ("Aug" . 8) ("Sep" . 9)
                             ("Oct" . 10) ("Nov" . 11) ("Dec" . 12)))

(defun afp/published (dom)
  (concat (dom-text (dom-by-class dom 'y))
          "-"
          (cdr (assoc (dom-text (dom-by-class dom 'm)) afp-month-lookup))
          "-"
          (dom-text (dom-by-class dom 'd))))

(defmacro dsf-defsource (domain methods)
  (let* ((title-method (assoc :title methods))
         (tags-method (assoc :tags methods))
         (published-method (assoc :published methods))
         (class-name (make-symbol domain)))
    (when (not (assoc domain dsf-registered-sources))
      (list 'progn
          `(defclass ,class-name (news--source)
             ())
       (when title-method
         `(defmethod dsf-title ((s ,class-name) ,@(cadr title-method))
           ,@(cddr title-method)))
       (when tags-method
         `(defmethod dsf-tags ((s ,class-name) ,@(cadr tags-method))
           ,@(cddr tags-method)))
       (when published-method
         `(defmethod dsf-published ((s ,class-name) ,@(cadr published-method))
            ,@(cddr published-method)))
       `(push `(,,domain ,(make-instance ,class-name))
              dsf-registered-sources)))))

(dsf-defsource "rollcall.com"
 ((:published (dom)
   (dom-attr (dom-elements dom 'property "article:published_time")
             'content))
  (:tags (dom) (rollcall/tags dom))))


(dsf-defsource "nytimes.com"
  ((:tags (dom)
    (mapcar 'nytimes.tag/normalize (nytimes/tags dom)))))

(dsf-defsource "economist.com"
  ((:published (dom)
    (or (dom-attr (dom-by-tag dom 'time) 'datetime) ; published articles
        (sailthru-date dom))))) ; blog articles

(dsf-defsource "politico.com"
  ((:published (dom)
    (dom-attr (dom-by-tag dom 'time) 'datetime))               
   (:tags (dom)
    (politico/tags dom))))

; og:article:tag contains a comma seperated string list of tags for the article
(dsf-defsource "reuters.com"
  ((:published (dom)
    (sailthru-date dom))))

(dsf-defsource "washingtonpost.com"
  ((:published (dom)
    (dom-attr (dom-elements dom 'itemprop "datePublished")
              'content))))

(dsf-defsource "wsj.com"
  ((:title (dom) 
    (dom-attr (dom-elements dom 'name "article.headline")
              'content))
   (:tags (dom) (wsj/tags dom))
   (:published (dom)
    (dom-attr (dom-elements dom 'name "article.published")
              'content))))

(dsf-defsource "afp.com"
  ((:title (dom)
    (dom-texts
     (dom-elements (dom-by-tag dom 'h3)
                   'class
                   "htitle")))
   (:published (dom) (afp/published dom))))

(ert-deftest dsf-url->source-test ()
  (should (washingtonpost.com-p (dsf-url->source "https://www.washingtonpost.com/news/post-politics/wp/2016/12/27/trump-names-bush-administration-veteran-thomas-bossert-to-white-house-homeland-security-post/?utm_term=.9cdc59ec3bd9"))))


;;; news article data
; code to create an object holding all the relevant information for a
; news article
(defclass news--article nil
  ((url
    :initarg :url
    :initform ""
    :documentation "Where the article lives on the inter-webs")
   (title
    :initarg :title
    :initform ""
    :documentation "The title of the article")
   (published
    :initarg :published
    :initform ""
    :documentation "Publication date for the article, when available")
   (tags
    :initarg :tags
    :initform nil
    :documentation "Tags the publication assigns to the article; right
  now, it is just a list of strings")))

(defun make-article (url)
  "Download the article, then parse out the relevant data"
  (download-article url)
  (let* ((source (dsf-url->source url))
         (file-path (concat news-dir
                            (url-domain url)
                            "/"
                            (car (last (split-string url "/" t)))))
         (dom (html-from-file file-path)))
    (news--article
     :url url
     :title (dsf-title source dom)
     :published (dsf-published source dom)
     :tags (distinct (dsf-tags source dom)))))

(defun append-org-tags (tags)
  "When the given list of TAGS (strings) are not part of the current
header's tags, append the distinct tags on, then alphebatically sort
them all."
  (when tags
    (save-excursion
      (org-back-to-heading)
      (let* ((current-tags (org-element-property :tags (org-element-at-point))))
        (when (not (subset? tags current-tags))
          (org-set-tags-to
           (sort
            (distinct (append current-tags
                              tags))
            'dictionary-lessp)))))))

(defun cite-article (url)
  "Replace the plain-link with a fancy link, whose description is the
article's title, followed by the URL's domain in parentheses, and then
the publication datetime."
  (let* ((article-object (make-article url)))
    (concat "[[" url "][" (oref article-object :title) "]] "
            "(" (url-domain url) ")"
            (if (oref article-object :published)
                (concat " <" (oref article-object :published) ">")))
    ))


;;;### autoload
(defun expand-citations ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\([^\\[]\\)\\(http[s]?://[^ \n]*\\)" nil t)
      (let ((remove (list (match-beginning 2) (match-end 2)))
            (description (org-match-string-no-properties 2)))
        (apply 'delete-region remove)
        (insert (cite-article description))
        (append-org-tags (oref (make-article description) :tags))))))

(provide 'dsf-news)
;;; news.el ends here
