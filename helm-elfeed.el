;;; helm-elfeed.el --- Helm interface for Elfeed   -*- lexical-binding: t -*-

;; Copyright (C) 2025 Timm Lichte

;; Author: Timm Lichte <timm.lichte@uni-tuebingen.de>
;; URL: 
;; Version: 0
;; Last modified: 2025-04-03 Thu 08:48:24
;; Package-Requires: ((helm "3.9.6") (elfeed "3.4.2"))
;; Keywords: helm elfeed

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use,
;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following
;; conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:

;;; Code:

(require 'helm)
(require 'elfeed)

(defgroup helm-elfeed nil
  "Helm interface for Elfeed."
  :group 'helm)

(defcustom helm-elfeed-generic-search-queries
	'(("All" . "@6-months-ago")
		("Marked/starred" . "+star")
		("Unread" . "@6-months-ago +unread"))
	"Generic search queries used in `helm-elfeed', represented as a list of pairs (NAME . QUERY)."
	:type '(cons string sting)
  :group 'helm-elfeed)

(defun he--trim-or-fill (field-value column-length)
  "Trim or fill a FIELD-VALUE to a specified COLUMN-LENGTH."
  (if (> (length field-value) (- column-length 2))
      (concat (truncate-string-to-width field-value
                                        (- column-length 2))
              "… ")
    (string-pad field-value column-length)))

(defun helm-elfeed--make-candidates ()
	"Make candidates for `helm-elfeed'.  A candidate is a pair (STRING PLIST)."
	(let ((unread-feeds (helm-elfeed--get-unread-feeds))
				(all-feeds (reverse (elfeed-feed-list)))
				(tags-width (let ((max-length 0))
											(dolist (feed elfeed-feeds max-length)
												(let* ((tags (cdr feed))
                               (tags-str (format "(%s) "
                                                 (string-join (mapcar 'symbol-name tags) ", ")))
                               (tags-length (length tags-str)))
													(setq max-length (max max-length tags-length))))))
				(generic-searches (cl-loop
                           for generic-search in helm-elfeed-generic-search-queries
                           for search-format = (car generic-search)
                           for search-query = (cdr generic-search)
                           collect `(,search-format
                                     .
                                     ,(list `(:url nil :query ,search-query))))))
		(seq-concatenate
     'list generic-searches
     (cl-loop
			for feed-url in (seq-union unread-feeds all-feeds)
			for feed-tags = (helm-elfeed--get-feed-tags feed-url)
			for feed-object = (elfeed-db-get-feed feed-url)
			for feed-title = (or (elfeed-meta feed-object :title) (elfeed-feed-title feed-object))
			for feed-url-format = (propertize
                             (he--trim-or-fill (format "(%s)" feed-url)
                                               (max 2
																										(- (window-width)
                                                       (length feed-title)
                                                       tags-width)))
                             'face 'font-lock-comment-face)
			for feed-tags-format = (propertize
															(format "(%s)"
																			(string-join (mapcar 'symbol-name feed-tags) ","))
															'face 'font-lock-comment-face)
			for feed-format = (concat
                         (if (member feed-url unread-feeds)
                             (propertize feed-title
                                         'face 'elfeed-summary-count-face-unread)
                           feed-title)
                         " " feed-url-format
                         " " feed-tags-format
                         )
			for feed-plist = `(:url ,feed-url
															:query ,(concat
                                       "=" (replace-regexp-in-string "\\([?+]\\)" "\\\\\\1"
                                                                     feed-url)
                                       (when (member feed-url unread-feeds) " +unread")))
			if feed-title
			collect `(,feed-format
								.
								,(list feed-plist)) 
			))))

(defun helm-elfeed--get-unread-feeds ()
  "Return unread feeds in `elfeed-db' as a list of feed URLs."
  (let ((unread-feeds ()))
    (with-elfeed-db-visit (entry feed)
      (let ((feed-url (elfeed-feed-url feed)))
        (when (and (not (member feed-url unread-feeds))
                   (member 'unread (elfeed-entry-tags entry)))
          (push feed-url unread-feeds))))
    (reverse unread-feeds)))

(defun helm-elfeed--get-feed-tags (url)
  "Return list of tag symbols in `elfeed-db' associated with a feed URL."
  (cl-loop
   for feed in elfeed-feeds
   for feed-url = (car feed)
   for feed-tags = (cdr feed)
   if (string= url feed-url)
   return feed-tags
   ))

(defvar helm-elfeed--actions
  (helm-make-actions
   "Show feed" #'helm-elfeed-update-filter-action
   "Mark feed as read" #'helm-elfeed-mark-feed-as-read-action
   "Update feed" #'helm-elfeed-update-feed-action
   "Update all feeds" #'helm-elfeed-update-action
   )
  "List of pairs (STRING FUNCTIONSYMBOL), which represent the
actions used in `helm-elfeed'.")

(defun helm-elfeed-update-filter-action (candidate)
  "Update the Elfeed filter using the query of CANDIDATE."
  (let ((search-query (cl-loop
                       for candidate in (helm-marked-candidates)
                       collect (plist-get (car candidate) :query)
                       into collected-strings
                       finally (return (string-join collected-strings " "))
                       )))
    (elfeed-search-set-filter search-query)
    (switch-to-buffer "*elfeed-search*")))

(defun helm-elfeed-mark-feed-as-read-action (candidate)
  "Update the Elfeed search using the query of CANDIDATE."
  (let ((input helm-input)
        (search-query (cl-loop
                       for candidate in (helm-marked-candidates)
                       collect (plist-get (car candidate) :query)
                       into collected-strings
                       finally (return (string-join collected-strings " "))
                       )))
    (elfeed-search-set-filter search-query)
    (switch-to-buffer "*elfeed-search*")
    (mark-whole-buffer)
    (elfeed-search-untag-all-unread)
    (helm-elfeed input)))

(defun helm-elfeed-update-feed-action (candidate)
  "Update Elfeed database."
  (let ((feed-url (plist-get (car candidate) :url))
        (search-query (plist-get (car candidate) :query))
        (input helm-input))
    (elfeed-search-set-filter search-query)
    (switch-to-buffer "*elfeed-search*")
    (if feed-url
        (elfeed-update-feed feed-url)
      (elfeed-update))))

(defun helm-elfeed-update-action (candidate)
  "Update Elfeed database."
  (elfeed-search-set-filter "@6-months-ago")
  (switch-to-buffer "*elfeed-search*")
  (elfeed-update))

;;;###autoload
(defun helm-elfeed ()
  "Switch between Elfeed feeds with Helm."
  (interactive)
  (helm :sources (helm-build-sync-source "Feeds:"
                   :candidates #'helm-elfeed--make-candidates
                   :display-to-real nil ; Transform the selected candidate when passing it to action.
                   :action helm-elfeed--actions)
        :buffer "*helm-elfeed*"
        :truncate-lines helm-buffers-truncate-lines
        ))

(provide 'helm-khard)


;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; helm-elfeed.el ends here
