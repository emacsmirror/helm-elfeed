(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(dolist (package '(elfeed helm))
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))

(tool-bar-mode 0)

(require 'elfeed)

(setq elfeed-feeds
      '(("https://planet.emacslife.com/atom.xml" computer emacs)
        ("https://snyder.substack.com/feed" politics)
        ("http://xkcd.com/rss.xml" comics)))

(setq elfeed-db-directory (expand-file-name "elfeeddb-test" user-emacs-directory))

(use-package gif-screencast
	:ensure t)

(use-package keycast
	:ensure t
	:config
	(keycast-mode-line-mode 1))

(use-package helm-elfeed
	:after elfeed
  :load-path "../../../helm-elfeed"
	:config
	(define-key elfeed-search-mode-map (kbd "C-i") 'helm-elfeed)
  )

;; Start Elfeed
(elfeed-db-load)
(elfeed-update)
(elfeed)

;; Start helm-elfeed
(switch-to-buffer "*elfeed-search*")
(helm-elfeed)

