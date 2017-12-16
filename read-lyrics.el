;;; read-lyrics.el --- Read lyrics for current song

;; Copyright (c) 2017 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav.tushar.vs@gmail.com>
;; Version: 3.1.2
;; Package-Requires ((enlive "0.0.1") (dash "2.13.0") (dash-functional "2.13.0") (f "0.19.0") (s "1.11.0) (spotify "0.3.3"))
;; Keywords: lyrics
;; URL: https://github.com/lepisma/read-lyrics.el

;;; Commentary:

;; read-lyrics.el lets you read lyrics of current song from multiple players
;; This file is not a part of GNU Emacs.

;;; Code:

(require 'dash)
(require 'dash-functional)
(require 'enlive)
(require 'f)
(require 'org)
(require 's)
(require 'spotify)
(require 'url-util)

(defconst read-lyrics-search-url "http://search.azlyrics.com/search.php?q=")

(defcustom read-lyrics-cache-dir (f-full "~/.lyrics/")
  "Cache directory for keeping lyrics")

(defun read-lyrics-cache-file (title artist)
  "Return file name for the cache"
  (--> (s-concat artist "--" title)
     (downcase it)
     (s-collapse-whitespace it)
     (s-trim it)
     (s-replace-all '((" " . "-")) it)
     (f-join read-lyrics-cache-dir (s-concat it ".lyr"))))

(defun read-lyrics-for (title artist)
  "Show lyrics for given song"
  (let ((cache-file (read-lyrics-cache-file title artist)))
    (if (f-exists? cache-file)
        (find-file cache-file)
      (let* ((search-url (read-lyrics-build-search-url title artist))
             (search-node (enlive-fetch search-url)))
        (if search-node
            (let ((lyrics-page-url (read-lyrics-parse-search
                                    search-node)))
              (if lyrics-page-url
                  (read-lyrics-display-page lyrics-page-url cache-file)
                (message "No lyrics results found")))
          (message "Error in search"))))))

(defun read-lyrics-parse-search (search-node)
  "Get link to first lyrics result from given node"
  (let ((result-urls (->> (enlive-query-all search-node [tr a])
                        (-map (-cut enlive-attr <> 'href))
                        (-filter (-not (-cut s-starts-with? "?q" <>))))))
    (car result-urls)))

(defun read-lyrics-build-search-url (title artist)
  "Return search url"
  (s-concat
   read-lyrics-search-url
   (url-hexify-string (concat artist " " title))))

(defun read-lyrics-display-page (lyrics-page-url cache-file)
  "Display lyrics from the page url. Also save it to cache-file."
  (f-mkdir read-lyrics-cache-dir)
  (let ((page-node (enlive-fetch lyrics-page-url)))
    (if page-node
        (let ((artist (read-lyrics-get-page-artist page-node))
              (title (read-lyrics-get-page-title page-node))
              (lyrics (read-lyrics-get-page-lyrics page-node))
              (buffer (find-file-noselect cache-file)))
          (set-buffer buffer)
          (read-lyrics-mode)
          (setq buffer-read-only nil)
          (erase-buffer)
          (insert (s-concat "* " title "\n"))
          (org-set-property "ARTIST" artist)
          (org-set-property "URL" lyrics-page-url)
          (insert "\n")
          (insert "#+BEGIN_QUOTE\n")
          (insert (s-concat (s-trim lyrics) "\n"))
          (insert "#+END_QUOTE\n")
          (setq buffer-read-only t)
          (goto-char (point-min))
          (save-buffer)
          (switch-to-buffer buffer))
      (message "Error in fetching page"))))

(defun read-lyrics-get-page-artist (page-node)
  "Get artist name from the page."
  (let ((bold-headings (enlive-get-elements-by-tag-name page-node 'b)))
    (s-chop-suffix " Lyrics" (enlive-text (car bold-headings)))))

(defun read-lyrics-get-page-title (page-node)
  "Get song name from the page."
  (let ((bold-headings (enlive-get-elements-by-tag-name page-node 'b)))
    (substring (enlive-text (second bold-headings)) 1 -1)))

(defun read-lyrics-get-page-lyrics (page-node)
  "Get lyrics from the page."
  (let* ((text (enlive-text
                (nth 4 (enlive-query-all
                        page-node
                        [div.container.main-page div.col-xs-12.col-lg-8 > div]))))
         (notice-text-end "licensing agreement. Sorry about that.")
         (notice-index (+ (string-match notice-text-end text)
                          (length notice-text-end))))
    (s-replace-all '(("" . "")) (if notice-index
                                      (string-trim (substring text notice-index))
                                    text))))

;; Now playing getters

(defun read-lyrics-get-bbq ()
  "Return artist, track pair from bbq."
  (let ((splits (mapcar (lambda (split) (s-trim (s-collapse-whitespace split)))
                        (s-split "-" (shell-command-to-string "bbq :current")))))
    (cons (car (last splits))
          (s-join " " (butlast splits)))))

(defun read-lyrics-get-spotify ()
  "Return artist, track pair or nil from spotify."
  (let ((sp-out (spotify-current)))
    (if sp-out
        (let ((splits (s-split " / " sp-out)))
          (cons
           (first splits)
           (second (s-split-up-to ": " (third splits) 1 t))))
      nil)))

(defun read-lyrics-get-mpd ()
  "Return artist, track pair or nil from mpd"
  (let ((mpc-out (s-lines (shell-command-to-string "mpc"))))
    (if (< (length mpc-out) 3)
        nil
      (let ((splits (s-split " - " (first mpc-out))))
        (cons (car splits) (second splits))))))

;; Variables
(defgroup read-lyrics nil
  "Read lyrics customization group"
  :group 'read-lyrics)

(defcustom read-lyrics-getters
  '(read-lyrics-get-bbq
    read-lyrics-get-spotify
    read-lyrics-get-mpd)
  "Current song getters in decreasing priority.
Should return a list or two items, artist and title."
  :group 'read-lyrics)

(defun read-lyrics-use-first-getter (getters)
  "Use first getter to show lyrics"
  (if getters
      (let ((current (funcall (first getters))))
        (if current
            (let ((artist (car current))
                  (title (cdr current)))
              (read-lyrics-for title artist))
          (read-lyrics-use-first-getter (cdr getters))))
    (message "No song being played")))

;;;###autoload
(defun read-lyrics ()
  "Get current playing track information"
  (interactive)
  (read-lyrics-use-first-getter read-lyrics-getters))

(defun read-lyrics-kill-buffer ()
  "Close lyrics buffer"
  (interactive)
  (kill-buffer (current-buffer)))

(defvar read-lyrics-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "r") #'read-lyrics)
    (define-key map (kbd "q") #'read-lyrics-kill-buffer)
    map)
  "Keymap for read lyrics.")

(define-derived-mode read-lyrics-mode org-mode
  "Read Lyrics"
  "Major mode for lyrics"
  (setq buffer-read-only t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lyr\\'" . read-lyrics-mode))

(provide 'read-lyrics)

;;; read-lyrics.el ends here
