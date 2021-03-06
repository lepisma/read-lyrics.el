;;; read-lyrics.el --- Read lyrics for current song

;; Copyright (c) 2017 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav.tushar.vs@gmail.com>
;; Version: 3.2.0
;; Package-Requires ((levenshtein) (enlive "0.0.1") (dash "2.13.0") (dash-functional "2.13.0") (f "0.19.0") (s "1.11.0"))
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
(require 'json)
(require 'levenshtein)
(require 'org)
(require 's)
(require 'url-util)

(defgroup read-lyrics nil
  "Read lyrics customization group"
  :group 'read-lyrics)

(defconst read-lyrics-search-url "http://search.azlyrics.com/search.php?q=")

(defcustom read-lyrics-cache-dir (f-full "~/.cache/read-lyrics.el/")
  "Cache directory for keeping lyrics"
  :group 'read-lyrics)

(defcustom read-lyrics-getters (list #'read-lyrics-get-interactive)
  "Current song getters in decreasing priority.
Should return a list or two items, artist and title."
  :group 'read-lyrics)

(defun read-lyrics-get-interactive ()
  "Interactive getter for title and artist. Use this as the
  fallback."
  (let ((artist (read-string "artist: "))
        (title (read-string "title: ")))
    (unless (string= title "")
      (cons artist title))))

(defun read-lyrics-cache-file (artist title)
  "Return file name for the cache"
  (--> (s-concat artist "--" title)
     (downcase it)
     (s-collapse-whitespace it)
     (s-trim it)
     (s-replace-all '((" " . "-")) it)
     (f-join read-lyrics-cache-dir (s-concat it ".lyr"))))

(defun read-lyrics-text-match? (original to-test)
  "Test if TO-TEST is sufficiently close to ORIGINAL."
  (let ((thresh 0.3))
    (> thresh (/ (levenshtein-distance original to-test) (* 1.0 (length original))))))

(defun read-lyrics-browser-fallback (artist title)
  "Open a fallback search page in browser"
  (let ((encoded-string (url-hexify-string (format "%s %s lyrics" title artist))))
    (browse-url (format "https://duckduckgo.com/?q=%s" encoded-string))))

(defun read-lyrics-parse-search (search-node)
  "Get link to first lyrics result from given node"
  (let ((result-urls (->> (enlive-query-all search-node [tr a])
                        (-map (-cut enlive-attr <> 'href))
                        (-filter (-not (-cut s-starts-with? "?q" <>))))))
    (car result-urls)))

(defun read-lyrics-build-search-url (artist title)
  "Return search url"
  (s-concat read-lyrics-search-url (url-hexify-string (s-concat artist " " title))))

(defun read-lyrics-display-lyrics (artist title lyrics cache-file)
  "Display lyrics for data. Also save it to cache-file."
  (f-mkdir read-lyrics-cache-dir)
  (let ((buffer (find-file-noselect cache-file)))
    (set-buffer buffer)
    (read-lyrics-mode)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (s-concat "* " title "\n"))
    (org-set-property "ARTIST" artist)
    (insert "\n")
    (insert "#+BEGIN_QUOTE\n")
    (insert (s-concat (s-trim lyrics) "\n"))
    (insert "#+END_QUOTE\n")
    (setq buffer-read-only t)
    (goto-char (point-min))
    (save-buffer)
    (switch-to-buffer buffer)))

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

(defun read-lyrics-get-now-playing (&optional getters)
  "Return now-playing based on the list of getter functions."
  (when getters
    (-if-let (result (funcall (car getters)))
        result
      (read-lyrics-get-now-playing (cdr getters)))))

(defun read-lyrics-for (artist title)
  "Show lyrics for given song"
  (let ((cache-file (read-lyrics-cache-file artist title)))
    (if (f-exists? cache-file)
        (find-file cache-file)
      (let* ((search-url (read-lyrics-build-search-url artist title))
             (search-node (enlive-fetch search-url)))
        (if search-node
            (let ((lyrics-page-url (read-lyrics-parse-search search-node)))
              (if lyrics-page-url
                  (let ((lyrics-page-node (enlive-fetch lyrics-page-url)))
                    (if lyrics-page-node
                        (let ((page-artist (read-lyrics-get-page-artist lyrics-page-node))
                              (page-title (read-lyrics-get-page-title lyrics-page-node))
                              (page-lyrics (read-lyrics-get-page-lyrics lyrics-page-node)))
                          (if (and (read-lyrics-text-match? title page-title)
                                   (read-lyrics-text-match? artist page-artist))
                              (read-lyrics-display-lyrics page-artist page-title page-lyrics cache-file)
                            (message "No lyrics found. Opening browser.")
                            (read-lyrics-browser-fallback artist title)))
                      (message "Error in fetching page")))
                (message "No lyrics found. Opening browser.")
                (read-lyrics-browser-fallback artist title)))
          (message "Error in search"))))))

;;;###autoload
(defun read-lyrics ()
  "Get current playing track information"
  (interactive)
  (-if-let (result (read-lyrics-get-now-playing read-lyrics-getters))
      (read-lyrics-for (car result) (cdr result))
    (message "No track detected.")))

;;;###autoload
(defun read-lyrics-interactive ()
  (interactive)
  (let ((read-lyrics-getters (list #'read-lyrics-get-interactive)))
    (read-lyrics)))

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

;;;###autoload
(define-derived-mode read-lyrics-mode org-mode
  "Read Lyrics"
  "Major mode for lyrics"
  (setq buffer-read-only t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lyr\\'" . read-lyrics-mode))

(provide 'read-lyrics)

;;; read-lyrics.el ends here
