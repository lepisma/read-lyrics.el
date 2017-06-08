;;; read-lyrics.el --- Read lyrics for current song

;; Copyright (c) 2017 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav.tushar.vs@gmail.com>
;; Version: 2.1
;; Package-Requires ((enlive "0.0.1") (s "1.11.0) (spotify "0.3.3"))
;; Keywords: lyrics
;; URL: https://github.com/lepisma/read-lyrics.el

;;; Commentary:

;; read-lyrics.el lets you read lyrics of current song from multiple players
;; This file is not a part of GNU Emacs.

;;; Code:

(require 'enlive)
(require 's)
(require 'spotify)


(defconst read-lyrics-search-url "http://search.azlyrics.com/search.php?q=")

(defvar read-lyrics-buffer-name "*Lyrics*"
  "Lyrics buffer name")

(defun read-lyrics-mode ()
  "Major mode for displaying lyrics."
  (kill-all-local-variables)
  (setq major-mode 'read-lyrics-mode)
  (setq mode-name read-lyrics-buffer-name)
  (run-hooks 'read-lyrics-mode-hook))

(defun read-lyrics-for (title artist)
  "Show lyrics for given song"
  (let* ((search-url (read-lyrics-build-search-url title artist))
         (search-node (enlive-fetch search-url)))
      (if search-node
          (let ((lyrics-page-url (read-lyrics-parse-search
                                  search-node)))
            (if lyrics-page-url
                (read-lyrics-display-page lyrics-page-url)
              (message "No lyrics results found")))
        (message "Error in search"))))

(defun read-lyrics-parse-search (search-node)
  "Get link to first lyrics result from given node"
  (let ((result-urls (enlive-query-all search-node [tr a])))
    (if result-urls
        (enlive-attr (car result-urls) 'href)
      nil)))

(defun read-lyrics-build-search-url (title artist)
  "Return search url"
  (s-concat
   read-lyrics-search-url
   (s-replace " " "+" (concat artist " " title))))

(defun read-lyrics-display-page (lyrics-page-url)
  "Display lyrics from the page url"
  (let ((page-node (enlive-fetch lyrics-page-url)))
    (if page-node
        (let ((artist (read-lyrics-get-page-artist page-node))
              (title (read-lyrics-get-page-title page-node))
              (lyrics (read-lyrics-get-page-lyrics page-node))
              (buffer (get-buffer-create read-lyrics-buffer-name)))
          (set-buffer buffer)
          (setq buffer-read-only nil)
          (erase-buffer)
          (read-lyrics-mode)
          (insert "\n")
          (insert (propertize title
                              'face '(:inherit variable-pitch
                                               :foreground "DeepSkyBlue"
                                               :height 1.6)))
          (insert "\n")
          (insert (propertize artist
                              'face '(:inherit variable-pitch
                                               :height 1.0
                                               :weight bold
                                               :foreground "gray")))
          (insert "\n\n")
          (setq text-start (point))
          (insert (propertize lyrics
                              'face '(:inherit variable-pitch
                                               :height 1.1
                                               :slant italic
                                               :foreground "DeepPink1")))
          (switch-to-buffer buffer)
          (add-text-properties text-start (point-max) '(line-spacing 0.4))
          (delete-trailing-whitespace)
          (setq buffer-read-only t)
          (goto-char (point-min)))
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
    (if notice-index
        (string-trim (substring text notice-index))
      text)))

;; Now playing getters

(defun read-lyrics-get-spotify ()
  "Return artist, track pair or nil from spotify."
  (let ((sp-out (spotify-current)))
    (if sp-out
        (let ((splits (s-split " / " sp-out)))
          (list
           (first splits)
           (second (s-split-up-to ": " (third splits) 1 t))))
      nil)))

(defun read-lyrics-get-mpd ()
  "Return artist, track pair or nil from mpd"
  (let ((mpc-out (s-lines (shell-command-to-string "mpc"))))
    (if (< (length mpc-out) 3)
        nil
      (s-split " - " (first mpc-out)))))

(defun read-lyrics-get-blackbird ()
  "Return artist, track pair or nil from blackbird.
Not Implemented."
  nil)

;; Variables
(defgroup read-lyrics nil
  "Read lyrics customization group"
  :group 'read-lyrics)

(defcustom read-lyrics-getters
  '(read-lyrics-get-spotify
    read-lyrics-get-mpd
    read-lyrics-get-blackbird)
  "Current song getters in decreasing priority.
Should return a list or two items, artist and title."
  :group 'read-lyrics)

(defun read-lyrics-use-first-getter (getters)
  "Use first getter to show lyrics"
  (if getters
      (let ((current (funcall (first getters))))
        (if current
            (let ((artist (first current))
                  (title (second current)))
              (read-lyrics-for title artist))
          (read-lyrics-use-first-getter (cdr getters))))
    (message "No song being played")))

;;;###autoload
(defun read-lyrics ()
  "Get current playing track information"
  (interactive)
  (read-lyrics-use-first-getter read-lyrics-getters))

(provide 'read-lyrics)

;;; read-lyrics.el ends here
