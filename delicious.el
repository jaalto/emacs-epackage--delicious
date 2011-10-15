;;; delicious.el --- functions to make productive use of the Delicious API

;; Copyright (C) 2004, 2005, 2006, 2007 John Sullivan

;; Author: John Sullivan <john@wjsullivan.net>
;;         Štěpán Němec <stepnem@gmail.com>
;; Maintainer: Štěpán Němec <stepnem@gmail.com>
;; Created: 25 October 2004
;; Version: 0.3FIXME
;; Keywords: comm, hypermedia

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary

;;; Code:

(require 'crm)
(require 'delicioapi)
(require 'delicious-html)

;;;;_+ Customization

(defgroup delicious nil
  "Functions for interacting with the Delicious API, a web application
for managing and sharing bookmarks."
  :group 'applications)

(defcustom delicious-posts-file (condition-case nil
                                    (locate-user-emacs-file ".delicious")
                                  (error "~/.delicious"))
  "Path to the file to save Delicious posts into (for internal use only)."
  :type 'string
  :group 'delicious)

(defcustom delicious-cache-file (condition-case nil
                                    (locate-user-emacs-file ".delicious-cache")
                                  (error "~/.delicious-cache"))
  "Path to the file to cache posts into for later posting to the server."
  :type 'string
  :group 'delicious)

;;;;_+ Faces
;; FIXME what about terminal?
(defface delicious-result-href-face
  '((t (:underline t :foreground "DeepSkyBlue1")))
  "Face for links in search results."
  :group 'delicious)

(defface delicious-result-description-face
  '((t (:foreground "SeaGreen2")))
  "Face for links in search results."
  :group 'delicious)

(defface delicious-result-extended-face
  '((t (:foreground "SeaGreen3")))
  "Face for links in search results."
  :group 'delicious)

;; We don't currently show the hash in the search buffer
;; (defface delicious-result-hash-face
;;   '((t (:foreground "DodgerBlue4")))
;;   "Face for the hash in search results."
;;   :group 'delicious)

(defface delicious-result-tag-face
  '((t (:foreground "LightCoral")))
  "Face for links in search results."
  :group 'delicious)

(defface delicious-result-time-face
  '((t (:foreground "DodgerBlue4")))
  "Face for timestamp in search results."
  :group 'delicious)

;;;;_+ Global stuff

(defconst delicious-version  "delicious.el/0.3FIXME"
  "The version string for this copy of delicious.el.")

(defvar delicious-tags-list nil
  "List of tags (strings) for use in completion (internal).")

(defcustom delicious-username user-login-name
  "User name of your Delicious account.
Only necessary for the feeds functions (see `delicious-w3m-html').")

(defcustom delicious-xsel-prog nil
  "Full path to a program that returns the X selection, like xsel."
  :type 'string
  :group 'delicious)

(defcustom delicious-guess-url-methods
  '(delicious-guess-check-point
    delicious-guess-check-w3m
    delicious-guess-check-buffer
    delicious-guess-check-selection
    delicious-guess-check-xsel
    delicious-guess-check-default)
  "List of functions to try, in order, to guess a URL to post."
  :type 'list
  :group 'delicious)

;;;;_+ Helper functions

(defmacro delicious-with-posts-buffer (&rest body)
  "Evaluate BODY in a buffer visiting `delicious-posts-file'.
The buffer will be put into `emacs-lisp-mode' and undo information
will be disabled."
  (declare (indent 0) (debug t))
  `(with-current-buffer (find-file-noselect delicious-posts-file)
     (or (eq buffer-undo-list t)
         (buffer-disable-undo))
     (or (derived-mode-p 'emacs-lisp-mode)
         (emacs-lisp-mode))
     (progn ,@body)))

(defsubst delicious-goto-posts ()
  "Go to the position where the posts start.
This assumes the buffer visiting `delicious-posts-file' is current."
  (goto-char (point-min))
  (down-list 1)
  (forward-sexp 2))

(defsubst delicious-get-next-post ()
  "Return the next post.
This assumes the buffer visiting `delicious-posts-file' is current."
  (condition-case nil
      (cadr (read (current-buffer)))
    ((end-of-file invalid-read-syntax) nil)))

(defsubst delicious-get-post-field (field post)
  "Return the value of FIELD (a symbol) in POST.
POST is a Delicious post as returned by `delicious-get-next-post'."
  (assoc-default field post 'eq))

;;;;_+ Posting

;;;_+ Online and offline

(defun delicious-build-posts-list (&optional offline force)
  "Load local copy of posts, then update if server timestamp is newer.
If OFFLINE is non-nil, don't query the server.
If FORCE is non-nil, or if a prefix is given interactively, skip the
timestamp comparison and force a refresh from the server."
  (interactive)
  (if (and offline force)
      (error "Can't force an update while offline"))
  (cond (offline
         (message "Reading local posts list..."))
        ((or force current-prefix-arg)
         (message "Reading posts from server..."))
        (t
         (message "Reading posts from last-modified source...")))
  (delicious-with-posts-buffer
    (unless offline
      (when (or force
                current-prefix-arg
                (delicious-refresh-p))
        (let ((posts (delicious-api/posts/all)))
          (erase-buffer)
          (prin1 posts (current-buffer)))
        (save-buffer))))
  (message "Done"))

(defun delicious-post (url description &optional tags extended time nolocal)
  "Post a bookmark with arguments URL, DESCRIPTION, TAGS, EXTENDED, and TIME.
If NOLOCAL is non-nil, don't add the post to the local list."
  (interactive (list
                (delicious-read-url)
                (delicious-read-description)
                (delicious-read-tags url)
                (delicious-read-extended-description)
                (delicious-read-time-string)))
  (message "Waiting for server")
  (delicious-api/posts/add url description tags extended time)
  (unless nolocal
    (delicious-post-local (list 'post
                                (list
                                 (cons 'href url)
                                 (cons 'description description)
                                 (cons 'extended extended)
                                 (cons 'hash (md5 url))
                                 (cons 'tag tags)
                                 (cons 'time time))))
    (message "URL posted")))

(defun delicious-post-local (post &optional offline)
  "Add POST to the local copy.
If we already had a post with the same hash as POST, delete it first.
If OFFLINE is non-nil, don't update the local timestamp."
  (unless offline (delicious-update-timestamp))
  (delicious-with-posts-buffer
    (delicious-goto-posts)
    (let ((maybe-point (delicious-find-hash-post
                        (delicious-get-post-field 'hash (cadr post)))))
      (and maybe-point (delete-region (point) maybe-point))
      (delicious-goto-posts)
      (prin1 post (current-buffer))
      (save-buffer)))
  (let ((tags (split-string (delicious-get-post-field 'tag (cadr post)))))
    (delicious-rebuild-tags-maybe tags t)))

(defun delicious-find-hash-post (hash)
  "Set point past post with HASH in current buffer and return its beginning position."
  (let (post)
    (delicious-goto-posts)
    (catch 'found
      (while (setq post (delicious-get-next-post))
        (when (string= hash (delicious-get-post-field 'hash post))
          (throw 'found (scan-sexps (point) -1)))))))

(defun delicious-edit-post-locally (hash fields)
  "Replace old information in local copy of post with HASH using FIELDS.
FIELDS is a list of cons cells, with each cell being a field name
and value.
Return the updated post."
  (delicious-with-posts-buffer
    (delicious-goto-posts)
    (let (maybe-beg)
      (unless (setq maybe-beg (delicious-find-hash-post hash))
        (error "No such post"))
      (let ((end (point))
            (post (progn (goto-char maybe-beg)
                         (cadr (read (current-buffer))))))
        (delete-region maybe-beg end)
        (dolist (cell fields)
          (let ((field (car cell))
                (value (cdr cell)))
            (setcdr (assq field post) value)))
        (prin1 (list 'post post) (current-buffer))
        (delicious-update-timestamp)
        (list 'post post)))))

(defun delicious-check-input (input &optional name)
  "Verify that INPUT has content.
NAME is the name of the field being checked."
  (if (equal input "")
      (error "%s was a required field" name)
    input))

;;;_+ Offline

(defun delicious-post-offline (url description &optional tags extended time)
  "Input bookmarks to post later.  Don't contact the server for anything."
  (interactive (list (delicious-read-url t)
                     (delicious-read-description)
                     (delicious-read-tags url nil nil t)
                     (delicious-read-extended-description)
                     (delicious-read-time-string)))
  (with-current-buffer (find-file-noselect delicious-cache-file)
    (goto-char (point-max))
    (let ((post (list 'post (list (cons 'href url)
                                  (cons 'description description)
                                  (cons 'extended extended)
                                  (cons 'hash (md5 url))
                                  (cons 'tag tags)
                                  (cons 'time time)))))
      (prin1 post (current-buffer)))
    (save-buffer))
  (when (y-or-n-p "Post another bookmark? ")
    (call-interactively 'delicious-post-offline))
  (message "Cache saved"))

(defun delicious-post-cache (&optional cache-file)
  "Post bookmarks from `delicious-cache-file', or CACHE-FILE if non-nil."
  (interactive)
  (let* ((cache-file (or cache-file delicious-cache-file))
         (buf (if (file-exists-p cache-file)
                  (find-file-noselect cache-file)
                (error "Cache file %s not found" cache-file))))
    (with-current-buffer buf
      (delicious-goto-posts)
      (while (setq post (delicious-get-next-post))
        (let* ((href (delicious-get-post-field 'href post))
               (description (delicious-get-post-field 'description post))
               (tags (delicious-get-post-field 'tag post))
               (extended (delicious-get-post-field 'extended post))
               (time (delicious-get-post-field 'time post)))
          (delicious-api/posts/add href description tags extended time)
          (delicious-post-local (list 'post post))
          (message "%s posted" description)
          (sleep-for 2))))
    (when (y-or-n-p "Clear cache now? ")
      (kill-buffer buf)
      (delete-file cache-file)
      (message "Cache cleared"))))

(defun delicious-clear-cache (&optional cache-file)
  "Kill buffer visiting `delicious-cache-file' or CACHE-FILE, and delete the file."
  (interactive)
  (let* ((file (or cache-file delicious-cache-file))
         (buffer (find-buffer-visiting file)))
    (and buffer (kill-buffer buffer))
    (and file (delete-file file)))
  (message "Cache cleared"))

;;;_+ Timestamps

(defun delicious-read-time-string ()
  "Prompt for a date string and format it properly for the server.
 Use the current date and time if nothing entered."
  (let ((date (read-string "(Optional) Date/Time [yyyy-mm-dd hh:mm:ss]: ")))
    (unless
        (or (equal date "")
            (string-match
             "^\\([1-9][0-9]\\{3\\}\\).\\([0-1][0-9]\\).\\([0-3][0-9]\\).\\([0-2][0-9]\\).\\([0-5][0-9]\\).\\([0-5][0-9]\\)"
             date))
      (message "Incorrect time string format")
      (sleep-for 1)
      ;; FIXME what about getting rid of the recursive calls?
      (delicious-read-time-string))
    (if (equal date "")
        (setq date (delicious-format-time (current-time)))
      (setq date (replace-match "\\1-\\2-\\3T\\4:\\5:\\6Z" t nil date)))
    date))

(defun delicious-refresh-p ()
  "Return t if server timestamp is newer than local timestamp."
  (let ((server-timestamp (delicious-api-get-timestamp))
        (local-timestamp (delicious-get-local-timestamp)))
    (or (null local-timestamp)
        (string< local-timestamp server-timestamp))))

(defun delicious-get-local-timestamp ()
  "Return the timestamp recorded in the local posts as a string."
  (delicious-with-posts-buffer
    (goto-char (point-min))
    (condition-case nil
        (assoc-default 'update (cadr (read (current-buffer))) 'eq)
      (end-of-file nil))))

(defun delicious-update-timestamp ()
  "Update or create the local timestamp in `delicious-posts-file'."
  (delicious-with-posts-buffer
    (goto-char (point-min))
    (let ((time (delicious-api-get-timestamp)))
      (re-search-forward delicious-timestamp)
      (replace-match time)
      (save-buffer))))

(defun delicious-format-time (time)
  "Return TIME as a Delicious timestamp."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" time))

;;;;_+ URL input, guessing, and duplicate checking

(defun delicious-read-url (&optional offline)
  "Read a URL from a prompt, suggesting an appropriate default.
Check the input to make sure it is valid and react if it is a duplicate.
If OFFLINE is non-nil, don't query the server for any information."
  (let ((url (delicious-check-input
              (read-string "(Required) URL: " (delicious-guess-url)) "URL")))
    (delicious-build-posts-list offline)
    (and (delicious-get-url-post url)
         (unless
             (y-or-n-p
              ;; FIXME multiline...
              (concat "This URL is a duplicate.\nIf you post it again, "
                      "old tags will be replaced by the new ones.\nPost? "))
           (error "Duplicate URL, not posted")))
    url))

(defun delicious-guess-url ()
  (let ((methods delicious-guess-url-methods)
        guess)
    (while (and (not guess)
                methods)
      (setq guess (funcall (car methods))
            methods (cdr methods)))
    guess))

(defun delicious-guess-check-w3m ()
  "If we're in a w3m buffer, use the current URL."
  (if (and (boundp 'w3m-current-url)
           w3m-current-url
           (derived-mode-p 'w3m-mode))
      w3m-current-url))

(defun delicious-guess-check-point ()
  "If point is on a URL, return it."
  (if (thing-at-point-looking-at thing-at-point-url-regexp)
      (thing-at-point-url-at-point)))

(defun delicious-guess-check-buffer ()
  "Check the buffer for a URL and return it."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward thing-at-point-url-regexp nil t)
        (match-string-no-properties 0))))

(defun delicious-guess-check-selection ()
  "Check the X selection for a URL and return it."
  (let (selection url)
    (when (eq window-system 'x)
      (setq selection (condition-case nil
                          (x-get-selection)
                        (error nil)))
      (when (and selection
                 (string-match thing-at-point-url-regexp selection))
        (setq url (match-string-no-properties 0 selection))))))

(defun delicious-guess-check-xsel ()
  "Check output of `delicious-xsel-prog' for a URL."
  (when delicious-xsel-prog
    (let ((selection (shell-command-to-string delicious-xsel-prog)))
      (if (string-match thing-at-point-url-regexp selection)
          (match-string-no-properties 0 selection)))))

(defun delicious-guess-check-default ()
  "Return some text to use for the URL guess."
  "http://")

(defun delicious-get-url-post (url)
  "Return the post with href field equal to URL."
  (let (post)
    (delicious-with-posts-buffer
      (delicious-goto-posts)
      (catch 'match
        (while (setq post (delicious-get-next-post))
          (when (string= url (delicious-get-post-field 'href post))
            (throw 'match post)))))))

;;;;_+ Description input and suggestion

(defun delicious-read-description ()
  "Prompt for a description, suggesting an appropriate default."
  (delicious-check-input
   (read-string "(Required) Description: " (delicious-guess-description))
   "Description"))

(defvar gnus-current-headers)
(defun delicious-guess-description ()
  "Try some different things to get a default description."
  (or (if (and (boundp 'w3m-current-title)
               (not (null w3m-current-title))
               (derived-mode-p 'w3m-mode))
          w3m-current-title)
      (if (derived-mode-p 'gnus-summary-mode 'gnus-article-mode)
          (aref gnus-current-headers 1))))

;;;;_+ Extended description

(defun delicious-read-extended-description (&optional suggest truncated)
  "Prompt for an extended description."
  (let ((ext (read-string
              (concat
               (when truncated
                 "Trimmed to fit 253 characters; please edit or accept.\n")
               "(Optional) Extended Description: ") suggest)))
    (if (> (length ext) 253)
        (delicious-read-extended-description (substring ext 0 252) t)
      ext)))

;;;;_+ Tag completion, suggestion, and manipulation

(defun delicious-read-tags (&optional url prompt-prefix existing offline)
  "Read tag(s) for URL (or all user tags if nil) in the minibuffer.
Tags should be comma-separated (cf. `completing-read-multiple').
Suggested tag completions (if any) are annotated with \"<S>\".

All arguments are optional: PROMPT-PREFIX should be the prefix
after which the list of suggested tags is appended; defaults to
\"Tag(s)\". If EXISTING is non-nil, only accept already existing
tags. If OFFLINE is non-nil, don't query the server.

Returns a string consisting of the tags read separated by
spaces."
  (unless delicious-tags-list
    (setq delicious-tags-list (delicious-build-tags-list offline)))
  (mapconcat
   'identity
   (let* ((suggestags
           (when url
             (apply 'append
                    (mapcar 'cdr (delicious-api/posts/suggest url t)))))
          (prompt (concat (or prompt-prefix "Tag(s)")
                          (when suggestags
                            (concat " [suggested: "
                                    (mapconcat 'identity suggestags " ")
                                    "]"))
                          ": "))
          (completion-annotate-function
           ;; GNU Emacs bug#8897 :-(
           (lambda (c) (when (member c suggestags) " <S>"))))
     (completing-read-multiple prompt
                               (append delicious-tags-list suggestags)
                               nil existing))
   " "))

(defun delicious-rebuild-tags-maybe (tags &optional offline)
  "Rebuild `delicious-tags-list' if it misses any of TAGS.
TAGS is a string \"TAG1 TAG2...\" or a list of strings.
If OFFLINE is non-nil, don't query the server."
  (and (stringp tags) (setq tags (split-string tags)))
  (when (catch 'new
          (dolist (tag tags)
            (unless (member tag delicious-tags-list)
              (throw 'new t))))
    (delicious-build-tags-list offline)))

(defun delicious-build-tags-list (&optional offline)
  "Build the `delicious-tags-list' for use in completion.
If OFFLINE is non-nil, don't query the server."
  (delicious-build-posts-list offline)
  (delicious-with-posts-buffer
    (delicious-goto-posts)
    (setq delicious-tags-list
          (let (tags-list
                post
                tags)
            (while (setq post (delicious-get-next-post))
              (setq tags (split-string (delicious-get-post-field 'tag post)))
              (and tags
                   (mapc (lambda (tag)
                           (add-to-list 'tags-list tag))
                         tags)))
            tags-list))))

;; FIXME and what about syncing?
(defun delicious-rename-tag (old-tag new-tag)
  "Change all instances of OLD-TAG to NEW-TAG.
NEW-TAG can be multiple tags, space-separated (interactively, the
tags are read comma-separated using the minibuffer)."
  (interactive
   (list
    (delicious-read-tags nil "Old tag" t)
    (delicious-read-tags nil "New tag(s)")))
  (if (or (equal old-tag "")
          (equal new-tag ""))
      (message "Aborting due to empty input")
    (message "Renaming...")
    (delicious-api/tags/rename old-tag new-tag)
    (delicious-build-tags-list)
    (message "Done renaming %s to %s" old-tag new-tag)))

;;;;_+ Deleting and editing posts

(defun delicious-delete-href-post (href)
  "Delete the post with URL HREF."
  (interactive "sEnter URL to delete: ")
  (delicious-api/posts/delete href)
  (delicious-delete-href-post-locally href)
  (message "%s deleted" href))

(defun delicious-delete-href-post-locally (url)
  "Delete the first local copy of the post with href field URL."
  (let (post)
    (delicious-with-posts-buffer
      (delicious-goto-posts)
      (when (catch 'match
              (while (setq post (delicious-get-next-post))
                (when (string= url (delicious-get-post-field 'href post))
                  (throw 'match post))))
        (delete-region (point) (scan-sexps (point) -1))
        (delicious-update-timestamp)))))

;;; FIXME use a local list of `meta's instead?
'(defun delicious-delete-hash-post-locally (hash)
  "Delete local copy of the post with hash field HASH."
  (let (post)
    (delicious-with-posts-buffer
      (delicious-goto-posts)
      (when (catch 'match
              (while (setq post (delicious-get-next-post))
                (when (string= hash (delicious-get-post-field 'hash post))
                  (throw 'match post))))
        (delete-region (point) (scan-sexps (point) -1))
        (delicious-update-timestamp)))))

;;;;_+ w3m
(defvar w3m-bookmark-file)
(defvar w3m-bookmark-section-delimiter)
(declare-function w3m-bookmark-sections "w3m-bookmark" nil)
(declare-function w3m-bookmark-write-file "w3m-bookmark"(url title section))
(declare-function w3m-browse-url "w3m" (url &optional new-session))

(defun delicious-w3m-html (username count tag)
  "Visit the HTML page for USERNAME showing COUNT most recent posts with TAG.
With a prefix argument, visit the page in a new w3m session."
  (interactive
   "sUsername (RET for yours): \nsNumber of posts (RET for 15): \nsTag (RET for all): ")
  (let ((count (or (string-to-int count) 15)))
    (w3m-browse-url
     (format "http://%s%s%s" delicious-feeds-host delicious-api-html
             (delicious-api-html-uri username tag count))
     current-prefix-arg)))

(defun delicious-w3m-bookmark-recent (count tag section)
  "Add your COUNT recent Delicious posts with TAG to your w3m bookmarks file.
They will be stored under SECTION."
  (interactive
   "nNumber of recent posts to bookmark: \nsTag to filter by: \nsw3m bookmark section to use: ")
  (let ((response (delicious-api/posts/recent tag count)))
    (with-temp-buffer
      (prin1 response (current-buffer))
      (goto-char (point-min))
      (delicious-goto-posts)
      (while (setq post (delicious-get-next-post))
        (w3m-bookmark-write-file
         (delicious-get-post-field 'href post)
         (delicious-get-post-field 'description post)
         section))))
  (message "w3m bookmarks updated"))

(defun delicious-w3m-export (section &optional tags extended time)
  "Export your w3m bookmarks from SECTION to Delicious.
Optionally assign TAGS, EXTENDED description, and TIME to the bookmarks."
  (interactive (list (completing-read "Section to export (required): "
                                      (w3m-bookmark-sections) nil t)
                     (delicious-read-tags)
                     (delicious-read-extended-description)
                     (delicious-read-time-string)))
  (let ((section-string (format "<h2>%s</h2>" section))
        (item-start "<li><a")
        bmks)
    (with-temp-buffer
      (insert-file-contents w3m-bookmark-file)
      (goto-char (point-min))
      (re-search-forward section-string)
      (while (not (looking-at w3m-bookmark-section-delimiter))
        (re-search-forward item-start)
        (let ((link (progn
                      (re-search-forward thing-at-point-url-regexp)
                      (match-string 0)))
              (title (progn
                       (re-search-forward ">")
                       (buffer-substring
                        (point) (- (re-search-forward "</a>")  4)))))
          (setq bmks (cons (cons link title) bmks))
          (beginning-of-line 2)))
      (dolist (bmk bmks)
        (delicious-api/posts/add (car bmk) (cdr bmk) tags extended time)
        (message "%s posted" (cdr bmk))
        (sleep-for 1)))))

;;;;_+ Searching

;;; FIXME error handling -- can really some fields be missing?

;;;_+ Search mode and results buffer

(defvar delicious-search-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab] 'delicious-search-next-result)
    (define-key map [(control ?i)] 'delicious-search-next-result)
    (define-key map [(shift tab)] 'delicious-search-previous-result)
    (unless (featurep 'xemacs)
      (define-key map [(shift iso-lefttab)]
        'delicious-search-previous-result)
      (define-key map [(shift control ?i)]
        'delicious-search-previous-result))
    (define-key map "n" 'delicious-search-next-result)
    (define-key map "p" 'delicious-search-previous-result)
    (define-key map " " 'scroll-up)
    (define-key map "\C-?" 'scroll-down)
    (define-key map "q" 'bury-buffer)
    map)
  "Keymap for `delicious-search-mode'.")

(define-derived-mode delicious-search-mode fundamental-mode "Delicious Search")

(defun delicious-search-build-buffer (what match-count matches)
  "Build a *Delicious search results* buffer.
Switch to it and turn on `delicious-search-mode'."
  (let ((buffer (get-buffer-create "*Delicious search results*")))
    (unless (eq buffer (current-buffer))
      (set-buffer buffer))
    (let ((inhibit-read-only t)
          (result (if (= match-count 1) "result" "results")))
      (delete-region (point-min) (point-max))
      (mapc 'delicious-search-insert-entry matches)
      (goto-char (point-min))
      (insert (format "Your search for \"%s\" returned %d %s.\n\n"
                      what match-count result)))
    (toggle-read-only 1)
    (delicious-search-mode)
    (pop-to-buffer buffer)))

(defun delicious-search-insert-entry (post)
  "Insert POST with the fields propertized into the current buffer."
  (let ((href (delicious-get-post-field 'href post))
        (hash (delicious-get-post-field 'hash post))
        (description (delicious-get-post-field 'description post))
        (tag (delicious-get-post-field 'tag post))
        (time (delicious-get-post-field 'time post))
        (extended (delicious-get-post-field 'extended post))
        (meta (delicious-get-post-field 'meta post))
        (map (make-sparse-keymap)))
    (define-key map "D" 'delicious-search-delete)
    (define-key map "a" 'delicious-search-add-tags)
    (define-key map "c" 'delicious-search-copy-url)
    (define-key map "d" 'delicious-search-delete-tags)
    (define-key map "e" 'delicious-search-edit-extended)
    (define-key map (kbd "RET") 'delicious-search-browse-url)
    (insert
     (propertize
      (concat (propertize href 'face 'delicious-result-href-face) "\n"
              (propertize description 'face 'delicious-result-description-face) "\n"
              (propertize tag 'face 'delicious-result-tag-face) "\n"
              (propertize extended 'face 'delicious-result-extended-face) "\n"
              (propertize time 'face 'delicious-result-time-face) "\n\n")
      'keymap map
      'href href
      'hash hash
      'description description
      'tag tag
      'time time
      'extended extended
      'meta meta))))

(defun delicious-search-browse-url ()
  "Browse URL of the current Delicious entry using `browse-url'."
  (interactive)
  (browse-url (get-text-property (point) 'href)))

;; FIXME These two functions are wonderfully different both in implementation
;; and in behaviour. I blame the wonderfully strange behaviour of the relevant
;; Emacs functionality.
(defun delicious-search-next-result ()
  "Go to the next search result in the Delicious search results buffer.
Signals an error if there is no result at all."
  (interactive)
  (let* ((hunk (lambda ()
                 (text-property-any (point) (point-max)
                                    'face 'delicious-result-href-face)))
         (pos (save-excursion
                (or (progn
                      (forward-line 2)
                      (funcall hunk))
                    (progn
                      (goto-char (point-min))
                      (funcall hunk))))))
    (if pos (goto-char pos)
      (error "No next result"))))

(defun delicious-search-previous-result ()
  "Go to the previous search result in the Delicious search results buffer."
  (interactive)
  (let ((opoint (point))
        (hunk (lambda ()
                (while (search-backward-regexp "[a-z]+?://.+?" (point-min) t)
                  (goto-char (line-beginning-position))
                  (when (looking-at
                         (regexp-quote (or (get-text-property (point) 'href)
                                           "")))
                    (throw 'gotcha nil))))))
    (catch 'gotcha
      (unless (funcall hunk)
        (goto-char (point-max)))
      (unless (funcall hunk)
        (goto-char opoint)))))

(defun delicious-search-copy-url ()
  "Copy URL of the current entry to the kill ring."
  (interactive)
  (let ((url (get-text-property (point) 'href)))
    (kill-new url)
    (message url)))

;;;_+ Editing posts

(defun delicious-bounds-of-entry (hash)
  "Helper for Delicious Search mode functions.
Return bounds of entry with HASH in current buffer as (BEG . END).
Signal an error when no such entry is found."
  (let ((pos (save-excursion
               (goto-char (point-min))
               (text-property-any (point) (point-max) 'hash hash))))
    (if pos
        (cons pos (1- (next-single-property-change pos 'hash nil (point-max))))
      (error "No entry with hash %s" hash))))

(defun delicious-search-update-entry (hash fields)
  "Helper for Delicious Search mode functions.
Update the entry hash of which is HASH with FIELDS."
  (let* ((edit-post (delicious-edit-post-locally hash fields))
         (bounds (delicious-bounds-of-entry hash))
         (inhibit-read-only t))
    (delete-region (car bounds) (cdr bounds))
    (delicious-search-insert-entry (cadr edit-post))
    (search-backward (cdar fields))))

(defun delicious-search-delete ()
  "Delete the post under point."
  (interactive)
  (let* ((hash (get-text-property (point) 'hash))
         (href (get-text-property (point) 'href))
         (bounds (delicious-bounds-of-entry hash))
         (inhibit-read-only t))
    (delicious-delete-href-post href)
    (delete-region (car bounds) (cdr bounds))))

(defun delicious-search-add-tags (tags update)
  "Add TAGS to the post under point in Delicious Search mode.
If UPDATE is non-nil, update the post's timestamp."
  (interactive (list (delicious-read-tags)
                     (y-or-n-p "Update timestamp? ")))
  (let* ((hash (get-text-property (point) 'hash))
         (post (delicious-get-hash-post hash))
         (href (delicious-get-post-field 'href post))
         (desc (delicious-get-post-field 'description post))
         (old-time (delicious-get-post-field 'time post))
         (new-time (if update (delicious-format-time (current-time))
                     old-time))
         (extended (or (delicious-get-post-field 'extended post) ""))
         (old-tags (delicious-get-post-field 'tag post))
         (new-tags (concat old-tags " " tags)))
    (delicious-post href desc new-tags extended new-time t)
    (delicious-rebuild-tags-maybe new-tags)
    (delicious-search-update-entry hash (list (cons 'tag new-tags)
                                              (cons 'time new-time)))))

(defun delicious-search-delete-tags (tags update)
  "Delete TAGS from the post under point in Delicious Search mode.
If UPDATE is non-nil, update the post's timestamp."
  (interactive (list (delicious-read-tags)
                     (y-or-n-p "Update timestamp? ")))
  (let* ((hash (get-text-property (point) 'hash))
         (post (delicious-get-hash-post hash))
         (href (delicious-get-post-field 'href post))
         (desc (delicious-get-post-field 'description post))
         (old-time (delicious-get-post-field 'time post))
         (new-time (if update (delicious-format-time (current-time))
                     old-time))
         (extended (or (delicious-get-post-field 'extended post) ""))
         (old-tags (split-string (delicious-get-post-field 'tag post)))
         (delete-tags (split-string tags))
         (new-tags
          (mapconcat 'identity
                     (let (l) (dolist (tag old-tags (nreverse l))
                                (unless (member tag delete-tags)
                                  (setq l (cons tag l)))))
                     " ")))
    (delicious-post href desc new-tags extended new-time t)
    (delicious-rebuild-tags-maybe new-tags)
    (delicious-search-update-entry hash (list (cons 'tag new-tags)
                                              (cons 'time new-time)))))

(defun delicious-search-edit-extended (ext update)
  "Edit the extended description under point."
  (interactive (list nil (y-or-n-p "Update timestamp? ")))
  (let* ((hash (get-text-property (point) 'hash))
         (post (delicious-get-hash-post hash))
         (href (delicious-get-post-field 'href post))
         (desc (delicious-get-post-field 'description post))
         (old-time (delicious-get-post-field 'time post))
         (new-time (if update (delicious-format-time (current-time))
                     old-time))
         (tag (delicious-get-post-field 'tag post))
         (extended (or ext (delicious-get-post-field 'extended post) ""))
         (new-ext (delicious-read-extended-description ext)))
    (delicious-post href desc tag new-ext new-time t)
    (delicious-search-update-entry hash (list (cons 'extended new-ext)
                                              (cons 'time new-time)))))

;;;_+ This does all the dirty work

(defun delicious-search (what lambda)
  "Helper for all the other search functions.
Look at their definitions for example usage.

WHAT is the thing we're searching for.

LAMBDA is the function to call with `post' bound to the currently
processed Delicious post, as returned by `delicious-get-next-post',
i.e. alist of the form ((FIELD1 . VALUE1) (FIELD2 . VALUE2) ...).

LAMBDA is expected to set `match' to the post in case it satisfied
whatever it's testing for. It can also set `continue' to nil to
prevent processing of further posts.

See also `delicious-get-post-field'."
  (let (post (continue t) match matches (match-count 0))
    (delicious-build-posts-list current-prefix-arg)
    (delicious-with-posts-buffer
      (delicious-goto-posts)
      (while (and continue (setq post (delicious-get-next-post)))
        (funcall lambda)
        (when match
          (setq match-count (1+ match-count))
          (setq matches (cons match matches))
          (setq match nil))))
    (delicious-search-build-buffer what match-count matches)))

;;;_+ Search by regexp

(defun delicious-search-posts-regexp (regexp)
  "Display all posts matching REGEXP string in any of their fields.
With a prefix argument, operate offline."
  (interactive "sEnter regexp search string: ")
  (delicious-search
   regexp
   (lambda ()
     (when (catch 'match
             (dolist (field post)
               (when (string-match regexp (cdr field))
                 (throw 'match t))))
       (setq match post)))))

(defun delicious-search-description-regexp (regexp)
  "Display all posts matching REGEXP string in their description fields.
With a prefix argument, operate offline."
  (interactive "sEnter regexp search string: ")
  (delicious-search
   regexp
   (lambda ()
     (when (string-match regexp (delicious-get-post-field 'description post))
       (setq match post)))))

(defun delicious-search-href-regexp (regexp)
  "Display all posts with URL matching REGEXP.
With a prefix argument, operate offline."
  (interactive "sEnter regexp search string: ")
  (delicious-search
   regexp
   (lambda ()
     (when (string-match regexp (delicious-get-post-field 'href post))
       (setq match post)))))

;;;_+ Search by tag

(defun delicious-search-tags (tags)
  "Display all posts with TAGS.  With a prefix argument, operate offline."
  (interactive (list (delicious-read-tags)))
  (let ((taglist (split-string tags)))
    (delicious-search
     tags
     (lambda ()
       (let ((post-tags (split-string
                         (or (delicious-get-post-field 'tag post) ""))))
         (setq match post)
         (mapc (lambda (tag)
                 (unless (member tag post-tags)
                   (setq match nil)))
               taglist))))))

(defun delicious-search-tags-any (tags)
  "Display all posts matching any of TAGS."
  (interactive (list (delicious-read-tags)))
  (let ((taglist (split-string tags)))
    (delicious-search
     tags
     (lambda ()
       (let ((post-tags (split-string
                         (or (delicious-get-post-field 'tag post) ""))))
         (when (catch 'match
                 (dolist (tag taglist)
                   (when (member tag post-tags)
                     (throw 'match t))))
           (setq match post)))))))

;;;_+ Search by date

(defun delicious-search-date (date)
  "Display all posts matching regexp SEARCH-DATE.
With a prefix argument, operate offline."
  (interactive "sEnter the date (regexp): ")
  (delicious-search
   date
   (lambda ()
     (when (string-match date (delicious-get-post-field 'time post))
       (setq match post)))))

;;;_+ Search by hash

(defun delicious-search-hash (hash)
  "Display the post with hash HASH.
With a prefix argument, operate offline."
  (interactive "sEnter the hash: ")
  (delicious-search
   hash
   (lambda ()
     (when (string= hash (delicious-get-post-field 'hash post))
       (setq match post
             continue nil)))))

(defun delicious-get-hash-post (hash)
  "Return the post with hash HASH."
  (let (post)
    (delicious-build-posts-list)
    (delicious-with-posts-buffer
      (delicious-goto-posts)
      (catch 'match
        (while (setq post (delicious-get-next-post))
          (when (string= hash (delicious-get-post-field 'hash post))
            (throw 'match post)))))))

(provide 'delicious)
;;; delicious.el ends here
