;;; delicious.el --- functions to make productive use of the http://del.icio.us API

;; Copyright (C) 2004, 2005 John Sullivan

;; Author: John Sullivan <john@wjsullivan.net>
;; Created 25 October 2004
;; Version: 0.2 2005-05-04
;; Keywords: comm, hypermedia

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, a copy is available at
;; http://www.wjsullivan.net, or write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301 USA

;;; Commentary

;; You should read the instructions in the source for `delicioapi.el' to get
;; the necessary setup instructions. This file is the collection of functions
;; I'm working on to actually put the API to productive use. Many things in
;; here are half-assed and half-finished, so it's probably not that productive
;; for anyone to contribute at the moment, but feature ideas and bug reports
;; are much appreciated.
;;
;; You don't need to add anything to your `.emacs' beyond what you already
;; added as per the instructions in `delicioapi.el' for these functions to
;; work. Just (require 'delicious).

;; Please report any bugs or suggestions to me at
;; john@wjsullivan.net. If enough people are interested, perhaps we
;; will open an area at http://www.emacswiki.org.

;;; Code:

(require 'delicioapi)

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

(defface delicious-result-hash-face
  '((t (:foreground "DodgerBlue4")))
  "Face for the hash in search results."
  :group 'delicious)

(defface delicious-result-tag-face
  '((t (:foreground "LightCoral")))
    "Face for links in search results."
    :group 'delicious)

(defface delicious-result-time-face
  '((t (:foreground "DodgerBlue4")))
  "Face for timestamp in search results."
  :group 'delicious)

(defconst delicious-version  "delicious.el/0.2 2005-05-04"
  "The version string for this copy of delicious.el.")

(defun delicious-post (url description &optional tags extended time)
  ; figure out how to get right time-string format
  "Post a url with arguments URL, DESCRIPTION, TAGS, EXTENDED, and TIME."
  (interactive (list
                (delicious-read-url)
                (delicious-read-description)
                (delicious-complete-tags)
                (delicious-read-extended-description)
                (delicious-read-time-string)))
  (message "Waiting for server.")
  (delicious-api-post url description tags extended time)
  (loop with post = (list `("href" . ,url)
                          `("description" . ,description)
                          `("tag" . ,tags)
                          `("extended" . ,extended)
                          `("time" . ,time))
        for cell in post
        unless (equal (cdr cell) "")
            collect cell into cells
        finally do (add-to-list 'delicious-posts-list cells t)
        finally do (delicious-add-tags tags))
  (message "URL posted."))

(defun delicious-read-time-string ()
  "Read a date string from a prompt and format it properly for the server.
The server uses the current date and time by default."
  (let ((date (read-string "(Optional) Date/Time [yyyy-mm-dd hh:mm:ss]: ")))
    (unless (equal date "")
      (unless (string-match "^\\([1-9][0-9][0-9][0-9]\\).\\([0-1][0-9]\\).\\([0-9][0-9]\\).\\([0-9][0-9]\\).\\([0-5][0-9]\\).\\([0-5][0-9]\\)" date)
        (message "Incorrect time string format.")
        (sleep-for 1)
        (delicious-read-time-string))
      (let ((time-string (replace-match "\\1-\\2-\\3T\\4:\\5:\\6Z" t nil date)))
        time-string))))

(defun delicious-read-url ()
  "Read a url from a prompt, suggesting an appropriate default.
Check the input to make sure it is valid and react if it is a duplicate."
  (let ((url (delicious-check-input
              (read-string "(Required) URL: " (delicious-guess-url)) "URL")))
    (delicious-duplicate-url-p url)
    url))

(defun delicious-read-description ()
  "Read a description from a prompt, suggesting an appropriate default."
  (delicious-check-input
   (read-string "(Required) Description: " (delicious-guess-description)) "Description"))

(defun delicious-check-input (input &optional name)
  "Verify that INPUT has content.  NAME is the name of the field being checked."
  (if (equal input "")
      (error "%s was a required field" name)
    input))

(defun delicious-read-extended-description ()
  "Read an extended description from a prompt."
  (read-string "(Optional) Extended Description: "))

(defun delicious-duplicate-url-p (url)
  "Check to see if URL is a duplicate."
  (delicious-build-posts-list-maybe)
  (if (assoc `(href . ,url) delicious-posts-list)
      (progn (let ((edit
                    (y-or-n-p "This URL is a duplicate.\nIf you post it again, the old tags will be replaced by the new ones.\nPost? ")))
               (if (eq edit t)
                   (setq duplicate nil)
                 (error "Duplicate URL not posted"))
               duplicate))))

(defun delicious-complete-tags (&optional nosuggest sofar quantity prompt-string
                                          require)
  "Get tags table if needed.  Do a completing read of tag input.
Blank line at the prompt ends the input.  If NOSUGGEST is non-nil, don't
suggest any tags. If SOFAR is non-nil, don't show tags entered so far.
Repeat the read QUANTITY times. If QUANTITY is nil, repeat until a blank
line is entered. If REQUIRE is non-nil, only a completion match or an empty string
are accepted as input."
  (unless (and (boundp 'delicious-tags-list)
               (not (null delicious-tags-list)))
    (delicious-build-tags-list))
  (let* ((base-prompt 
           (or prompt-string
               "(Enter one at a time, blank to end.) Tag: "))
           (suggest-prompt
            (unless nosuggest
              (format "Suggested Tags: %s\n" (delicious-suggest-tags))))
           (sofar-prompt
            (unless sofar
              "%sTags so far: %s\n"))
           (prompt (concat suggest-prompt sofar-prompt base-prompt)))
    (loop until (or (equal tag "")
                    (and
                     (numberp quantity)
                     (<= quantity 0)))
          for tag = (completing-read (format prompt suggested-tags tags)
                                     delicious-tags-list nil require)
          if (numberp quantity) do (setq quantity (1- quantity))
          if (member tag tags) do (and (message "Duplicate tag ignored.") 
                                       (sleep-for 1))
          else collect tag into tags
          finally return (progn
                           (message "%s" tags)
                           (mapconcat 'identity tags " ")))))

(defun delicious-suggest-tags ()
  "Suggest tags based on the contents of the current buffer and the current list of tags."
  (let ((buffer-words (delicious-buffer-words))
        (tags (loop for cell in delicious-tags-list
                    collect (downcase (car cell)) into tags
                    finally return tags)))
    (loop for word in buffer-words
          if (or (member word tags)
                 (member (concat word "s") tags)
                 (member (concat word "es") tags)
                 (if (and (> (length word) 1)
                          (or (member (substring word 0 -1) tags)
                              (if (and (equal (substring word -1) "y")
                                       (member (concat (substring word 0 -1) "ies") tags))
                                  t)))
                     t)
                 (if (and (> (length word) 2)
                          (member (substring word 0 -2) tags)) t))
          collect word into shared
          finally return shared)))
  
(defun delicious-buffer-words ()
  "Break the current buffer into a list of lowercase unique words."
  (save-excursion
    (goto-char (point-min))
    (loop until (eobp)
          with words = '()
          for word = (downcase (current-word))
          if (not (member word words))
            collect word into words
            end
          do (forward-word 1)
          finally return words)))

(defun delicious-build-tags-list ()
  "Refresh or build the tags table for use in completion."
  (interactive)
  (message "Refreshing delicious tags list from server.")
  (setq delicious-tags-list (delicious-api-build-tag-completion)))

(defun delicious-add-tags (tags)
  "Add TAGS to the local copy of the tags list in DELICIOUS-TAGS-LIST."
  (let ((tags-list (split-string tags)))
    (mapc '(lambda (tag)
             (let ((tag-count (cadar (last delicious-tags-list))))
               (unless (assoc tag delicious-tags-list)
                 (let ((new-tag (list tag (1+ tag-count))))
                   (add-to-list 'delicious-tags-list new-tag t)))))
          tags-list)))
       
(defun delicious-build-posts-list ()
  "Refresh or build the posts list from the server for use in duplicate checking."
  (interactive)
  (message "Refreshing delicious posts list from server.")
  (setq delicious-posts-list (delicious-api-get-all)))

(defun delicious-guess-description ()
  "Try some different things to get a default description."
  (or
   (if (and (boundp 'w3m-current-url)
            (not (null w3m-current-url))
            (eq major-mode 'w3m-mode))
       (w3m-current-title))
   (if (memq major-mode '(gnus-summary-mode gnus-article-mode))
       (aref gnus-current-headers 1))))

(defun delicious-guess-url ()
  "Try to guess a url based on buffer context.
If we're in a w3m buffer, use the current url.
If not, use the url under point.
If not that, see if there is a url in the buffer.
If not that, use the clipboard if it's a url.
If not that, just insert http:// into the prompt."
  ; if there is a prefix, maybe it should use the kill ring
  (or (if (and (boundp 'w3m-current-url)
               (not (null w3m-current-url))
               (eq major-mode 'w3m-mode))
          w3m-current-url)
      (if (thing-at-point-looking-at thing-at-point-url-regexp)
          (thing-at-point-url-at-point))
      (save-excursion
        (goto-char (point-min))
        (loop until (eobp)
         if (thing-at-point-looking-at thing-at-point-url-regexp)
              return (thing-at-point-url-at-point)
                else
              do (forward-char)))
      (if (string-match thing-at-point-url-regexp (x-get-selection 'CLIPBOARD))
          (x-get-selection 'CLIPBOARD))
      "http://"))

(defun delicious-w3m-html (username count tag)
  "Visit the HTML page for USERNAME showing COUNT most recent posts under TAG.
With prefix, visit the page in a new w3m session."
  (interactive "sUsername (RET for yours): \nnNumber of posts (RET for 15): \nsTag (RET for all): ")
  (w3m-browse-url
   (format "http://%s%s%s" delicious-api-host delicious-api-html
           (delicious-api-html-uri username tag count))
   (not (null current-prefix-arg))))

(defun delicious-w3m-bookmark-recent (count tag section)
  "Add your COUNT recent delicious posts under TAG to your w3m bookmarks file.
They will be stored under SECTION."
  (interactive "nNumber of recent posts to bookmark: \nsTag to filter by: \nsw3m bookmark section to use: ")
  (let ((delicious-list (delicious-api-get-recent tag count)))
    (loop for bookmark in delicious-list
          for url = (cdr (assoc "href" bookmark))
          for title = (cdr (assoc "description" bookmark))
          do (w3m-bookmark-write-file url title section)
          finally do (message "w3m bookmarks updated."))))

(defun delicious-w3m-export (section &optional tags extended time)
  "Export your w3m bookmarks from w3m SECTION to del.icio.us.
Optionally assign TAGS, an EXTENDED description, and TIME to the bookmarks."
  (interactive (list
                (delicious-w3m-read-section)
                (delicious-complete-tags t)
                (delicious-read-extended-description)
                (delicious-read-time-string)))
  (let* ((section-string (format "<h2>%s</h2>" section))
         (item-start "<li><a"))
    (save-excursion
      (with-temp-buffer
        (insert-file-contents w3m-bookmark-file)
        (goto-char (point-min))
        (re-search-forward section-string)
        (let* ((links (loop until (looking-at w3m-bookmark-section-delimiter)
                            do (re-search-forward item-start)
                            for link = (progn
                                         (re-search-forward thing-at-point-url-regexp)
                                         (match-string 0))
                            do (re-search-forward ">")
                            for title = (buffer-substring (point) (- (re-search-forward "</a>")  4))
                            for link = (cons link title)
                            collect link into links
                            do (beginning-of-line 2)
                            finally return links)))
          (loop for cell in links
                for link = (car cell)
                for title = (cdr cell)
                do (delicious-api-post link title tags extended time)
                do (message "%s posted." title)
                do (sleep-for 1)))))))

(defun delicious-w3m-read-section ()
  "Prompt for the name of a w3m bookmark section."
  (let* ((completions (w3m-bookmark-sections)))
    (completing-read "Section to export (required): " completions nil t)))

(defvar delicious-search-mode-map
   (let ((map (make-sparse-keymap)))
     (define-key map [tab] 'delicious-search-next-result)
     map)
   "Keymap for function `delicious-search-mode'.")

(define-minor-mode delicious-search-mode
  "Toggle Delicious Search mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When Delicious Search mode is enabled, the tab key
advances to the next search result."
  nil
  " Delicious Search"
  delicious-search-mode-map)

(defun delicious-search-next-result ()
  "Goto the next search result in a delicious search results list."
  (interactive)
  (if (thing-at-point-url-at-point)
      (goto-char (match-end 0)))
  (let ((last-match (match-beginning 0)))
    (re-search-forward thing-at-point-url-regexp nil t)
    (if (equal (match-beginning 0) last-match)
        (goto-char (point-min))
      (goto-char (match-beginning 0)))))

(defun delicious-search-previous-result ()
  "Goto the previous search result in a delicious search results list."
  (interactive)
  (if (thing-at-point-url-at-point)
      (goto-char (match-beginning 0)))
  (let ((last-match (match-beginning 0)))
    (re-search-backward thing-at-point-url-regexp nil t)
    (if (equal (match-beginning 0) last-match)
        (goto-char (point-max))
      (goto-char (match-beginning 0)))))

(defun delicious-search-posts-regexp (search-string)
  "Search DELICIOUS-POSTS-LIST for SEARCH-STRING, a regular expression.
Display the results in *delicious search results*."
  (interactive "sEnter regexp search string: ")
  (delicious-build-posts-list-maybe)
  (delicious-search-buffer-prep)
  (let ((match-count (loop for post in delicious-posts-list
                           for match = (loop for field in post
                                             when (string-match search-string
                                                                (cdr field))
                                             return post)
                           if match do (delicious-search-insert-match post)
                           count match)))
    (delicious-search-buffer-finish search-string match-count)))

(defun delicious-search-description-regexp (search-string)
  "Search DELICIOUS-POSTS-LIST for descriptions matching SEARCH-STRING.
Display the results in *delicious search results*."
  (interactive "sEnter regexp search string: ")
  (delicious-build-posts-list-maybe)
  (delicious-search-buffer-prep)
  (let ((match-count (loop for post in delicious-posts-list
                           for match = (loop for field in post
                                             if (equal (car field) "description")
                                                 when (string-match search-string
                                                                    (cdr field)) return post)
                           if match do (delicious-search-insert-match post)
                           count match)))
  (delicious-search-buffer-finish search-string match-count)))

(defun delicious-search-tags-regexp (search-string)
  "Search DELICIOUS-POSTS-LIST for tags matching SEARCH-STRING.
Display the results in *delicious search results*."
  (interactive "sEnter regexp search string: ")
  (delicious-build-posts-list-maybe)
  (delicious-search-buffer-prep)
  (let ((match-count (loop for post in delicious-posts-list
                           for match = (loop for field in post
                                             if (equal (car field) "tag")
                                                 when (string-match search-string
                                                                    (cdr field)) return post)
                           if match do (delicious-search-insert-match post)
                           count match)))
  (delicious-search-buffer-finish search-string match-count)))

(defun delicious-search-href-regexp (search-string)
  "Search DELICIOUS-POSTS-LIST for urls matching SEARCH-STRING.
Display the results in *delicious search results*."
  (interactive "sEnter regexp search string: ")
  (delicious-build-posts-list-maybe)
  (delicious-search-buffer-prep)
  (let ((match-count (loop for post in delicious-posts-list
                           for match = (loop for field in post
                                             if (equal (car field) "href")
                                                 when (string-match search-string
                                                                    (cdr field)) return post)
                           if match do (delicious-search-insert-match post)
                           count match)))
  (delicious-search-buffer-finish search-string match-count)))

(defun delicious-search-tags (tags)
  "Display all posts with TAGS, which can include regular expression syntax."
  (interactive (list (delicious-complete-tags t)))
  (delicious-build-posts-list-maybe)
  (delicious-search-buffer-prep)
  (let* ((tags (split-string tags " "))
         (matches (loop for post in delicious-posts-list
                        for match = (loop for tag in tags
                                          with post-tags = (cdr (assoc "tag" post))
                                          unless (string-match tag post-tags)
                                              return 1)
                        unless (equal match 1) do (delicious-search-insert-match post)
                        count (not match))))
    (delicious-search-buffer-finish tags matches)))

(defun delicious-search-buffer-prep ()
  "Prepare a *delicious search results* buffer for output."
  (unless (equal (buffer-name) "*delicious search results*")
    (switch-to-buffer-other-window
     (get-buffer-create "*delicious search results*")))
  (view-mode -1)
  (delete-region (point-min) (point-max)))

(defun delicious-search-buffer-finish (search-string matches)
  "Report search results in the *delicious search results* buffer.
SEARCH-STRING is the regexp pattern that was used in the search.
MATCHES is the number of matches found."
  (with-current-buffer "*delicious search results*"
    (goto-char (point-min))
    (insert (format "Your search for \"%s\" returned %d results.\n\n"
                    search-string matches))
    (view-mode 1)
    (delicious-search-mode 1)))

(defun delicious-search-insert-match (post)
  "Insert POST with the fields propertized."
  (with-current-buffer "*delicious search results*"
    (loop for cell in post
          do (insert (propertize (cdr cell) 'face
                                 (intern (concat "delicious-result-" (car cell) "-face")))
                     "\n")
          finally do (insert "\n"))))

(defun delicious-build-posts-list-maybe ()
  "Do the inital load of DELICIOUS-POSTS-LIST if needed."
  (unless (and (boundp 'delicious-posts-list)
               (not (null delicious-posts-list)))
    (delicious-build-posts-list)))

(defun delicious-save-posts ()
  "Write posts to `delicious-posts-file-name' in your $HOME directory."
  (interactive)
  (save-excursion
    (let* ((home (getenv "HOME"))
           (buffer delicious-posts-file-name)
           (file (concat home "/" buffer)))
      (set-buffer
       (get-buffer-create buffer))
      (erase-buffer)
      (prin1 delicious-posts-list (current-buffer))
      (write-file file)
      (kill-buffer buffer)
      (message "del.icio.us posts successfully saved to %s" file))))

(defun delicious-load-posts-file ()
  "Read posts from `delicious-posts-file-name' into DELICIOUS-POSTS-LIST."
  (interactive)
  (save-excursion
    (with-temp-buffer
      (let* ((home (getenv "HOME"))
             (file (concat home "/" delicious-posts-file-name)))
        (insert-file-contents file)
        (setq delicious-posts-list (read (buffer-string))))))
  (message "Done reading posts."))

(defun delicious-get-posts-from-stored (tag date)
  "Return a list of posts filtered by TAG and DATE.
TAG should be a regexp matching a space-separated group of tags.
DATE should be a regexp. An example of a stored date string
is `2005-04-23T20:22:55Z'."
;; The list is HREF, DESCRIPTION, EXTENDED, HASH, TAG, and TIME.
  (unless (and (boundp 'delicious-posts-list)
               delicious-posts-list)
    (delicious-load-posts-file))
  (let ((matches '()))
    (mapc (lambda (post)
            (if (and (string-match tag (cdr (assoc "tag" post)))
                     (string-match date (cdr (assoc "time" post))))
                (add-to-list 'matches post)))
          delicious-posts-list)
    matches))
  
(provide 'delicious)
         
;;; delicious.el ends here


