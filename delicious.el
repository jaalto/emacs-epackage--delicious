;; delicious.el --- functions to make productive use of the http://del.icio.us API

;; Copyright (C) 2004, 2005 John Sullivan

;; Author: John Sullivan <john@wjsullivan.net>
;; Created 25 October 2004
;; Version: 0.2 2005-06-21
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

(defconst delicious-version  "delicious.el/0.2 2005-06-21"
  "The version string for this copy of delicious.el.")

(defconst delicious-tags-list '()
  "Table of tags for use in completion.")

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
  (let ((post (list
               (cons "href" url)
               (cons "description" description)
               (cons "tag" tags)
               (cons "extended" extended)
               (cons "time" time))))
    (delicious-post-local post)
    (message "URL posted.")))

(defun delicious-post-local (post &optional offline)
  "Add POST to the local copy.
If OFFLINE is non-nil, don't update the local timestamp."
    (save-window-excursion
      (save-excursion
        (delicious-get-posts-buffer)
        (unless offline (delicious-update-timestamp))
        (goto-char (point-max))
        (prin1 post (current-buffer))
        (let ((tags (cdr (assoc "href" post))))
          (delicious-rebuild-tags-maybe tags))
        (save-buffer))))

(defun delicious-rebuild-tags-maybe (tags)
  "If any tags in the space separated string TAGS are new, rebuild tags table."
  (let ((tags-list (split-string tags))
        (new))
    (mapc 
     (lambda (tag)
       (unless (assoc tag delicious-tags-list)
         (setq new t)))
     tags-list)
    (if new (delicious-build-tags-list))))

(defun delicious-read-time-string ()
  "Read a date string from a prompt and format it properly for the server.
 Use the current date and time if nothing entered."
  (let ((date (read-string "(Optional) Date/Time [yyyy-mm-dd hh:mm:ss]: ")))
    (if (equal date "")
	(setq date (delicious-format-time))
      (unless (string-match "^\\([1-9][0-9][0-9][0-9]\\).\\([0-1][0-9]\\).\\([0-9][0-9]\\).\\([0-9][0-9]\\).\\([0-5][0-9]\\).\\([0-5][0-9]\\)" date)
        (message "Incorrect time string format.")
        (sleep-for 1)
        (delicious-read-time-string))
      (let ((time-string (replace-match "\\1-\\2-\\3T\\4:\\5:\\6Z" t nil date)))
        time-string))))

(defun delicious-read-url (&optional offline)
  "Read a url from a prompt, suggesting an appropriate default.
Check the input to make sure it is valid and react if it is a duplicate.
If OFFLINE is non-nil, don't query the server for any information."
  (delicious-build-posts-list offline)
  (let ((url (delicious-check-input
              (read-string "(Required) URL: " (delicious-guess-url)) "URL")))
    (if (delicious-duplicate-url-p url)
        (delicious-post-duplicate-p url))
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
  "Check to see if URL is a duplicate. If so, return the post."
  (save-window-excursion
    (save-excursion
     (delicious-get-posts-buffer)
     (re-search-forward delicious-timestamp)
     (let ((dup))
       (while (and (not (eobp))
                   (not dup))
         (let ((post (read (current-buffer))))
           (if (member (cons "href" url) post)
               (setq dup post))))
       dup))))

(defun delicious-post-duplicate-p (url)
  "Return t if existing post should be replaced with URL."
  (unless 
      (y-or-n-p 
       "This URL is a duplicate.\nIf you post it again, the old tags will be replaced by the new ones.\nPost? ")
    (error "Duplicate URL not posted")))

(defun delicious-complete-tags (&optional nosuggest sofar quantity prompt-string
                                          require offline)
  "Get tags table if needed.  Do a completing read of tag input.
Blank line at the prompt ends the input.  If NOSUGGEST is non-nil, don't
suggest any tags. If SOFAR is non-nil, don't show tags entered so far.
Repeat the read QUANTITY times. If QUANTITY is nil, repeat until a blank
line is entered. If REQUIRE is non-nil, only a completion match or an empty string
are accepted as input. if OFFLINE is non-nil, don't contact the server."
  (unless delicious-tags-list
    (setq delicious-tags-list (delicious-build-tags-list)))
  (let* ((base-prompt 
           (or prompt-string
               "(Enter one at a time, blank to end.) Tag: "))
           (suggest-prompt
            (unless nosuggest
              (format "Suggested Tags: %s\n" (delicious-suggest-tags))))
           (sofar-prompt
            (unless sofar
              "Tags so far: %s\n"))
           (prompt (concat suggest-prompt sofar-prompt base-prompt)))
    (loop until (or (equal tag "")
                    (and
                     (numberp quantity)
                     (<= quantity 0)))
          for tag = (completing-read (format prompt tags)
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
       
(defun delicious-build-posts-list (&optional offline)
  "Load local copy of posts, then update if server timestamp is newer.
If OFFLINE is non-nil, don't query the server."
  (interactive)
  (message "Building delicious posts list.")
  (save-window-excursion
    (save-excursion
      (delicious-get-posts-buffer)
      (unless offline
        (when (delicious-refresh-p)
          (delete-region (point-min) (point-max))
          (insert (delicious-format-time (delicious-api-get-timestamp)))
          (mapc '(lambda (post)
                   (prin1 post (current-buffer)))
                (delicious-api-get-all))
          (save-buffer)))
    (message "Done."))))

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
      (if (and
           (eq window-system 'X)
           (x-get-selection 'CLIPBOARD)
               (string-match thing-at-point-url-regexp 
                             (x-get-selection 'CLIPBOARD)))
          (x-get-selection 'CLIPBOARD))
      "http://"))

(defun delicious-rename-tag (old-tag new-tag)
  "Change all instances of OLD-TAG to NEW-TAG.
NEW-TAG can be multiple tags, space-separated."
  (interactive 
   (list
    (delicious-complete-tags t t 1 "Enter old tag: " t)
    (delicious-complete-tags
     t "Enter new tag(s) one at a time (blank to end): ")))
  (if (or (equal old-tag "")
          (equal new-tag ""))
      (message "Aborting due to empty input.")
    (message "Renaming...")
    (delicious-api-rename old-tag new-tag)
    (delicious-build-tags-list)
    (message "Done renaming %s to %s" old-tag new-tag)))
  
(defun delicious-w3m-html (username count tag)
  "Visit the HTML page for USERNAME showing COUNT most recent posts under TAG.
With prefix, visit the page in a new w3m session."
  (interactive 
   "sUsername (RET for yours): \nsNumber of posts (RET for 15): \nsTag (RET for all): ")
  (let ((count (or (string-to-int count) 15)))
    (w3m-browse-url
     (format "http://%s%s%s" delicious-api-host delicious-api-html
             (delicious-api-html-uri username tag count))
     (not (null current-prefix-arg)))))

(defun delicious-w3m-bookmark-recent (count tag section)
  "Add your COUNT recent delicious posts under TAG to your w3m bookmarks file.
They will be stored under SECTION."
  (interactive
   "nNumber of recent posts to bookmark: \nsTag to filter by: \nsw3m bookmark section to use: ")
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
        (let* ((links 
                (loop until (looking-at w3m-bookmark-section-delimiter)
                      do (re-search-forward item-start)
                      for link = (progn
                                   (re-search-forward thing-at-point-url-regexp)
                                   (match-string 0))
                      do (re-search-forward ">")
                      for title = (buffer-substring 
                                   (point) (- (re-search-forward "</a>")  4))
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
     (define-key map [(control ?i)] 'delicious-search-next-result)
     (define-key map [(shift tab)] 'delicious-search-previous-result)
     (unless (featurep 'xemacs)
       (define-key map [(shift iso-lefttab)]
         'delicious-search-previous-result)
       (define-key map [(shift control ?i)]
         'delicious-search-previous-result))
     (define-key map [(meta ?n)] 'delicious-search-next-result)
     (define-key map [(meta ?p)] 'delicious-search-previous-result)
     (define-key map [(return)] 'browse-url-at-point)
     (define-key map [(space)] 'scroll-up)
     (define-key map [(delete)] 'scroll-down)
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
  (delete-region (point-min) (point-max)))

(defun delicious-search-buffer-finish (search-string matches)
  "Report search results in the *delicious search results* buffer.
SEARCH-STRING is the regexp pattern that was used in the search.
MATCHES is the number of matches found."
  (with-current-buffer "*delicious search results*"
    (goto-char (point-min))
    (insert (format "Your search for \"%s\" returned %d results.\n\n"
                    search-string matches))
      (delicious-search-mode 1)))

(defun delicious-search-insert-match (post)
  "Insert POST with the fields propertized."
  (with-current-buffer "*delicious search results*"
    (loop for cell in post
          do (insert 
              (propertize (cdr cell) 'face
                          (intern 
                           (concat "delicious-result-" (car cell) "-face")))
                     "\n")
          finally do (insert "\n"))))

(defun delicious-build-posts-list-maybe ()
  "Do the inital load of DELICIOUS-POSTS-LIST if needed."
  (unless (and (boundp 'delicious-posts-list)
               (not (null delicious-posts-list)))
    (delicious-build-posts-list)))

;; Offline and cached status handling

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

;; Searching posts stored offline

(defun delicious-get-posts-from-stored (tag date &optional tag-match-type)
  "Return a list of posts filtered by TAG and DATE.
TAG is a space-delimited string of tags. DATE is a regexp. 
TAG-MATCH-TYPE is either 'any' or 'all'. If it's `any', posts with any of the
tags in TAG are returned. If it's `all' (the default), only posts with all of
the tags in TAG are returned. An example of a stored date string is
`2005-04-23T20:22:55Z'."
  (unless (and (boundp 'delicious-posts-list)
               delicious-posts-list)
    (delicious-load-posts-file))
  (let ((matches '()))
    (mapc (lambda (post)
	    (when (and (delicious-test-date-regexp post date)
		       (cond ((or (null tag-match-type)
				  (eq tag-match-type 'all))
			      (delicious-test-tag-all post tag))
			     ((eq tag-match-type 'any)
			      (delicious-test-tag-any post tag))
			     (t
			      (error "Invalid tag match type"))))
	      (add-to-list 'matches post)))
	  delicious-posts-list)
    matches))

(defun delicious-test-tag-all (post tag)
  "Return t if the tag field of POST contains all the tags in TAG.
TAG is a space-delimited string."
  (let ((post-tags (split-string (cdr (assoc "tag" post))))
	(check-tags (split-string tag))
	(result t))
    (while (and result 
		(setq this-tag (pop check-tags)))
      (unless (member this-tag post-tags)
	(setq result nil)))
    result))

(defun delicious-test-tag-any (post tag)
  "Return t if the tag field of POST contains any of the tags in TAG.
TAG is a space-delimited string."
  (let ((post-tags (split-string (cdr (assoc "tag" post))))
	(check-tags (split-string tag))
	(result nil))
    (while (and (null result)
		(setq this-tag (pop check-tags)))
      (if (member this-tag post-tags)
	  (setq result t)))
    result))
   
(defun delicious-test-date-regexp (post date)
  "Return t if the time field of POST matches regexp DATE."
  (if (string-match date (cdr (assoc "time" post)))
      t))

;; Posting while offline

(defun delicious-post-offline (url description &optional tags extended time)
  "Input bookmarks to post later."
  (interactive (list
                (delicious-read-url)
                (delicious-read-description)
                (delicious-complete-tags)
                (delicious-read-extended-description)
                (delicious-read-time-string)))
  (save-window-excursion
    (save-excursion
      (find-file delicious-cache-file)
      (goto-char (point-max))
      (let ((post (list (cons "href" url)
                        (cons "description" description)
                        (cons "tag" (or tags nil))
                        (cons "extended" (or extended nil))
                        (cons "time" (or time nil)))))
        (prin1 post (current-buffer)))
      (save-buffer)))
  (if (y-or-n-p "Post another bookmark? ")
      (call-interactively 'delicious-post-offline)
    (message "Cache saved.")))

(defun delicious-post-cache (&optional cache-file)
  "Post bookmarks from `delicious-cache-file', or CACHE-FILE if non-nil."
  (interactive)
  (save-window-excursion
    (save-excursion
      (find-file (or cache-file delicious-cache-file))
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((delicious-cache-post (read (current-buffer)))
               (href (cdr (elt delicious-cache-post 0)))
               (description (cdr (elt delicious-cache-post 1)))
               (tags (cdr (elt delicious-cache-post 2)))
               (extended (cdr (elt delicious-cache-post 3)))
               (time (cdr (elt delicious-cache-post 4))))
          (delicious-api-post href description tags extended time)
          (message "%s posted." description)
          (sleep-for 2)))))
  (when (y-or-n-p "Clear cache now? ")
    (save-excursion
      (let ((buf (or cache-file delicious-cache-file)))
        (find-file buf)
        (kill-buffer (current-buffer))
        (delete-file buf)))
    (message "Cache cleared.")))

(defun delicious-clear-cache (&optional cache-file)
  "Kill buffer visiting `delicious-cache-file' or CACHE-FILE, and delete file."
  (interactive)
  (save-excursion
    (let ((buf (or cache-file delicious-cache-file)))
      (find-file buf)
      (kill-buffer (current-buffer))
      (delete-file buf)))
  (message "Cache cleared."))

(defun delicious-build-tags-list (&optional offline)
  "Build the `delicious-tags-list' table of tags and and index number,
for use in completion. If OFFLINE is non-nil, don't query the server."
  (delicious-build-posts-list offline)
  (save-window-excursion
    (save-excursion
      (delicious-get-posts-buffer)
      (re-search-forward delicious-timestamp) ; skip over the timestamp
      (setq delicious-tags-list
            (let ((index 0)
                  (tags-table '()))
              (while (not (eobp))
                (let* 
                    ((post (read (current-buffer)))
                     (tags (split-string (cdr (assoc "tag" post)))))
                  (mapc
                   (lambda (tag) ; collect tags if new
                     (unless (assoc tag tags-table)
                       (add-to-list 'tags-table (list tag index))
                       (setq index (1+ index))))
                   tags)))
              tags-table)))))
      
(defun delicious-refresh-p ()
  "Return t if server timestamp is newer than local timestamp."
  (let ((server-timestamp (delicious-api-get-timestamp))
        (local-timestamp 
         (save-window-excursion
           (save-excursion
             (delicious-get-posts-buffer)
             (delicious-get-local-timestamp-value)))))
    (time-less-p local-timestamp server-timestamp)))
 
(defun delicious-get-posts-buffer ()
  "Switch to a buffer containing `delicious-posts-file-name'."
  (let* ((home (getenv "HOME"))
         (posts-file (concat home "/" delicious-posts-file-name)))
    (find-file posts-file)
    (goto-char (point-min))))

(defconst delicious-timestamp
  (concat
   "\\([1-9][0-9]\\{3\\}\\)-" ;year
   "\\([0-1][0-9]\\)-" ;month
   "\\([0-3][0-9]\\)T" ;day
   "\\([0-2][0-9]\\):" ;hour
   "\\([0-5][0-9]\\):" ;minute
   "\\([0-5][0-9]\\)Z") ;second
  "Regular expression matching the timestamp format.")

(defun delicious-get-local-timestamp ()
  "Return the timestamp recorded in the local posts as a string."
  (let ((local-timestamp 
         (if (re-search-forward delicious-timestamp nil t)
             (match-string 0))))
    local-timestamp))

(defun delicious-get-local-timestamp-value ()
  "Return the timestamp recorded in the local posts as a time value.
Return '(0) if there is no timestamp."
  (let* ((local-timestamp (delicious-get-local-timestamp))
         (time-value 
          (if (null local-timestamp)
              (list 0)
            (string-match delicious-timestamp local-timestamp)
            (encode-time
             (string-to-int
              (match-string 6 local-timestamp)) ;seconds
             (string-to-int
              (match-string 5 local-timestamp)) ;minutes
             (string-to-int
              (match-string 4 local-timestamp)) ;hours
             (string-to-int
              (match-string 3 local-timestamp)) ;days
             (string-to-int
              (match-string 2 local-timestamp)) ;month
             (string-to-int
              (match-string 1 local-timestamp)))))) ;year
    time-value))
         
(defun delicious-update-timestamp ()
  "Update or create the local timestamp in `delicious-posts-file-name'."
  (save-window-excursion
    (save-excursion
      (delicious-get-posts-buffer)
      (let ((time (delicious-format-time)))
        (if (looking-at delicious-timestamp)
            (replace-match time)
        (insert time)))
      (save-buffer))))

(defun delicious-format-time (&optional time)
  "Return TIME as a del.icio.us timestamp.
If TIME is not provided, use the server timestamp."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" (or time (delicious-api-get-timestamp))))

(provide 'delicious)
         
;;; delicious.el ends here
