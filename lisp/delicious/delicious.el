;;; delicious.el --- functions to make productive use of the http://del.icio.us API

;; Copyright (C) 2004, 2005 John Sullivan

;; Author: John Sullivan <john@wjsullivan.net>
;; Created 25 October 2004
;; Version: 0.1 2005-01-14
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
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

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

(defvar delicious-posted-urls '()
  "A running list of urls that have been posted since the last update of the list from the delicious server.")

(defconst delicious-version  "delicious.el/0.1 2005-01-14"
  "The version string for this copy of delicious.el.")

(defun delicious-post (url description &optional tags extended time)
  ;; figure out how to get right time-string format
  "Post a url to your del.icio.us account with arguments URL, DESCRIPTION, TAGS, EXTENDED, and TIME."
  (interactive (list
                (delicious-read-url)
                (delicious-read-description)
                (delicious-complete-tags)
                (delicious-read-extended-description)
                (delicious-read-time-string)))
  (message "Waiting for server.")
  (delicious-api-post url description tags extended time)
  (add-to-list 'delicious-posted-urls url)
  (message "URL posted."))

(defun delicious-read-time-string ()
  "Read a date string from a prompt and format it properly for the server.
The server uses the current date and time by default."
  (read-string "(Optional) Date/Time [yyyy-mm-ddThh:mm:ssZ]: "))

(defun delicious-read-url ()
  "Read a url from a prompt, suggesting an appropriate default.  Check the input to make sure it is valid and react if it is a duplicate."
  (let ((url (delicious-check-input (read-string "(Required) URL: " (delicious-guess-url)) "URL")))
    (delicious-duplicate-url-p url)
    url))

(defun delicious-read-description ()
  "Read a description from a prompt, suggesting an appropriate default."
  (delicious-check-input (read-string "(Required) Description: " (delicious-guess-description)) "Description"))

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
  (unless (and (boundp 'delicious-posts-list)
               (not (null delicious-posts-list)))
    (delicious-build-posts-list))
  (if (or (assoc url delicious-posts-list)
          (member url delicious-posted-urls))
      (progn (let ((edit
                    (y-or-n-p "This URL is a duplicate.\nIf you post it again, the old tags will be replaced by the new ones.\nPost? ")))
               (if (eq edit t)
                   (setq duplicate nil)
                 (error "Duplicate URL not posted"))
               duplicate))))

(defun delicious-complete-tags (&optional nosuggest)
  "Get tags table if needed, and do a completing read of tag input until a blank line is entered. If NOSUGGEST is non-nil, don't suggest any tags."
  ;; add check. if tag was not part of the completion table, add it immediately.
  (unless (and (boundp 'delicious-tags-list)
               (not (null delicious-tags-list)))
    (delicious-build-tags-list))
  (loop with suggested-tags = (if nosuggest ""
                               (delicious-suggest-tags))
        with prompt = (if nosuggest 
                         "%sTags so far: %s\n(Enter one at a time, blank to end.) Tag: "
                       "Suggested Tags: %s\nTags so far: %s\n(Enter one at a time, blank to end.) Tag: ")
        for tag = (completing-read (format prompt suggested-tags tags)
                                   delicious-tags-list)
        until (equal tag "")
        collect tag into tags
        finally return (progn
                         (add-to-list 'delicious-tags-local tags)
                         (message "%s" tags)
                         (mapconcat 'identity tags " "))))

(defun delicious-suggest-tags ()
  "Suggest tags based on the intersection of the contents of the current buffer and the current list of tags."
  (let ((buffer-words (delicious-buffer-words))
        (tags (loop for cell in delicious-tags-list
                    collect (downcase (car cell)) into tags
                    finally return tags)))
    (loop for word in buffer-words
          if (member word tags)
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
  (setq delicious-tags-list (delicious-api-build-tag-completion)
        delicious-tags-local '()))

(defun delicious-build-posts-list ()
  "Refresh or build the posts list from the server for use in duplicate checking."
  (interactive)
  (message "Refreshing delicious posts list from server.")
  (setq delicious-posts-list (delicious-api-get-all)
        delicious-posted-urls '()))

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
  "Try some different things to guess a url.  If we're in a w3m buffer, use the current url.  If not, use the url under point.  If not that, search through the buffer and see if there is a url to use in the buffer.  If not that, just insert http:// into the prompt."
  ;; if there is a prefix, maybe it should use the kill ring
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
      "http://"))

(defun delicious-w3m-html (username count tag)
  "Visit the HTML feed page, in a new-session if a prefix is used, for the del.icio.us USERNAME showing COUNT most recent posts under TAG."
  (interactive "sUsername (RET for yours): \nnNumber of posts (RET for 15): \nsTag (RET for all): ")
  (w3m-browse-url 
   (format "http://%s%s%s" delicious-api-host delicious-api-html (delicious-api-html-uri username tag count))
   (not (null current-prefix-arg))))

(defun delicious-w3m-bookmark-recent (count tag section)
  "Add your COUNT recent delicious posts under TAG to your w3m bookmarks file. They will be stored under SECTION."
  (interactive "nNumber of recent posts to bookmark: \nsTag to filter by: \nsw3m bookmark section to use: ")
  (let ((delicious-list (delicious-api-get-recent tag count)))
    (loop for bookmark in delicious-list
          for url = (nth 0 bookmark)
          for title = (nth 1 bookmark)
          do (w3m-bookmark-write-file url title section)
          finally do (message "w3m bookmarks updated."))))

(defun delicious-w3m-export (section &optional tags extended time)
  "Export your w3m bookmarks from SECTION to del.icio.us, assigning TAGS to each entry. Optionally enter an EXTENDED description and a TIME."
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

(provide 'delicious)

;;; delicious.el ends here
