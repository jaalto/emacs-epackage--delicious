;;; delicious.el --- functions to make productive use of the http://del.icio.us API

;; Copyright (C) 2004 John Sullivan

;; Author: John Sullivan <john@wjsullivan.net>
;; Created 25 October 2004
;; Version: 0.1 2004-12-22
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

;; Notes to myself

;; (format-time-string "%C%y-%m-%dT%H:%M:%SZ")))
;; add offline caching of requests so that they can be dumped all at once
;; write a function to post a buffer full of urls
;; write something that lets you edit an entry, probably by deleting the old version and posting the new version
;; I complained before about the BBDB entry system that uses the minibuffer. Should I have at least an option
;;  for a form-based entry system in a pop-up window?

;;; Code:

(require 'delicioapi)

(defvar delicious-posted-urls '()
  "A running list of urls that have been posted since the last update of the list from the delicious server.")

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
    (message "Refreshing delicious post list from server.")
    (setq delicious-posts-list (delicious-api-get-all)
	  delicious-posted-urls '()))
  (if (or (assoc url delicious-posts-list)
	  (member url delicious-posted-urls))
      (progn
	(let
	    ((edit
	      (y-or-n-p "This URL is a duplicate.\nIf you post it again, the old tags will be replaced by the new ones.\nPost? ")))
	  (if (eq edit t)
	      (setq duplicate nil)
;	    (setq duplicate t))
	    (error "Duplicate URL not posted"))
	  duplicate))))

(defun delicious-complete-tags ()
  "Get tags table if needed, and do a completing read of tag input until a blank line is entered."
  ;; add check. if tag was not part of the completion table, add it immediately.
  (unless (and (boundp 'delicious-tags-list)
               (not (null delicious-tags-list)))
    (delicious-build-tags-list))
  (loop for tag = (completing-read
                   (format "Tags so far: %s\n(Optional. Enter one at a time, blank to end.) Tag: " tags)
                   delicious-tags-list)
        until (equal tag "")
        collect tag into tags
        finally return (progn
	         (add-to-list 'delicious-tags-local tags)
                         (message "%s" tags)
                         (mapconcat 'identity tags " "))))

(defun delicious-build-tags-list ()
  "Refresh or build the tags table for use in completion."
  (interactive)
  (message "Refreshing delicious tags list from server.")
  (setq delicious-tags-list (delicious-api-build-tag-completion)
        delicious-tags-local '()))

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

(provide 'delicious)

;;; delicious.el ends here
