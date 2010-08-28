;;; delicioapi.el --- functions to interact with the Delicious API

;; Copyright (C) 2004, 2005, 2006, 2007, 2009 John Sullivan
;; Copyright (C) 2010 Štěpán Němec

;; Author: John Sullivan <john@wjsullivan.net>
;;         Štěpán Němec <stepnem@gmail.com>
;; Maintainer: Štěpán Němec <stepnem@gmail.com>
;; Time-stamp: "2010-08-28 11:38:34 CEST stepnem"
;; Created 25 October 2004
;; Version: 0.4FIXME
;; Keywords: comm, hypermedia

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary

;; This is a set of functions for interacting with the REST API at
;; <http://delicious.com>, a "social bookmarking" project. None of these are
;; interactive commands. There is a separate library, `delicious.el', which
;; includes the interactive commands that put these functions to use in
;; various ways. These functions are provided separately to make it convenient
;; for people to write their own front-end interactive commands.
;;
;; del.icio.us was written and is maintained by Joshua Shachter.
;;
;; Information about the API is at <http://delicious.com/help/api>.

;;; Code:

;;;;_+ Dependencies

(require 'url)
(require 'xml)
(require 'thingatpt)

;;;;_+ Variables

(defconst delicious-api-version "0.4FIXME"
  "The version string for this copy of delicioapi.el.")

(defconst delicious-api-user-agent (format "delicioapi.el/%s"
                                           delicious-api-version)
  "The User-Agent header that we will send to the server.")

(defconst delicious-api-host "api.del.icio.us"
  "The Delicious host name.")

(defconst delicious-api "/v1/"
  "The path to the Delicious API.  It should begin and end in a slash.")

(defconst delicious-timestamp
  (concat
   "\\([1-9][0-9]\\{3\\}\\)-"           ;year
   "\\([0-1][0-9]\\)-"                  ;month
   "\\([0-3][0-9]\\)T"                  ;day
   "\\([0-2][0-9]\\):"                  ;hour
   "\\([0-5][0-9]\\):"                  ;minute
   "\\([0-5][0-9]\\)Z")                 ;second
  "Regular expression matching the Delicious timestamp format.")

;; FIXME is this used anywhere?
;; (defconst delicious-api-realm (format "%s API" delicious-api-host)
;;   "The Delicious auth realm name.")

;;;;_+ API Functions

;; FIXME unused
;; (put 'delicious-api-error 'error-message "Delicious error")
;; (put 'delicious-api-error 'error-conditions '(delicious-api-error error))

;; (defun delicious-api-check-error (response)
;;   "Signal the error if RESPONSE is an API error."
;;   (unless (string= "done" (xml-get-attribute response 'code))
;;     (let* ((error-node (car (xml-get-children response 'error)))
;;            (code (xml-get-attribute error-node 'code))
;;            (message (mapconcat 'identity (xml-node-children error-node) " ")))
;;       (signal 'delicious-api-error (list code message)))))

(defun delicious-api-response (buffer)
  "Process the XML response from Delicious in BUFFER."
  (declare (special url-http-end-of-headers))
  (with-current-buffer buffer
    (set-buffer-multibyte t)
    ;; FIXME do we need utf-8-dos?
    (decode-coding-region url-http-end-of-headers (point-max) 'utf-8)
    (goto-char url-http-end-of-headers)
    (let* ((beg (point))
           (end (- (re-search-forward "<!--") 5))
           (response (car (xml-parse-region beg end))))
      (delete "\n" (delete "\n  " response)))))

(defun delicious-api-request (path)
  "Do a Delicious API request to PATH.
Return the result as parsed by `xml-parse-region', with blanks removed."
  (let ((url-package-name "delicioapi.el")
        (url-package-version delicious-api-version))
    (delicious-api-response
     (url-retrieve-synchronously
      (format "https://%s%s%s" delicious-api-host delicious-api path)))))

(defun delicious-api-get-timestamp ()
  "Return time of the last update for your Delicious user account.
The value returned is a time string as specified by `delicious-timestamp'."
  (let* ((path (format "posts/update"))
         (update (delicious-api-request path))
         (date (xml-get-attribute update 'time)))
    date))

(defun delicious-api/posts/add (url &optional description tags extended time)
  "Post a bookmark to your Delicious account.
You may include a DESCRIPTION (string), TAGS (space-separated string),
EXTENDED (extra description string) and TIME (in the format
%C%y-%m-%dT%H:%M:%SZ)."
  (let* ((description (url-hexify-string description))
         (tags (url-hexify-string tags))
         (extended (url-hexify-string extended))
         (time (url-hexify-string time))
         (post-url (format
                    "posts/add?url=%s&description=%s&tags=%s&extended=%s&dt=%s"
                    url description tags extended time)))
    (delicious-api-request post-url)))

;; FIXME unused
(defun delicious-api/tags/get (&optional tags)
  "Return your tags and the number of entries with each tag.
TAGS can be Delicious tags  in the `xml' package format.
If not provided, contact the server."
  (let* ((path "tags/get")
         (tags
          (cdr (or tags
                   (delicious-api-request path))))
         tags-list)
    (dolist (tag tags (nreverse tags-list))
      (when (listp tag) ; FIXME necessary?
        (let (cell
              (name (xml-get-attribute tag 'tag))
              (count (string-to-number (xml-get-attribute tag 'count))))
          (setq cell (cons (cons 'tag name) (cons 'count count)))
          (setq tags-list (cons cell tags-list)))))))

(defun delicious-api/posts/get (&optional tag date)
  "Return a list of posts filtered by TAG on a given DATE.
If no date is supplied, the most recent date with posts will be used."
  (let ((path (concat "posts/get?"
                      (if tag (format "tag=%s&" tag))
                      (if date (format "dt=%s" date)))))
    (delicious-api-request path)))

(defun delicious-api/posts/recent (&optional tag count)
  "Return a list, optionally filtered by TAG, of the COUNT most recent posts.
This will max out at 100. Use `delicious-api/posts/all' if you want more
than that."
  (let* ((count (cond ((null count) 15)
                      ((> count 100) 100)
                      (t count)))
         (path (concat "posts/recent?"
                       (and tag (format "tag=%s&" tag))
                       (format "count=%s" count))))
    (delicious-api-request path)))

(defun delicious-api/posts/all (&optional tag)
  "Return all posts. If TAG is non-nil, return all posts with that tag."
  (let ((path (concat "posts/all" (and tag (format "?tag=%s" tag)))))
    (delicious-api-request path)))

;; FIXME unused
(defun delicious-api/posts/dates (&optional tag)
  "Return dates with the number of posts at each date.
TAG is a tag to filter by."
  (let ((path (concat "posts/dates" (and tag (format "?tag=%s" tag)))))
    (delicious-api-request path)))

(defun delicious-api/tags/rename (old new)
  "Rename OLD tag to NEW in all posts."
  (let ((path (format "tags/rename?old=%s&new=%s" old new)))
    (delicious-api-request path)))

(defun delicious-api/posts/delete (url)
  "Delete URL from bookmarks."
  (let ((path (format "posts/delete?url=%s" url)))
    (delicious-api-request path)))

(defun delicious-api-version ()
  "Return the version of the Emacs Delicious API in use."
  (interactive)
  (message "%s" delicious-api-version))

(provide 'delicioapi)

;;; delicioapi.el ends here
