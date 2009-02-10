;;; delicioapi.el --- functions to interact with the http://del.icio.us API

;; Copyright (C) 2004, 2005, 2006, 2007, 2009 John Sullivan

;; Author: John Sullivan <john@wjsullivan.net>
;; Created 25 October 2004
;; Version: 0.4
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
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301 USA.

;;; Commentary

;; This is a complete set of functions for interacting with the REST
;; API at <http://del.icio.us>, a "social bookmarking" project.  None of
;; these are interactive commands. There is a separate program,
;; `delicious.el', which includes the interactive commands that put
;; these functions to work in various ways. These functions are
;; provided separately to make it convenient for people to write their
;; own front-end interactive commands.
;;
;; The API does go through frequent changes; I've made an effort here
;; to structure things so that these changes can easily be
;; accommodated. That's part of why some things are done in separate
;; functions that could probably be done together in one.
;;
;; del.icio.us was written and is maintained by Joshua Shachter.
;;
;; Information about the API is at <http://del.icio.us/help/api>.
;;
;; Please report any bugs or suggestions to me at
;; <john@wjsullivan.net>. If enough people are interested, perhaps we
;; will open an area at <http://www.emacswiki.org>.
;; Latest versions are available at <http://www.wjsullivan.net/delicious-el.html>.

;;; Code:

;;;;_+ Dependencies

(require 'url)
(require 'xml)
(require 'thingatpt)
(require 'rest-api)

;;;;_+ Variables

(defconst delicious-api-version "0.4"
  "The version string for this copy of delicioapi.el.")

(defvar delicious-api-user-agent (format "delicioapi.el/%s" 
                                         delicious-api-version)
  "The User-Agent field that we will send to the server.")

(defconst delicious-api-host "api.del.icio.us"
  "The delicious host name.")

(defconst delicious-api "/v1/"
  "*The path to the del.ici.ous api.  It should begin and end in a slash.")

(defconst delicious-api-realm (format "%s API" delicious-api-host)
  "The delicious auth realm name.")

;;;;_+ Customization

(defgroup delicious nil
  "Functions for interacting with the del.icio.us API, a web application
for managing and sharing bookmarks."
  :group 'applications
  :version "21.4.1")

(defcustom delicious-posts-file-name "~/.delicious"
  "*Path to the file to save delicious posts to."
  :version "21.4.1"
  :group 'delicious
  :type 'string
  :tag "Delicious posts filename")

(defcustom delicious-cache-file "~/.delicious-cache"
  "*Path to the file to cache posts for later posting to the server."
  :version "21.4.1"
  :group 'delicious
  :type 'string
  :tag "Delicious cache filename")

;;;;_+ API Functions

(put 'delicious-api-error 'error-message "del.icio.us error")
(put 'delicious-api-error 'error-conditions '(delicious-api-error error))

(defun delicious-api-check-error (response)
  "Signal the error if RESPONSE is an API error."
  (unless (string= "done" (xml-get-attribute response 'code))
    (let* ((error-node (car (xml-get-children response 'error)))
           (code (xml-get-attribute error-node 'code))
           (message (rest-api-join (xml-node-children error-node))))
      (signal 'delicious-api-error (list code message)))))

(defun delicious-api-response (buffer)
  "Process the XML response from del.icio.us in BUFFER."
  (unwind-protect
      (with-current-buffer buffer
        (save-excursion
          (goto-char url-http-end-of-headers)
          (let ((response 
                 (car (xml-parse-region 
                       (point) 
                       (- (re-search-forward "<!--") 5)))))
            (delete "\n" (delete "\n  " response)))))))
            
(defun delicious-api-request (path)
  "Do a del.icio.us API request to PATH."
  (let ((url-package-name "delicioapi.el")
        (url-package-version delicious-api-version))
    (delicious-api-response
     (url-retrieve-synchronously
      (format "https://%s%s%s" delicious-api-host delicious-api path)))))

(defun delicious-api/posts/add (url &optional description tags extended time)
  "Post a URL to your del.icio.us account.  
You may include a DESCRIPTION (string), TAGS (space-separated string), 
EXTENDED (extra description string) and TIME (in the format 
%C%y-%m-%dT%H:%M:%SZ)."
  (let* ((description (url-hexify-string description))
         (tags (url-hexify-string tags))
         (extended (url-hexify-string extended))
         (time (url-hexify-string time))
         (post-url (format 
                    "posts/add?&url=%s&description=%s&tags=%s&extended=%s&dt=%s"
                    url description tags extended time)))
    (delicious-api-request post-url)))

(defun delicious-api/tags/get (&optional tags)
  "Return your tags and the number of entries with each tag.
TAGS can be del.icio.us XML. If not provided, contact the server."
  (let* ((path "tags/get")
         (tags-xml 
          (cdr (or tags
                   (delicious-api-request path))))
         (tags-list '()))
    (mapc 
     (lambda (list)
       (when (listp list)
         (let ((this '())
               (tag (xml-get-attribute list 'tag))
               (count (string-to-number (xml-get-attribute list 'count))))
           (setq this (add-to-list 'this (cons 'tag tag))
                 id (add-to-list 'this (cons 'count count)))
           (add-to-list 'tags-list this))))
     tags-xml)
    (nreverse tags-list)))

(defun delicious-api/posts/get (&optional tag date)
  "Return a list of posts filtered by TAG on a given DATE.
If no date is supplied, the most recent date with posts will be used."
  (let ((path (concat "posts/get?"
                      (if tag (format "&tag=%s" tag))
                      (if date (format "&dt=%s" date)))))
    (delicious-api-request path)))

(defun delicious-api/posts/recent (&optional tag count)
  "Return a list, optionally filtered by TAG, of the COUNT most recent posts.
This will max out at 100. Use `delicious-api-get-all' if you want more
than that."
  (let* ((max 
          (cond 
           ((null count) 15)
           ((> count 100) 100)
           (t count)))
         (path
          (concat "posts/recent?"
                  (if tag (format "&tag=%s" tag))
                  (format "&count=%s" max))))
    (delicious-api-request path)))

(defun delicious-api/posts/all (&optional tag)
  "Return all posts. If TAG is non-nil, return all posts with that tag."
  (let ((path 
         (concat "posts/all"
                 (if tag (format "?tag=%s" tag)))))
    (delicious-api-request path)))

(defun delicious-api/posts/dates (&optional tag)
  "Return dates with the number of posts at each date.
TAG is a tag to filter by."
  (let ((path
         (concat "posts/dates"
                 (if tag (format "?tag=%s" tag)))))
    (delicious-api-request path)))

(defun delicious-api/tags/rename (old new)
  "Rename OLD tag to NEW across all posts."
  (let ((path
         (format "tags/rename?&old=%s&new=%s"
                 old new)))
    (delicious-api-request path)))

(defun delicious-api/posts/delete (url)
  "Delete URL from bookmarks."
  (let ((path (format "posts/delete?url=%s" url)))
    (delicious-api-request path)))

(defconst delicious-timestamp
  (concat
   "\\([1-9][0-9]\\{3\\}\\)-"		;year
   "\\([0-1][0-9]\\)-"			;month
   "\\([0-3][0-9]\\)T"			;day
   "\\([0-2][0-9]\\):"			;hour
   "\\([0-5][0-9]\\):"			;minute
   "\\([0-5][0-9]\\)Z")			;second
  "Regular expression matching the timestamp format.")

(defun delicious-api-get-timestamp ()
  "Return the time of the last update from the server.
The value returned is a time value."
  (let* ((path (format "posts/update"))
         (update (delicious-api-request path))
         (date (xml-get-attribute update 'time)))
    date))

(defun delicious-api-version ()
  "Return the version of the Emacs Delicious API in use."
  (interactive)
  (message "%s" delicious-api-version))

(provide 'delicioapi)

;;; delicioapi.el ends here
