;;; delicioapi.el --- functions to interact with the http://del.icio.us API

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
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

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
;; Information about the API is at <http://del.icio.us/doc/api>.
;;
;; Please report any bugs or suggestions to me at
;; <john@wjsullivan.net>. If enough people are interested, perhaps we
;; will open an area at <http://www.emacswiki.org>.

;;; Code:

;; Dependcies

(require 'cl)
(require 'url)
(require 'thingatpt)

;; Variables

(defvar delicious-api-buffer "*delicious output*"
  "*The name of the buffer to direct output to.")

(defvar delicious-api-user-agent "delicious.el/0.1"
  "The User-Agent field that we will send to the server.")

(defvar delicious-api-host "del.icio.us"
  "The delicious host name.")

(defvar delicious-api "/api/"
  "*The path to the del.ici.ous api.  It should begin and end in a slash.")

(defconst delicious-api-version "delicious.el/0.1 2004-12-20"
  "The version string for this copy of delicious-api.el.")

(defconst delicious-api-field-match "=\"\\(.*?\\)\""
  "Regular expression to match a field.")

(defconst delicious-api-success-match "\\(</.*>\\)\\|\\(<result code=\"done\" />\\)"
  "Regular expression to match the various successful result tags.")

(defconst delicious-api-error-match "\\(HTTP/1.0 503 Service Unavailable\\)"
  "Regular expression to match the various error messages.")

;; Customization

(defgroup delicious nil
  "Functions for interacting with the del.icio.us API, a web application for managing bookmarks."
  :group 'applications
  :version "21.3.1")

(defcustom delicious-api-user nil
  "*Your del.icio.us username."
  :version "21.3.1"
  :group 'delicious
  :type 'string
  :tag "del.icio.us user name")

(defcustom delicious-api-password nil
  "*Your del.icio.us password."
  :version "21.3.1"
  :group 'delicious
  :type 'string
  :tag "del.icio.us password")

(defcustom delicious-api-from nil
  "*Sent to the server to identify the request sender.  Use your email address."
  :version "21.3.1"
  :group 'delicious
  :type 'string
  :tag "del.icio.us 'From' header")

(defcustom delicious-api-timeout 60
  "*The number of seconds to wait for output before timing out.  The default is 60."
  :version "21.3.1"
  :group 'delicious
  :type 'integer
  :tag "del.icio.us timeout wait")

;; Functions

;; All "inbox" function have been commented out because they have been temporarily disabled at the
;;  del.icio.us. server.

(defun delicious-api-post (url &optional description tags extended time)
  "Post a URL to your del.icio.us account.  You must include a DESCRIPTION (string).  TAGS (space separated string), EXTENDED (extra description string) and TIME (in the format %C%y-%m-%dT%H:%M:%SZ) are optional additions."
  (let* ((description (url-hexify-string description))
	 (tags (url-hexify-string tags))
	 (extended (url-hexify-string extended))
	 (time (url-hexify-string time))
	 (post-url (format "posts/add?&url=%s&description=%s&tags=%s&extended=%s&dt=%s"
			   url description tags extended time)))
    (delicious-send-request (delicious-build-request post-url))))

(defun delicious-api-get-tags ()
  "Return a hash table of your tags and the number of your entries under each tag.  The keys are the tags."
  (let ((uri "tags/get?")
	(search (delicious-build-search "count" "tag")))
    (delicious-send-request (delicious-build-request uri))
    (delicious-do-search-hash (car search) 2)))

;; (defun delicious-api-get-subscriptions ()
;;   "Return a list of your inbox subscriptions. The list is tag and user."
;;   (let ((uri "inbox/subs?")
;; 	(search (delicious-build-search "tag" "user")))
;;     (delicious-send-request (delicious-build-request uri))
;;     (delicious-do-search-list (car search) (cdr search))))


;; (defun delicious-api-get-inbox-dates ()
;;   "Return a hash table of dates and their entry count in your inbox.
;; The keys are the dates."
;;   (let ((uri "inbox/dates?")
;; 	(search (delicious-build-search "date" "count")))
;;     (delicious-send-request (delicious-build-request uri))
;;     (delicious-do-search-hash (car search) 1)))


;; (defun delicious-api-get-inbox (&optional date)
;;   "Return a list of entries in your inbox.
;;  Filter this list by optional DATE.
;; The list is HREF, DESCRIPTION, TAGS, TIME, and USER."
;;   (let* ((date-filter (url-hexify-string date))
;; 	 (uri (concat "inbox/get?"
;; 	       (unless (null date)
;; 		 (format "&dt=%s" date-filter))))
;; 	 (search (delicious-build-search "href" "description" "tags" "time" "user")))
;;     (delicious-send-request (delicious-build-request uri))
;;     (delicious-do-search-list (car search) (cdr search))))


(defun delicious-api-get-posts (&optional tag date)
   "Return a list of posts filtered by TAG on a given DATE.  If no date is supplied, the most recent date with posts will be used.  The list is HREF, DESCRIPTION, HASH, TAG, and TIME."
  (let* ((uri (concat "posts/get?"
		      (unless (null tag)
			(format "&tag=%s" tag))
		      (unless (null date)
			  (format "&dt=%s" date))))
	 (search (delicious-build-search "href" "description" "hash" "tag" "time")))
    (delicious-send-request (delicious-build-request uri))
    (delicious-do-search-list (car search) (cdr search))))

(defun delicious-api-get-recent (&optional tag count)
  "Return a list, optionally filtered by TAG, of the COUNT most recent posts.  The list is HREF, DESCRIPTION, HASH, TAG, and TIME.  This will max out at 100.  Use `delicious-api-get-all' if you want more than that."
   (let* ((tag (unless (null tag) (url-hexify-string tag)))
	  (count (if (> count 100) 100) count)
	 (uri (concat "posts/recent?"
		      (unless (null tag)
			(format "&tag=%s" tag))
		      (unless (null count)
			(format "&count=%s" count))))
	 (search (delicious-build-search "href" "description" "hash" "tag" "time")))
     (delicious-send-request (delicious-build-request uri))
     (delicious-do-search-list (car search) (cdr search))))

(defun delicious-api-get-all ()
  "Return a list of all posts from your account.  The list is HREF, DESCRIPTION, HASH, TAG, and TIME."
  (let ((uri "posts/all")
	(search (delicious-build-search "href" "description" "hash" "tag" "time")))
    (delicious-send-request (delicious-build-request uri))
    (delicious-do-search-list (car search) (cdr search))))

(defun delicious-api-get-dates (&optional tag)
  "Return a hash table of dates with the number of posts at each date.
TAG is a tag to filter by.  The dates are the keys."
  (let* ((tag (url-hexify-string tag))
	 (uri (concat "posts/dates?"
		      (unless (null tag)
			(format "&tag=%s" tag))))
	 (search (delicious-build-search "count" "date")))
    (delicious-send-request (delicious-build-request uri))
    (delicious-do-search-hash (car search) 2)))

;; (defun delicious-api-unsubscribe (name &optional tag)
;;   "Unsubscribe the inbox feed from NAME under TAG."
;;   (let ((uri (format "inbox/unsub?&user=%s&tag=%s" name tag)))
;;     (delicious-send-request (delicious-build-request uri))))

;; (defun delicious-api-subscribe (name &optional tag)
;;   "Subscribe to an inbox feed from NAME under TAG."
;;   (let ((uri (format "inbox/sub?&user=%s&tag=%s" name tag)))
;;     (delicious-send-request (delicious-build-request uri))))

(defun delicious-api-rename (old-tag new-tag)
  "Rename OLD-TAG to NEW-TAG across all posts."
  (let ((uri (format "tags/rename?&old=%s&new=%s" old-tag new-tag)))
    (delicious-send-request (delicious-build-request uri))))

(defun delicious-api-delete (url)
  "Delete a URL."
  (let ((uri (format "posts/delete?url=%s" url)))
    (delicious-send-request (delicious-build-request uri))))

;;  /html/USERNAME/ [?arg=val&arg=val....] or /html/USERNAME/TAGNAME
;;  [?arg=val&arg=val....]
;; Note that I allow TAGS to be nil or nonnil, but the server has to
;; have a "yes" or a "no" sent to it.

(defun delicious-api-html (&optional tagname count extended divclass
				     aclass tags tagclass tagsep
				     tagsepclass bullet rssbutton
				     extendeddiv extendedclass)
  "Get results formatted in HTML, according to a long list of options.
If TAGNAME is nil, then results from all of the user's tags will be
used.  If TAGNMAE is passed, only posts under that tag will be
considered.

COUNT is the number of items to show.  It defaults to 15.
EXTENDED is either 'title' or 'body'.  It defaults to 'title'.
DIVCLASS is the name of the CSS class to use for the div elements.  It
defaults to 'delPost'.
ACLASS is the CSS class to use for the link elements.  It defaults to
'delLink'.
If TAGS is non-nil, don't show tags.  If it is nil, do.  The server
default is 'yes'.
TAGCLASS is the CSS class to use for tags.  It defaults to 'delTag'.
TAGSEP is the string to use for the separator.  If it is nil, use '/'.
TAGSEPCLASS is the CSS class to use for the separator.  If it is nil,
use 'delTagSep'.
BULLET is the HTML entity to use for the bullets.  Default is nil,
which means no bullet.  'raquo' is a sample alternative value, which is
also the default for the server.
If RSSBUTTON is nil, add an RSS feed button using CSS.  It it is
non-nil, don't add an RSS feed button.
EXTENDEDDIV is an extended entry in its own div.  If it is nil, don't
use it.  If it is non-nil, do something.
EXTENDEDCLASS is a CSS class to use for EXTENDEDDIV.")

(defun delicious-api-build-tag-completion ()
  "Return a numbered list of current tags, to use in tab completion."
  (let ((tags-hash (delicious-api-get-tags))
	(tags-list '())
	(counter 1))
    (maphash '(lambda (key value)
		(setq tags-list (cons (list key counter) tags-list))
		(setq counter (1+ counter)))
	     tags-hash)
    (reverse tags-list)))

(defun delicious-build-request (uri)
  "Return the proper HTTP request to get URI."
  (let* ((uri (format "http://%s%s%s" delicious-api-host delicious-api uri)))
    (format "GET %s HTTP/1.0\nFrom: %s\nUser-Agent: %s\nAuthorization: Basic %s\n\n"
	    uri delicious-api-from delicious-api-user-agent (delicious-auth))))

(defun delicious-send-request (request)
  "Send the REQUEST to the server.  Wait for success, HTTP error, or timeout.
Output goes to `delicious-api-buffer'."
  (let ((error-check
	 (catch 'error
	   (unless (null (get-buffer delicious-api-buffer))
	     (kill-buffer delicious-api-buffer))
	   (open-network-stream "delicious" delicious-api-buffer delicious-api-host "http")
	   (process-send-string "delicious" request)
	   (with-current-buffer delicious-api-buffer
	     (let ((proc (get-process "delicious")))
	       (setq time-out (run-with-timer delicious-api-timeout nil '(lambda () (throw 'error "timeout"))))
	       (while (save-excursion
			(and (memq (process-status proc) '(open run))
			     (not (re-search-forward delicious-api-success-match nil t))))
		 (save-excursion
		   (if (re-search-forward delicious-api-error-match nil t)
		       (throw 'error (match-string 1))))
		 (accept-process-output proc))))
	   (cancel-timer time-out))))
    (cond ((equal error-check "timeout")
	   (error "Timed out waiting for response.  Your transaction may still occur, though")
	   (kill-process (get-process "delicious")))
	  (error-check
	   (error error-check)))))
	   
(defun delicious-build-search (&rest fields)
  "Given list FIELDS, return a list with CAR as the search-string and CDR the number of search fields."
  (let ((search-string nil)
	(count-fields (length fields)))
    (loop for field in fields do
 	  (setq search-string
		(concat search-string
			field delicious-api-field-match ".*")))
    (cons search-string count-fields)))

(defun delicious-do-search-list (search-string fields)
  "Return a list of occurrences of SEARCH-STRING.  SEARCH-STRING has FIELDS separate fields.  Output to `delicious-api-buffer'."
  (save-excursion
    (with-current-buffer delicious-api-buffer
      (goto-char (point-min))
      (let ((results-list))
	(while (re-search-forward search-string nil t)
	  (setq results-list (cons (let ((result))
                                     (loop for field from 1 to fields do
                                           (setq result (append result (list (match-string field)))))
                                     result)
                                   results-list)))
	results-list))))

(defun delicious-do-search-hash (search-string key)
  "Return a hash table of occurrences in `delicious-api-buffer' of SEARCH-STRING.  KEY is either 1 or 2. If it is 1, then the hash key will be the match for the first field in the search string.  If it is 2, then the key will be the match for the second field."
  (save-excursion
    (with-current-buffer delicious-api-buffer
      (goto-char (point-min))
      (let ((results-hash (make-hash-table :test 'equal)))
	(while (re-search-forward search-string nil t)
	  (if (equal key 1)
	      (puthash (match-string 1) (match-string 2) results-hash)
	    (puthash (match-string 2) (match-string 1) results-hash)))
	results-hash))))
	
(defun delicious-auth ()
  "Return the authorization string using `delicious-api-user' and `delicious-api-password'."
  (base64-encode-string
   (format "%s:%s" delicious-api-user delicious-api-password)))

(defun delicious-api-version ()
  "Return the version of the Emacs Delicious API in use."
  (interactive)
  (message "%s" delicious-api-version))

(provide 'delicioapi)

;;; delicioapi.el ends here
