;;; delicioapi.el --- functions to interact with the http://del.icio.us API

;; Copyright (C) 2004, 2005 John Sullivan

;; Author: John Sullivan <john@wjsullivan.net>
;; Created 25 October 2004
;; Version: 0.2 2005-06-05
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

(defvar delicious-api-user-agent "delicioapi.el/0.2"
  "The User-Agent field that we will send to the server.")

(defvar delicious-api-host "del.icio.us"
  "The delicious host name.")

(defvar delicious-api "/api/"
  "*The path to the del.ici.ous api.  It should begin and end in a slash.")

(defvar delicious-api-html "/html/"
  "*The path to the del.icio.us HTML feed.  It should begin and end with a slash.")

(defconst delicious-api-version "delicioapi.el/0.2 2005-05-12"
"The version string for this copy of delicioapi.el.")

(defconst delicious-api-field-match "=\"\\(.*?\\)\""
"Regular expression to match a field.")

(defconst delicious-api-success-match "\\(</.*>\\)\\|\\(<result code=\"done\" />\\)"
"Regular expression to match the various successful result tags.")

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

(defcustom delicious-api-html-count 15
"*The number of times to show by default when fetching an HTML del.icio.us feed.  The server default is 15."
:version "21.3.1"
:group 'delicious
:type 'integer
:tag "HTML item count parameter")

(defcustom delicious-api-html-extended "title"
"*Either 'title' or 'body'.  The server default is `title'.  This means that the extended description will just be shown in the title attribute of the link tag.  If you want it displayed on its own, use `body'."
:version "21.3.1"
:group 'delicious
:type 'string
:tag "HTML extended parameter")

(defcustom delicious-api-html-divclass "delPost"
"*Class to use for div.  The server default is `delPost'."
:version "21.3.1"
:group 'delicious
:type 'string
:tag "HTML divclass parameter")

(defcustom delicious-api-html-aclass "delLink"
"*Class to use for a link.  The server default is `delLink'."
:version "21.3.1"
:group 'delicious
:type 'string
:tag "HTML aclass parameter")

(defcustom delicious-api-html-tags nil
"*Show tags or not.  Server default is to show tags.  Set this to true if you do NOT want tags."
:version "21.3.1"
:group 'delicious
:type 'boolean
:tag "HTML tags parameter")

(defcustom delicious-api-html-tagclass "delTag"
"*Class to use for tags.  Server default is `delTag'."
:version "21.3.1"
:group 'delicious
:type 'string
:tag "HTML tagclass parameter")

(defcustom delicious-api-html-tagsep "/"
"*String to use for separator.  Server default is `/'."
:version "21.3.1"
:group 'delicious
:type 'string
:tag "HTML tagsep parameter")

(defcustom delicious-api-html-tagsepclass "delTagSep"
"*Class to use for separator.  Server default is `delTagSep'."
:version "21.3.1"
:group 'delicious
:type 'string
:tag "HTML tagsepclass parameter")

(defcustom delicious-api-html-bullet "raquo"
"*HTML entity to use for bullet.  Set it to empty for no bullet.  Server default is `raquo'."
:version "21.3.1"
:group 'delicious
:type 'string
:tag "HTML bullet parameter")

(defcustom delicious-api-html-rssbutton "yes"
"*Add an RSS feed button using CSS.  Server default is to show a button."
:version "21.3.1"
:group 'delicious
:type 'string
:tag "HTML rssbutton parameter")

(defcustom delicious-api-html-extendeddiv nil
"*Extended entry in its own div.  Server default is `no'."
:version "21.3.1"
:group 'delicious
:type 'boolean
:tag "HTML extendeddiv parameter")

(defcustom delicious-api-html-extendedclass nil
"*Class to use for extendeddiv.  Server default is empty."
:version "21.3.1"
:group 'delicious
:type 'string
:tag "HTML extendedclass parameter")

(defcustom delicious-posts-file-name ".delicious"
"*Filename in $HOME to save delicious posts to."
:version "21.4.1"
:group 'delicious
:type 'string
:tag "Delicious posts filename")

;; Functions

;; All "inbox" function have been commented out because they have been
;; temporarily disabled at the del.icio.us. server.

(defun delicious-api-post (url &optional description tags extended time)
"Post a URL to your del.icio.us account.  
You must include a DESCRIPTION (string).  TAGS (space separated string), 
EXTENDED (extra description string) and TIME (in the format %C%y-%m-%dT%H:%M:%SZ) 
are optional additions."
(let* ((description (url-hexify-string description))
       (tags (url-hexify-string tags))
       (extended (url-hexify-string extended))
       (time (url-hexify-string time))
       (post-url (format 
                  "posts/add?&url=%s&description=%s&tags=%s&extended=%s&dt=%s"
                         url description tags extended time)))
  (delicious-send-request (delicious-build-request post-url))))

(defun delicious-api-get-tags ()
  "Return a hash table of your tags and the number of your entries under each tag.
The keys are the tags."
(let ((uri "tags/get?")
      (search (delicious-build-search "count" "tag")))
  (delicious-send-request (delicious-build-request uri))
  (delicious-do-search-hash (car search) 2)))

;; (defun delicious-api-get-subscriptions ()
;;   "Return a list of your inbox subscriptions. The list is tag and user."
;;   (let ((uri "inbox/subs?")
;;      (search (delicious-build-search "tag" "user")))
;;     (delicious-send-request (delicious-build-request uri))
;;     (delicious-do-search-list (car search) (cdr search))))


;; (defun delicious-api-get-inbox-dates ()
;;   "Return a hash table of dates and their entry count in your inbox.
;; The keys are the dates."
;;   (let ((uri "inbox/dates?")
;;      (search (delicious-build-search "date" "count")))
;;     (delicious-send-request (delicious-build-request uri))
;;     (delicious-do-search-hash (car search) 1)))


;; (defun delicious-api-get-inbox (&optional date)
;;   "Return a list of entries in your inbox.
;;  Filter this list by optional DATE.
;; The list is HREF, DESCRIPTION, TAGS, TIME, and USER."
;;   (let* ((date-filter (url-hexify-string date))
;;       (uri (concat "inbox/get?"
;;             (unless (null date)
;;               (format "&dt=%s" date-filter))))
;;       (search (delicious-build-search "href" "description" "tags" "time" "user")))
;;     (delicious-send-request (delicious-build-request uri))
;;     (delicious-do-search-list (car search) (cdr search))))


(defun delicious-api-get-posts (&optional tag date)
"Return a list of posts filtered by TAG on a given DATE.  If no date is supplied, the most recent date with posts will be used.  The list is HREF, DESCRIPTION, EXTENDED, HASH, TAG, and TIME."
(let* ((uri (concat "posts/get?"
                    (unless (null tag)
                      (format "&tag=%s" tag))
                    (unless (null date)
                      (format "&dt=%s" date)))))
  (delicious-send-request (delicious-build-request uri))
  (delicious-api-parse-posts)))

(defun delicious-api-get-recent (&optional tag count)
"Return a list, optionally filtered by TAG, of the COUNT most recent posts.  The list is HREF, DESCRIPTION, EXTENDED, HASH, TAG, and TIME.  This will max out at 100.  Use `delicious-api-get-all' if you want more than that."
(let* ((count-fixed (cond
                     ((null count)
                      15)
                     ((> count 100)
                      100)
                     (t count)))
       (uri (concat "posts/recent?"
                    (unless (null tag)
                      (format "&tag=%s" (url-hexify-string tag)))
                    (format "&count=%s" count-fixed))))
  (delicious-send-request (delicious-build-request uri))
  (delicious-api-parse-posts)))

(defun delicious-api-get-all ()
"Return a list of all posts from your account.  The list is HREF, DESCRIPTION, EXTENDED, HASH, TAG, and TIME."
(let ((uri "posts/all"))
  (delicious-send-request (delicious-build-request uri))
  (delicious-api-parse-posts)))

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
  (let ((uri (format "tags/rename?&old=%s&new=%s" 
		     (url-hexify-string old-tag)
		     (url-hexify-string new-tag))))
    (delicious-send-request (delicious-build-request uri))))

(defun delicious-api-delete (url)
  "Delete a URL."
  (let ((uri (format "posts/delete?url=%s" url)))
    (delicious-send-request (delicious-build-request uri))))

(defun delicious-api-get-timestamp ()
  "Return the time of the last update from the server.
The value returned is a time value."
  (let ((uri (format "posts/update"))
	(search-string (delicious-build-search "update time")))
    (delicious-send-request (delicious-build-request uri))
    (setq date (caar (delicious-do-search-list (car search-string) 1)))
    (string-match "\\([0-9]++++\\)-\\([0-9]++\\)-\\([0-9]++\\)T\\([0-9]++\\):\\([0-9]++\\):\\([0-9]++\\)" date)
    (apply 'encode-time 
	   (mapcar (lambda (i)
		     (string-to-int (match-string i date)))
		   '(6 5 4 3 2 1)))))

(defun delicious-api-html
  (&optional username tagname count extended divclass aclass tags tagclass tagsep tagsepclass bullet rssbutton extendeddiv extendedclass)
  "Return results formatted in HTML, according to a long list of options.
USERNAME is the name of the user whose links you want to fetch.  If you don't
specify a name, `delicious-api-user' will be used.  If TAGNAME is nil, then
results from all of the user's tags will be used.  If TAGNAME is passed, only
posts under that tag will be considered. COUNT is the number of items to show.
It defaults to 15 at the server.  EXTENDED is either 'title' or 'body'.  It
defaults to 'title'.  DIVCLASS is the name of the CSS class to use for the div
elements.  It defaults to 'delPost'.  ACLASS is the CSS class to use for the
link elements.  It defaulnts to 'delLink'.  If TAGS is non-nil, don't show tags.
If it is nil, do.  The server default is 'yes'.  TAGCLASS is the CSS class to
use for tags.  It defaults to 'delTag'.  TAGSEP is the string to use for the
separator.  If it is nil, use '/'.  TAGSEPCLASS is the CSS class to use for the
separator.  If it is nil, use 'delTagSep'.  BULLET is the HTML entity to use
for the bullets.  Default is nil, which means no bullet.  'raquo' is a sample
alternative value, which is also the default for the server.  If RSSBUTTON is
nil, add an RSS feed button using CSS.  It it is non-nil, don't add an RSS feed
button.  EXTENDEDDIV is an extended entry in its own div.  If it is nil, don't
use it.  If it is non-nil, do something.  EXTENDEDCLASS is a CSS class to use
for EXTENDEDDIV."
  (delicious-send-request
   (delicious-api-build-html-request
    (delicious-api-html-uri username tagname count extended divclass aclass tags tagclass tagsep tagsepclass bullet rssbutton extendeddiv extendedclass)))
  (save-excursion
    (with-current-buffer delicious-api-buffer
      (let ((beginning (progn
			 (goto-char (point-min))
			 (re-search-forward "<div")
			 (line-beginning-position)))
	    (end (progn
		   (goto-char (point-max))
		   (re-search-backward "</a>")
		   (line-end-position))))
	(buffer-substring beginning end)))))

(defun delicious-api-html-uri
  (&optional username tagname count extended divclass aclass tags tagclass tagsep tagsepclass bullet rssbutton extendeddiv extendedclass)
  "Assemble the uri for getting the list of recent posts, formatted in HTML.
The long list of options comes from the del.icio.us API.
USERNAME is the name of the user whose links you want to fetch.  If you don't
specify a name, `delicious-api-user' will be used.  If TAGNAME is nil, then
results from all of the user's tags will be used.  If TAGNAME is passed, only
posts under that tag will be considered.  COUNT is the number of items to show.
It defaults to 15 at the server.  EXTENDED is either 'title' or 'body'.  It
defaults to 'title'.  DIVCLASS is the name of the CSS class to use for the div
elements.  It defaults to 'delPost'.  ACLASS is the CSS class to use for the
link elements.  It defaulnts to 'delLink'.  If TAGS is non-nil, don't show tags.
If it is nil, do.  The server default is 'yes'.  TAGCLASS is the CSS class to
use for tags.  It defaults to 'delTag'.  TAGSEP is the string to use for the
separator.  If it is nil, use '/'.  TAGSEPCLASS is the CSS class to use for the
separator.  If it is nil, use 'delTagSep'.  BULLET is the HTML entity to use
for the bullets.  Default is nil, which means no bullet.  'raquo' is a sample
alternative value, which is also the default for the server.  If RSSBUTTON is
nil, add an RSS feed button using CSS.  It it is non-nil, don't add an RSS feed
button.  EXTENDEDDIV is an extended entry in its own div.  If it is nil, don't
use it.  If it is non-nil, do something.  EXTENDEDCLASS is a CSS class to use
for EXTENDEDDIV."
  (let* ((user (if (not (or (null username) (equal username "")))
                   username
                 delicious-api-user))
         (uri (substring (concat (format "%s/?" user)
                                 (if (not (or (null tagname) (equal tagname "")))
                                     (format "%s?" tagname))
                                 (if (not (or (null count) (equal count "")))
                                     (format "count=%s&" count)
                                   (unless (null delicious-api-html-count)
                                     (format "count=%s&" delicious-api-html-count)))
                                 (if (not (null extended))
                                     (format "extended=%s&" extended)
                                   (unless (null delicious-api-html-extended)
                                     (format "extended=%s&" delicious-api-html-extended)))
                                 (if (not (null divclass))
                                     (format "divclass=%s&" divclass)
                                   (unless (null delicious-api-html-divclass)
                                     (format "divclass=%s&" delicious-api-html-divclass)))
                                 (if (not (null aclass))
                                     (format "aclass=%s&" aclass)
                                   (unless (null delicious-api-html-aclass)
                                     (format "aclass=%s&" delicious-api-html-aclass)))
                                 (if (not (null tags))
                                     (format "tags=%s&" tags)
                                   (unless (null delicious-api-html-tags)
                                     (format "tags=%s&" delicious-api-html-tags)))
                                 (if (not (null tagclass))
                                     (format "tagclass=%s&" tagclass)
                                   (unless (null delicious-api-html-tagclass)
                                     (format "tagclass=%s&" delicious-api-html-tagclass)))
                                 (if (not (null tagsep))
                                     (format "tagsep=%s&" tagsep)
                                   (unless (null delicious-api-html-tagsep)
                                     (format "tagsep=%s&" delicious-api-html-tagsep)))
                                 (if (not (null tagsepclass))
                                     (format "tagsepclass=%s&" tagsepclass)
                                   (unless (null delicious-api-html-tagsepclass)
                                     (format "tagsepclass=%s&" delicious-api-html-tagsepclass)))
                                 (if (not (null bullet))
                                     (format "bullet=%s&" bullet)
                                   (unless (null delicious-api-html-bullet)
                                     (format "bullet=%s&" delicious-api-html-bullet)))
                                 (if (not (null rssbutton))
                                     (format "rssbutton=%s&" rssbutton)
                                   (unless (null delicious-api-html-rssbutton)
                                     (format "rssbutton=%s&" delicious-api-html-rssbutton)))
                                 (if (not (null extendeddiv))
                                     (format "extendeddiv=%s&" extendeddiv)
                                   (unless (null delicious-api-html-extendeddiv)
                                     (format "extendeddiv=%s&" delicious-api-html-extendeddiv)))
                                 (if (not (null extendedclass))
                                     (format "extendedclass=%s&" extendedclass)
                                   (unless (null delicious-api-html-extendedclass)
                                     (format "extendedclass=%s&" delicious-api-html-extendedclass))))
                         0 -1)))
    uri))
  
(defun delicious-api-build-html-request (uri)
  "Return the proper HTTP request to get URI from the HTML feed."
  (let* ((uri (format "http://%s%s%s" delicious-api-host delicious-api-html uri)))
    (format "GET %s HTTP/1.0\nFrom: %s\nUser-Agent: %s\nAuthorization: Basic %s\n\n"
            uri delicious-api-from delicious-api-user-agent (delicious-auth))))

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
  "Send the REQUEST to the server.
Wait for up to `delicious-api-timeout' seconds for output. Output goes to
`delicious-api-buffer'. After receiving output, check for HTTP errors."
  (let ((error-check
         (catch 'error
           (unless (null (get-buffer delicious-api-buffer))
             (kill-buffer delicious-api-buffer))
           (open-network-stream "delicious" delicious-api-buffer 
                                delicious-api-host "http")
           (process-send-string "delicious" request)
           (with-current-buffer delicious-api-buffer
             (let ((proc (get-process "delicious")))
               (setq time-out (run-with-timer delicious-api-timeout nil
                                              '(lambda () (throw 'error 
                                                                 "timeout"))))
               (while (memq (process-status proc) '(open run))
                 (accept-process-output proc)
                 (save-excursion 
                   (goto-char (point-min))
                   (cond ((re-search-forward "HTTP/1.1 503" nil t)
                          (throw 
                          'error "HTTP 503 error received, server unavailable"))
                         ((not (re-search-forward "HTTP/1.1 200 OK" nil t))
                          (throw 
                           'error 
                           "HTTP error received, see delicious output buffer"))
                         ((not (re-search-forward 
                                delicious-api-success-match nil t))
                          (throw
                           'error "Unrecognized output received")))))))
           (cancel-timer time-out))))
    (cond ((equal error-check "timeout")
           (error 
            "Timed out waiting for response (transaction may still occur)")
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

(defun delicious-api-parse-posts ()
  "Parse the *delicious output* XML for posts into fields."
  (save-excursion
    (with-current-buffer delicious-api-buffer
      (goto-char (point-min))
      (let* ((posts
              (loop while (re-search-forward "<post " nil t)
                    for post = (buffer-substring (point)
                                                 (- (re-search-forward "/>" nil t) 3))
                    collect post))
             (posts-parsed
              (loop for post in posts
                    with fields = '("href" "description" "extended" "hash" "tag" "time")
                    collect (loop for field in fields
                                  if (string-match
                                      (concat field "=\"\\(.*?\\)\"") post)
                                  collect (cons field (match-string 1 post))))))
        posts-parsed))))

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
