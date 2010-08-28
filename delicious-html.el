;;; delicious-html.el --- interface to the Delicious HTML feed API

;; Copyright (C) 2009 John Sullivan

;; Author: John Sullivan <john@wjsullivan.net>
;;         Štěpán Němec <stepnem@gmail.com>
;; Maintainer: Štěpán Němec <stepnem@gmail.com>
;; Time-stamp: "2010-08-28 11:37:19 CEST stepnem"
;; Created: Mon Feb 9 13:08:24 2009 -0500
;; Version: FIXME
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

;;; Code:
(defconst delicious-feeds-host "feeds.delicious.com"
  "Delicious feeds host name.")

(defconst delicious-api-html "/html/"
  "Path to the Delicious HTML feed.
It should begin and end with a slash.")

(defcustom delicious-api-html-count 15
  "Number of bookmarks to show by default when fetching an HTML feed.
The server default is 15."
  :group 'delicious
  :type 'integer
  :tag "HTML item count parameter")

(defcustom delicious-api-html-extended "title"
  "Either 'title' or 'body'.  The server default is `title'.
This means that the extended description will just be shown in the title
attribute of the link tag.  If you want it displayed on its own, use `body'."
  :group 'delicious
  :type 'string
  :tag "HTML extended parameter")

(defcustom delicious-api-html-divclass "delPost"
  "Class to use for div.  The server default is `delPost'."
  :group 'delicious
  :type 'string
  :tag "HTML divclass parameter")

(defcustom delicious-api-html-aclass "delLink"
  "Class to use for a link.  The server default is `delLink'."
  :group 'delicious
  :type 'string
  :tag "HTML aclass parameter")

(defcustom delicious-api-html-tags nil
  "Show tags or not.  Server default is to show tags.
Set this to true if you do NOT want tags."
  :group 'delicious
  :type 'boolean
  :tag "HTML tags parameter")

(defcustom delicious-api-html-tagclass "delTag"
  "Class to use for tags.  Server default is `delTag'."
  :group 'delicious
  :type 'string
  :tag "HTML tagclass parameter")

(defcustom delicious-api-html-tagsep "/"
  "String to use for separator.  Server default is `/'."
  :group 'delicious
  :type 'string
  :tag "HTML tagsep parameter")

(defcustom delicious-api-html-tagsepclass "delTagSep"
  "Class to use for separator.  Server default is `delTagSep'."
  :group 'delicious
  :type 'string
  :tag "HTML tagsepclass parameter")

(defcustom delicious-api-html-bullet "raquo"
  "HTML entity to use for bullet.  Set it to empty for no bullet.
Server default is `raquo'."
  :group 'delicious
  :type 'string
  :tag "HTML bullet parameter")

(defcustom delicious-api-html-rssbutton "yes"
  "Add an RSS feed button using CSS.  Server default is to show a button."
  :group 'delicious
  :type 'string
  :tag "HTML rssbutton parameter")

(defcustom delicious-api-html-extendeddiv nil
  "Extended entry in its own div.  Server default is `no'."
  :group 'delicious
  :type 'boolean
  :tag "HTML extendeddiv parameter")

(defcustom delicious-api-html-extendedclass nil
  "Class to use for extendeddiv.  Server default is empty."
  :group 'delicious
  :type 'string
  :tag "HTML extendedclass parameter")


(defvar delicious-username)
(defun delicious-api-html-uri
  (&optional username tagname count extended divclass aclass tags tagclass
             tagsep tagsepclass bullet rssbutton extendeddiv extendedclass)
  "Assemble the URI for getting the list of recent posts, formatted in HTML.
The long list of options comes from the Delicious API.

USERNAME is the name of the user whose links you want to fetch.
If you don't specify a name, `delicious-api-user' will be used.

If TAGNAME is nil, then results from all of the user's tags will be used.
If TAGNAME is passed, only posts under that tag will be considered.

COUNT is the number of items to show. It defaults to 15 at the server.

EXTENDED is either 'title' or 'body'. It defaults to 'title'.

DIVCLASS is the name of the CSS class to use for the div elements.
It defaults to 'delPost'.

ACLASS is the CSS class to use for the link elements.
It defaults to 'delLink'.

If TAGS is non-nil, don't show tags. If it is nil, do.
The server default is 'yes'.

TAGCLASS is the CSS class to use for tags. It defaults to 'delTag'.

TAGSEP is the string to use for the separator. If it is nil, use '/'.

TAGSEPCLASS is the CSS class to use for the separator. If it is nil,
use 'delTagSep'.

BULLET is the HTML entity to use for the bullets. Default is nil,
which means no bullet. 'raquo' is a sample alternative value,
which is also the default for the server.

If RSSBUTTON is nil, add an RSS feed button using CSS.
If it is non-nil, don't add an RSS feed button.

EXTENDEDDIV is an extended entry in its own div. If it is nil, don't use it.
If it is non-nil, do something.

EXTENDEDCLASS is a CSS class to use for EXTENDEDDIV."
  (let* ((user (if (not (or (null username) (equal username "")))
                   username
                 delicious-username))
         (uri
          (substring
           (concat
            (format "%s/?" user)
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

(provide 'delicious-html)
;;; delicious-html.el ends here
