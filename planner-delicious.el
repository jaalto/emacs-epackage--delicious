;;; planner-delicious.el --- bringing del.icio.us bookmarks to your Planner

;; Copyright (C) 2005 John Sullivan

;; Author: John Sullivan <john@wjsullivan.net>
;; Created 31 March 2005
;; Version: 0.1 2005-04-26
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

;; You can load this file by putting (require 'planner-delicious) in your
;; `.emacs' file. This program requires Planner, which is available at Sacha
;; Chua's Web site <http://sacha.free.net.ph>.  Please report any bugs or
;; suggestions to me at <john@wjsullivan.net>. The newest version of this file
;; can always be found at <http://www.wjsullivan.net>.
;;
;; To use this on a Planner page, you need to have:
;;
;;   * Delicious
;;
;;   <lisp>(planner-delicious-insert-posts "tag-regexp" "date-regexp")</lisp>
;;
;; on the pages where you want your posts to appear. This function rewrites
;; everything between `planner-delicious-section' ("* Delicious" by default)
;; and the beginning of the next section on the page, but should leave
;; everything else alone. You could add this to your
;; `planner-day-page-template' if you want posts to appear every day.
;;
;; If you don't want to filter by either date or time, just use a regexp that
;; matches anything, like ".*" for either or both arguments. When constructing
;; your regexps, keep in mind that tags are stored space-separated, and dates
;; are stored in a format like `2005-04-23T20:22:55Z'. 

;; The code to rewrite `planner-delicious-section' is based on work by Sacha
;; Chua in `planner-accomplishments.el'.

;;; Code:

(require 'delicious)

(defcustom planner-delicious-section "Delicious"
  "*Header for the del.icio.us section in a plan page."
  :version "21.4.1"
  :type 'string
  :group 'delicious)

(defun planner-delicious-insert-posts (tag date)
  "Insert all your posts matching TAG and DATE under `planner-delicious-section'.
TAG should be a regexp constructed to match the stored space-separated list
of tags. DATE should be a regexp constructed with the stored date format
in mind. An example of the stored date format is `2005-04-23T20:22:55Z'." 
  (save-excursion
    (save-restriction
      (when (planner-narrow-to-section planner-delicious-section)
        (delete-region (point-min) (point-max))
        (insert "* " planner-delicious-section "\n\n"
                (let ((matching-posts 
                       (delicious-get-posts-from-stored tag date)))
                  (with-temp-buffer
                    (mapc (lambda (post)
                            (let ((href (cdr (assoc "href" post)))
                                  (description (cdr (assoc "description" post)))
                                  (extended (cdr (assoc "extended" post))))
                              (insert (planner-make-link href description) "\n")
                              (unless (null extended)
                                (insert (format "%s\n" extended)))
                              (insert "\n")))
                          matching-posts)
                    (buffer-string))) "\n\n")))))

(provide 'planner-delicious)


