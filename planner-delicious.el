;;; planner-delicious.el --- bringing del.icio.us bookmarks to your Planner

;; Copyright (C) 2005 John Sullivan

;; Author: John Sullivan <john@wjsullivan.net>
;; Created 31 March 2005
;; Version: 0.1 2005-05-18
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
;; on the pages where you want your posts to appear. These functions append
;; links to the end of `planner-delicious-section', which is ("* Delicious" by
;; default). You could add this to your `planner-day-page-template' if you want
;; posts to appear every day.
;;
;; When constructing your date regexp, keep in
;; mind that dates are stored in a format like `2005-04-23T20:22:55Z'.
;;
;; The two interactive commands to use are `planner-delicious-insert-posts-any'
;; and `planner-delicious-insert-posts-all'. Read their docstrings for details.

;;; Code:

(require 'delicious)

(defcustom planner-delicious-section "Delicious"
  "*Header for the del.icio.us section in a plan page."
  :version "21.4.1"
  :type 'string
  :group 'delicious)

(defun planner-delicious-append-posts (posts)
  "Append POSTS to `planner-delicious-section'."
  (if (null (planner-narrow-to-section planner-delicious-section))
      (error "No delicious section on this page")
    (goto-char (point-max))
    (mapc
     (lambda (post)
       (let* ((href (cdr (assoc "href" post)))
              (desc (cdr (assoc "description" post)))
              (link (planner-make-link href desc)))
         (insert "\n" link "\n\n")))
     posts)))

(defun planner-delicious-insert-posts-all (tag &optional search-date)
  "Insert all your posts matching all of the tags and the date entered.
If a prefix is given, do not filter by date."
  (interactive (list (delicious-complete-tags nil nil nil nil t)
                     (unless current-prefix-arg
                       (planner-delicious-read-date))))
  (save-excursion
    (save-restriction
      (let ((matches (delicious-posts-matching-tags tag)))
        (when search-date
          (setq matches (delicious-posts-matching-date matches search-date)))
        (planner-delicious-append-posts matches)))))

(defun planner-delicious-insert-posts-any (tag &optional search-date)
  "Insert all your posts matching any of TAGS with date matching SEARCH-DATE.
If a prefix is given, do not filter by date."
  (interactive (list (delicious-complete-tags nil nil nil nil t)
                     (unless current-prefix-arg
                       (planner-delicious-read-date))))
  (save-excursion
    (save-restriction
      (let ((matches (delicious-posts-matching-tags-any tag)))
        (when search-date
          (setq matches (delicious-posts-matching-date matches search-date)))
        (planner-delicious-append-posts matches)))))

(defun planner-delicious-read-date ()
  "Input a date."
  (let ((date (read-string "Date (a regexp pattern): ")))
    date))

(provide 'planner-delicious)


