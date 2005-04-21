;;; planner-delicious.el --- bringing del.icio.us bookmarks to your Planner

;; Copyright (C) 2005 John Sullivan

;; Author: John Sullivan <john@wjsullivan.net>
;; Created 31 March 2005
;; Version: 0.1 2005-04-21
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
;; can always be fonud at <http://www.wjsullivan.net>.

;;; Code:

(require 'delicious)

(defun planner-rewrite-delicious-posts (&optional tag date)
  "Insert all posts from your account filtered by TAG, from DATE. 
Into the '* Delicious' section of your planner page. If no DATE
is specified, the most recent date with posts will be used.
DATE format should be %C%y-%m-%dT%H:%M:%SZ. The T and Z are literal."
  (save-excursion 
    (goto-char (point-min))
    (re-search-forward "* Delicious")
    (forward-line 2)
    (save-excursion
      (let ((beg (line-beginning-position))
            (end (if (re-search-forward "^* " (point-max) t)
                     (line-beginning-position)
                   (point-max))))
        (delete-region beg end)))
    (let ((matching-posts (delicious-get-posts-from-stored tag date)))
      (ignore 
       (mapc (lambda (post)
               (let* ((href (cdr (assoc "href" post)))
                      (description (cdr (assoc "description" post)))
                      (extended (cdr (assoc "extended" post))))
                 (insert (format "[[%s]" href))
                 (if (null description)
                     (insert "]\n")
                   (insert (format "[%s]]\n" description)))
                 (unless (null extended)
                   (insert (format "%s\n" extended)))
                 (insert "\n")))
             matching-posts)))))

(provide 'planner-delicious)
