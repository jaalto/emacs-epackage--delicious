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
;;   <lisp>(planner-rewrite-delicious-posts "whatever-tag")</lisp>
;;
;; on the pages where you want your posts to appear. This function rewrites
;; everything between `* Delicious' and the end of the next closing </lisp>
;; tag, but should leave everything else alone. You could add this to your
;; `planner-day-page-template' if you want posts to appear there every day.

;;; Code:

(require 'delicious)

(defun planner-rewrite-delicious-posts (tag)
  "Insert all your posts filtered by TAG into the '* Delicious' section."
  (save-excursion
    (goto-char (point-min))
    (or (re-search-forward "^\* Delicious" (point-max) t)
        (error "No * Delicious section in this buffer"))
    (let ((beg (point))
          (end (re-search-forward "</lisp>" (point-max) t)))
      (delete-region beg end))
    (insert 
     "\n\n"
     (let ((matching-posts (delicious-get-posts-from-stored tag)))
       (with-temp-buffer
         (mapc (lambda (post)
                 (let ((href (cdr (assoc "href" post)))
                       (description (cdr (assoc "description" post)))
                       (extended (cdr (assoc "extended" post))))
                   (insert (format "[[%s]" href))
                   (if (null description)
                       (insert "]\n")
                     (insert (format "[%s]]\n" description)))
                   (unless (null extended)
                     (insert (format "%s\n" extended)))
                   (insert "\n")))
               matching-posts)
         (buffer-string))))))

(provide 'planner-delicious)
