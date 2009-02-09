;; From: Gunnar Wrobel <post@gunnarwrobel.de>

(defun muse-publish-delicious-tags (beg end)
  (let* ((tags (buffer-substring-no-properties beg end))
         (matches (delicious-posts-matching-tags tags))
         (matches (delicious-sort-date-descending matches)))
    (delete-region beg end)
    (mapc
     (lambda (post)
       (let* ((href (assoc-default "href" post))
              (desc (assoc-default "description" post))
              (link (muse-make-link href desc))
              (ext (assoc-default "extended" post))
              (txt (format "%s ::\n  %s\n\n" link (or ext desc))))
         (insert txt)))
     matches)
    (insert "\n")))

(defun muse-publish-delicious-end-tags (beg end)
  (muse-publish-escape-specials beg end)
  (let ((tags (buffer-substring-no-properties beg end)))
    (delete-region beg end)))

(add-to-list 'muse-publish-markup-tags
             '("delistart" t nil muse-publish-delicious-tags))

(add-to-list 'muse-publish-markup-tags
             '("deliend" t nil muse-publish-delicious-end-tags))

;; Write functions using SORT to compare the date fields.

(defun delicious-sort-date-descending (posts)
  posts)

(defun delicious-sort-date-ascending (posts)
  posts)

(provide 'muse-delicious)
