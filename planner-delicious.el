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
