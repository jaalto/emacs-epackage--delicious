
;;;### (autoloads (delicious-search-hash delicious-search-date delicious-search-tags-any
;;;;;;  delicious-search-tags delicious-search-href-regexp delicious-search-description-regexp
;;;;;;  delicious-search-posts-regexp delicious-w3m-export delicious-w3m-bookmark-recent
;;;;;;  delicious-delete-href-post delicious-rename-tag delicious-clear-cache
;;;;;;  delicious-post-cache delicious-post-offline delicious-post
;;;;;;  delicious-sync-posts) "../delicious" "../delicious.el" (20227
;;;;;;  32503))
;;; Generated autoloads from delicious.el

(autoload 'delicious-sync-posts "delicious" "\
Bring the local copy of posts into sync with the Delicious server.
If FORCE is non-nil, or if a prefix argument is given
interactively, unconditionally replace the local with the remote
version.

\(fn &optional FORCE)" t nil)

(autoload 'delicious-post "delicious" "\
Post a bookmark with arguments URL, DESCRIPTION, TAGS, EXTENDED, and TIME.
If NOLOCAL is non-nil, don't add the post to the local list.

\(fn URL DESCRIPTION &optional TAGS EXTENDED TIME NOLOCAL)" t nil)

(autoload 'delicious-post-offline "delicious" "\
Input bookmarks to post later.  Don't contact the server for anything.

\(fn URL DESCRIPTION &optional TAGS EXTENDED TIME)" t nil)

(autoload 'delicious-post-cache "delicious" "\
Post bookmarks from `delicious-cache-file', or CACHE-FILE if non-nil.

\(fn &optional CACHE-FILE)" t nil)

(autoload 'delicious-clear-cache "delicious" "\
Delete `delicious-cache-file' or CACHE-FILE and kill the buffer visiting it.

\(fn &optional CACHE-FILE)" t nil)

(autoload 'delicious-rename-tag "delicious" "\
Change all instances of OLD-TAG to NEW-TAG.
NEW-TAG can be multiple tags, comma-separated.

\(fn OLD-TAG NEW-TAG)" t nil)

(autoload 'delicious-delete-href-post "delicious" "\
Delete the post with URL HREF.

\(fn HREF)" t nil)

(autoload 'delicious-w3m-bookmark-recent "delicious" "\
Add your COUNT recent Delicious posts with TAG to your w3m bookmarks file.
They will be stored under SECTION.

\(fn COUNT TAG SECTION)" t nil)

(autoload 'delicious-w3m-export "delicious" "\
Export your w3m bookmarks from SECTION to Delicious.
Optionally assign TAGS, EXTENDED description, and TIME to the bookmarks.

\(fn SECTION &optional TAGS EXTENDED TIME)" t nil)

(autoload 'delicious-search-posts-regexp "delicious" "\
Display all posts matching REGEXP string in any of their fields.
With a prefix argument, operate offline.

\(fn REGEXP)" t nil)

(autoload 'delicious-search-description-regexp "delicious" "\
Display all posts matching REGEXP string in their description fields.
With a prefix argument, operate offline.

\(fn REGEXP)" t nil)

(autoload 'delicious-search-href-regexp "delicious" "\
Display all posts with URL matching REGEXP.
With a prefix argument, operate offline.

\(fn REGEXP)" t nil)

(autoload 'delicious-search-tags "delicious" "\
Display all posts with TAGS.  With a prefix argument, operate offline.

\(fn TAGS)" t nil)

(autoload 'delicious-search-tags-any "delicious" "\
Display all posts matching any of TAGS.

\(fn TAGS)" t nil)

(autoload 'delicious-search-date "delicious" "\
Display all posts matching regexp SEARCH-DATE.
With a prefix argument, operate offline.

\(fn DATE)" t nil)

(autoload 'delicious-search-hash "delicious" "\
Display the post with hash HASH.
With a prefix argument, operate offline.

\(fn HASH)" t nil)

;;;***
