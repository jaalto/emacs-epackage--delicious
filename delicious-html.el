;;;;_+ HTML

(defconst delicious-api-html "/html/"
  "*The path to the del.icio.us HTML feed.
It should begin and end with a slash.")

(defcustom delicious-api-html-count 15
  "*The number of times to show by default when fetching an HTML feed.
The server default is 15."
  :version "21.3.1"
  :group 'delicious
  :type 'integer
  :tag "HTML item count parameter")

(defcustom delicious-api-html-extended "title"
  "*Either 'title' or 'body'.  The server default is `title'.
This means that the extended description will just be shown in the title
attribute of the link tag.  If you want it displayed on its own, use `body'."
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
  "*Show tags or not.  Server default is to show tags.
Set this to true if you do NOT want tags."
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
  "*HTML entity to use for bullet.  Set it to empty for no bullet. 
Server default is `raquo'."
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

(defun delicious-api-html
  (&optional username tagname count extended divclass aclass tags tagclass
             tagsep tagsepclass bullet rssbutton extendeddiv extendedclass)
  "Return results formatted in HTML, according to a long list of options.
USERNAME is the name of the user whose links you want to fetch.  If you don't
specify a name, `delicious-api-user' will be used.  If TAGNAME is nil, then
results from all of the user's tags will be used.  If TAGNAME is passed, only
posts under that tag will be considered. COUNT is the number of items to show.
It defaults to 15 at the server.  EXTENDED is either 'title' or 'body'.  It
defaults to 'title'.  DIVCLASS is the name of the CSS class to use for the div
elements.  It defaults to 'delPost'.  ACLASS is the CSS class to use for the
link elements.  It defaults to 'delLink'.  If TAGS is non-nil, don't show tags.
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
  (delicious-api-send-request
   (delicious-api-build-html-request
    (delicious-api-html-uri username tagname count extended divclass aclass
                            tags tagclass tagsep tagsepclass bullet rssbutton
                            extendeddiv extendedclass)))
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
  (&optional username tagname count extended divclass aclass tags tagclass
             tagsep tagsepclass bullet rssbutton extendeddiv extendedclass)
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

;;;;_+ Assembling requests
  
(defun delicious-api-build-html-request (uri)
  "Return the proper HTTP request to get URI from the HTML feed."
  (let ((uri 
         (format "http://%s%s%s" delicious-api-host delicious-api-html uri)))
    uri))


(provide 'delicious-html)
