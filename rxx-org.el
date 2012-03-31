
(require 'rxx)

(def-rxx-namespace org)

(def-rxx org priority-char "A priority character.  Parsed as the priority char."
   (eval-regexp (format "[%s-%s]" org-highest-priority org-lowest-priority)))

(def-rxx org priority-cookie "A priority cookie.  Parsed as the priority char."
  (seq "[#" priority-char "]") priority-char)

(defrxx org tag-name "Tag name, parses as tag name" (1+ (any alnum "_@#%")))
(defrxx org tags "List of tags.  Parses as list of tags."  (& blanks? (opt ":" (1+ (& tag-name ":")) eol)) tag-name-list)

(defrxx org stars "Stars at start of headline.  Parses as the level, taking odd-only into account."
  (seq bol (0+ (named-grp star "*") :sep-by (if org-odd-levels-only "*" "")))
  (length star-list))

(defrxx org todo "A todo keyword" (eval-words org-todo-keywords-1))

(defrxx org headline-text "Headline text" (1+ nonl))
(defrxx org stats-cookie "statistics cookie" (seq "[" alnum "/" alnum "]"))

(defrxx org headline (sep-by blanks stars priority? todo? headline-text stats-cookie? tags?) 'parse-as-struct)

;; special support for multi-line regexps?
			     
(defrxxnamespace org-balance :import (org elu-valu))
(defrxx org-balance goal-headline (replace-grp headline todo (eval-words org-balance-goal-keywords)) 'parse-as-struct)

;; allow (defrxx org-balance (def1) (def2))

(defrxx org-balance link "An Org link. Parsed as an elu-loc at the target of the link."
  (named-grp link (eval-regexp (rxx-make-shy org-any-link-re)))
  (elu-save excursion restriction window-excursion match-data
    (goto-char (rxx-match-beginning 'link))
    (org-balance-open-at-point)
    (point-elu-loc)))

;; so then, need to implement get-symbol that takes a namespace and a name in it,
;; as well as, separately, that just takes a name and uses the current scoped-in namespace.

;; 




