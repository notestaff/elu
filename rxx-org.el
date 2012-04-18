
(require 'elu)
(require 'rxx)
(require 'elu-valu)

(def-rxx-namespace org
  "Org mode expressions"
  :imports (std elu-valu)
  )

;; should this be def-rxx-regexp instead?

;; so, next thing will be:
;; to make a routine that takes a namespace name and regexp name,
;; and returns the fully constructed regexp and its parser.

;; add error checking etc:

;;    -- when doing imports, make sure conflicting symbols are not imported from different namespaces
;;    -- when adding a symbol to a namespace, make sure it does not conflict with ones imported
;;    (or say it will override them?)

;;    -- in an expr, allow specifying a symbol's namespace, as in (from-namespace namespace symbol).
;;    -- in :import, allow specifying particular symbols to import from a namespace, or :except to import everything
;;       except given ones.
;;    

;; it will be called by the top-level functions such as rxx-string-match and rxx-parse etc,
;; but also where currently rxx-symbol is called.

;; what it will do is:

;;   - check if there is a buffer-local non-void setting for this already; if yes, just return that.
;;   - if not, we'll compute the result then store it buffer-locally.
;;   - to compute the result:
;;       - bind rxx-cur-namespace to the namespace
;;       - call rxx-to-string on the global definition of the symbol

;;         when that encounters a symbol and needs to know if the symbol is an aregexp,
;;         it will check rxx-cur-namespace and its imports, and look for the symbol there.

;;    - if namespace is not specified, get the scoped-in one (error if not there).
;;

;
; afterward, replace all uses of advice with elu-flet.
;

; so, this way, the expressions do not refer to each other -- they're just symbols.
; now, when we need to interpret them, at that point we do that by constructing symbol names
; according to the standard convention.
; so the binding is as late as possible.
; and, 

(def-rxx-regexps org

  (headline "A headline"
	    (seq bol (sep-by blanks stars todo? priority-cookie? headline-text? stats-cookie? tags?) eol)
	    (list stars priority-cookie todo (elu-trim-whitespace headline-text)
		  stats-cookie tags))

  (stars "The initial stars of an Org headline.  Parses as the level."
	 (seq bol (sep-by (eval (if org-odd-levels-only "*" ""))
		    (1+ (named-grp star "*")))) (length star-list))

  ((todo :args (&optional (which-keywords org-todo-keywords-1)) :case-fold-search t)
   "The todo keyword" (word-from-list (which-keywords)))

  (not-done-todo "A not-done todo keyword" (todo (org-not-done-keywords)))
  
  (priority-char "A priority character.  Parsed as the priority char."
		 (eval-regexp (format "[%c-%c]" org-highest-priority org-lowest-priority))
		 string-to-char)

  (priority-cookie "A priority cookie.  Parsed as the priority char."
		   (seq "[#" priority-char "]") priority-char)

  (tag "Tag name, parses as tag name" (1+ (any alnum "_@#%")))
  (tags "List of tags.  Parses as list of tags."  (& blanks? (opt ":" (1+ (& tag ":")) eol)) tag-list)

  (headline-text "Headline text" (minimal-match (1+ nonl)))

  (int "an integer.  parses as its value." (1+ digit) string-to-number)
  (int-ratio "a ratio.  parses as a floating-point value of the ratio." (seq (int numer) "/" (int denom)) (/ (float numer) (float denom)))

  (percentage "a percentage.  parses as a floating-point value of the percentage."
	      (seq int "%") (/ (float int) 100.0)) 
  
  (stats-cookie "statistics cookie" (seq "[" (or (int-ratio val) (percentage val)) "]") val)

  
  (matcher "A tags-and-properties matcher.  Parses as the corresponding form."
	   (seq tags-and-props-matcher? (opt "/" todo-matcher))
	   `(and ,tags-and-props-matcher ,todo-matcher))

  (tags-and-props-matcher "Tags and properties matcher."
			  (sep-by "|" (1+ tags-and-props-term)) (cons 'or tags-and-props-term-list))
  (tags-and-props-term "One term of the tags-and-props matcher"
		       (sep-by (opt "&") (1+ tags-and-props-cond)) (cons 'and tags-and-props-cond-list))

  (sign "a sign" (any "+-"))
  (braced-regexp "regexp in braces" (parens ("{" "}")) parens)
  (tags-and-props-cond "One condition"
		       (seq (opt sign) (or tag braced-regexp)) (list :sign (or sign "+") :tag tag :regexp braced-regexp))

  (todo-matcher "Todo matcher."
		(1+ alnum))
  )

;; so, one solution is to have the module explicitly define a const containing the exported regexps.
;; that's not such a bad solution, and is what i had before.

;; so, in the same clause where it says import, we can say what we export.
;; now, this is not quite ideal as import is implementation detail and export is the interface.
;; but this would definitely be usable and probably safe.

;; now, otherwise, where will we put things?
;; suppose we do def-rxx-namespace as eval-and-compile.

;; another could be to use defvar.  

;; (defrxx org tag-name "Tag name, parses as tag name" (1+ (any alnum "_@#%")))
;; (defrxx org tags "List of tags.  Parses as list of tags."  (& blanks? (opt ":" (1+ (& tag-name ":")) eol)) tag-name-list)

;; (defrxx org stars "Stars at start of headline.  Parses as the level, taking odd-only into account."
;;   (seq bol (0+ (named-grp star "*") :sep-by (if org-odd-levels-only "*" "")))
;;   (length star-list))

;; (defrxx org todo "A todo keyword" (eval-words org-todo-keywords-1))

;; (defrxx org headline-text "Headline text" (1+ nonl))
;; (defrxx org stats-cookie "statistics cookie" (seq "[" alnum "/" alnum "]"))

;; (defrxx org headline (sep-by blanks stars priority? todo? headline-text stats-cookie? tags?) 'parse-as-struct)

;; ;; special support for multi-line regexps?
			     
;; (defrxxnamespace org-balance :import (org elu-valu))
;; (defrxx org-balance goal-headline (replace-grp headline todo (eval-words org-balance-goal-keywords)) 'parse-as-struct)

;; ;; allow (defrxx org-balance (def1) (def2))

;; (defrxx org-balance link "An Org link. Parsed as an elu-loc at the target of the link."
;;   (named-grp link (eval-regexp (rxx-make-shy org-any-link-re)))
;;   (elu-save excursion restriction window-excursion match-data
;;     (goto-char (rxx-match-beginning 'link))
;;     (org-balance-open-at-point)
;;     (point-elu-loc)))

;; so then, need to implement get-symbol that takes a namespace and a name in it,
;; as well as, separately, that just takes a name and uses the current scoped-in namespace.

;; 

(defconst rxx-org-test-defs
  '(
    (stars ((org-odd-levels-only t)) "***" 2)
    (stars ((org-odd-levels-only nil)) "***" 3)
    (stars ((org-odd-levels-only t)) "****" (rxx-parse-error "Error parsing `****' as `The initial stars of an Org headline.  Parses as the level.': match ends at 3"))
    (todo ((org-todo-keywords-1 ("TODO" "DONE"))) "TODO" str)
    (headline ((org-odd-levels-only t) (org-highest-priority ?A) (org-lowest-priority ?C) (org-todo-keywords-1 ("TODO" "DONE")))
	      "* Vsem privet"
	      (1 nil nil "Vsem privet" nil nil))
    (headline ((org-odd-levels-only t) (org-highest-priority ?A) (org-lowest-priority ?C)  (org-todo-keywords-1 ("TODO" "DONE")))
	      "*** TODO [#A] Vsem privet   :new:work:"
	      (2 ?A "TODO" "Vsem privet"  nil ("new" "work")))
    (int-ratio nil "3/4" .75)
    (percentage nil "33%" .33)
    (headline ((org-odd-levels-only t)  (org-highest-priority ?A) (org-lowest-priority ?C) (org-todo-keywords-1 ("TODO" "DONE")))
	      "*** TODO [#A] Vsem privet [2/4]   :new:work:"
	      (2 ?A "TODO" "Vsem privet"  .5 ("new" "work")))
    (headline ((org-odd-levels-only t)  (org-highest-priority ?A) (org-lowest-priority ?C) (org-todo-keywords-1 ("TODO" "DONE")))
	      "*** TODO [#A] Vsem privet [75%]   :new:work:"
	      (2 ?A "TODO" "Vsem privet"  .75 ("new" "work")))
    (tags-and-props-matcher () "{privet}&-poka|+work-home" (or (and (:sign "+" :tag nil :regexp "privet") (:sign "-" :tag "poka" :regexp nil)) (and (:sign "+" :tag "work" :regexp nil) (:sign "-" :tag "home" :regexp nil))))
;    (matcher () "privet/lunatikam" (and "privet" "lunatikam"))
    ))

(defun rxx-org-tests ()
  (interactive)
  ;; TODO add code to run this on actual org files.  maybe compare results with the new parser.
  (rxx-reset)
  (let ((num-ok 0))
    (dolist (test-def rxx-org-test-defs)
      (destructuring-bind (name var-settings str expected-result) test-def
	(message "testing test %s" test-def)
	(let ((expected-result (if (eq expected-result 'str) str expected-result))
	      (cur-result
	       (elu-progv (mapcar 'car var-settings) (mapcar 'cadr var-settings)
		 (rxx-reset)
		 (condition-case err
		     (rxx-parse-string-func 'org name str)
		   (rxx-parse-error err)))))
	  (if (equal cur-result expected-result)
	      (incf num-ok)
	    (message "\n----------\n\nexp=%s\ncur=%s\n---------\n" expected-result cur-result)
	    (error "rxx org test failed: test=%s got=%s"
		   test-def cur-result)))))
    (message "%s tests passed" num-ok)))

(rxx-org-tests)

;(rxx-parse-string org matcher "privet/lunatikam")
