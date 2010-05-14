;;; eventb-mode.el --- Major Mode to edit Event-B files

;; Copyright (C) 2008, 2009  Free Software Foundation, Inc.

;; Author: Paulo Matos <pocmatos@gmail.com>
;; Keywords: hypermedia
;; Based initially in graphviz-dot-mode.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


(defconst eventb-mode-version "0.1"
  "Version of `eventb-mode.el'.")

(defgroup eventb nil
  "Event-B editing mode (EventB)."
  :group 'languages)

(defun eventb-customize ()
  "Run \\[customize-group] for the `eventb' group."
  (interactive) 
  (customize-group 'eventb))

(defvar eventb-mode-abbrev-table nil
  "Abbrev table in use in Eventb mode buffers.")
(define-abbrev-table 'eventb-mode-abbrev-table ())

;; Key map
(defvar eventb-mode-map ()
  "Keymap used in Eventb mode.")

(if eventb-mode-map
    ()
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-r"   'comment-region)
    (setq eventb-mode-map map)
))

;; Font Locking (Highlighting)
;
;(regexp-opt '("Machine" "Variants" "Invariants" "End" "Initialisation" "Variables" 
;              "Sees" "Context" "Set" "Theorems" "Constants" "Axioms" "Event" "Extends") t)
; (regexp-opt '("any" "where" "status" "when" "then") t)
; (regexp-opt '("finite" "partition" "min" "max" "mod" "id" "card" "prj1" "prj2" "union" 
; "pred" "succ" "dom" "bool" "ran" "<=>" "=>" "&" "or" "!" "#" "." "not" "/=" "<" ">" "<="
;  ">=" "POW" "POW1" ":" "/:" "<<:" "/<<:" "<:" "/<:" "<->" "<<->" "<->>" "<<->>" "-+>" "-->"
;  ">+>" ">->" "+>>" "->>" ">->>" "|->" "\/" "/\\" "\\" "**" "<+" "circ" "><" "||" "~" "<<|" 
;  "<|" "|>" "Union" "Inter" ".." "+" "-" "div" "^" "|") t)
(defvar eventb-font-lock-keywords
  (list 
   '("\\<\\(Machine\\|Variants\\|Invariants\\|End\\|Initialisation\\|Variables\\|Sees\\|Context\\|Sets\\|Theorems\\|Constants\\|Axioms\\|Event\\|Extends\\)\\>" . font-lock-keyword-face)
   '("\\<\\(any\\|status\\|then\\|when\\|where\\|end\\)\\>" . font-lock-function-name-face)
   '("\\<\\(finite\\|partition\\|min\\|max\\|mod\\|id\\|card\\|prj1\\|prj2\\|union\\|pred\\|succ\\|dom\\|bool\\|ran\\|<=>\\|=>\\|&\\|or\\|!\\|#\\|\\.\\|not\\|/=\\|<\\|>\\|<=\\|>=\\|POW\\|POW1\\|:\\|/:\\|<<:\\|/<<:\\|<:\\|/<:\\|<->\\|<<->\\|<->>\\|<<->>\\|-\\+>\\|-->\\|>\\+>\\|>->\\|\\+>>\\|->>\\|>->>\\||->\\|\\\\/\\|/\\\\\\|\\\\\\|\\*\\*\\|<\\+\\|circ\\|><\\|||\\|~\\|<<|\\|<|\\||>\\|Union\\|Inter\\|\\.\\.\\|\\+\\|-\\|div\\|\\^\\||\\)\\>" . font-lock-builtin-face)
   '("\\<\\(BOOL\\|FALSE\\|INT\\|NAT\\|NAT1\\|TRUE\\|bfalse\\|btrue\\)\\>" . font-lock-constant-face)
   '("\\@\\w*" . font-lock-string-face)
   '("\\('\\w*'\\)" . font-lock-variable-name-face))
  "Highlighting keywords for Eventb mode")


;; Syntax table
(defvar eventb-mode-syntax-table
  (let ((eventb-mode-syntax-table (make-syntax-table)))
    
    ;; This entry to the syntax table lets Emacs that allowed comments in EventB are
    ;; the same as in C++.
    (modify-syntax-entry ?/ ". 124b" eventb-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" eventb-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" eventb-mode-syntax-table)
    eventb-mode-syntax-table)
  "Syntax table for eventb-mode")


;; Indentation in EventB
(defcustom eventb-indent-offset 4
  "Basic size of one indentation step."
  :version "22.2"
  :type 'integer
  :group 'eventb)

;; Rules for EventB code indentation
;; 1. If we are at the beginning of the buffer or End line indent to column 0.
;; 2. If we see a major new block like Variables or Event, then we indent 1.
;; 3. If we are currently at an end line, then de-indent relative to previous line.
;; 4. If we are at a minor new block like any, where, etc, then we indent relative to previous major.
;; 5. If none of the above apply then don't indent at all.
;;
;;
(defun eventb-indent-line ()
  "Indent current line as EventB code"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (cond ((or (bobp) (looking-at "^[ \t]*End")) ;; Rule 1
           (indent-line-to 0)) 
          ((looking-at "^[ \t]*\\(Axioms\\|Constants\\|Contexts\\|Event\\|Extends\\|Initialisation\\|Invariants\\|Machine\\|Sees\\|Sets\\|\\Theorems\\|Variables\\|Variants\\)") ;; Rule 2
           (indent-line-to default-tab-width))
          ((looking-at "^[ \t]*end") ; Rule 3
           (let (cur-indent)
             (save-excursion 
               (forward-line -1)
               (setq cur-indent (- (current-indentation) default-tab-width)))
             (indent-line-to cur-indent)))
          ((looking-at "^[ \t]*\\(any\\|when\\|where\\|then\\|status\\)") ; Rule 4
           (let ((not-indented t) cur-indent)
           (save-excursion 
             (while not-indented
               (forward-line -1)
               (if (looking-at "^[ \t]*\\(Axioms\\|Constants\\|Contexts\\|Event\\|Extends\\|Initialisation\\|Invariants\\|Machine\\|Sees\\|Sets\\|\\Theorems\\|Variables\\|Variants\\)") 
                   (progn 
                     (setq cur-indent (+ (current-indentation) default-tab-width))
                     (setq not-indented nil))
                 (if (bobp) ;; Rule 5
                     (setq not-indented nil)))))
           (if cur-indent 
               (indent-line-to cur-indent)
             (indent-line-to 0))))
        (t 
         (let (cur-indent)
           (save-excursion
             (forward-line -1)
             (if (or (looking-at "^[ \t]*\\(any\\|when\\|where\\|then\\|status\\)")
                     (looking-at "^[ \t]*\\(Axioms\\|Constants\\|Contexts\\|Event\\|Extends\\|Initialisation\\|Invariants\\|Machine\\|Sees\\|Sets\\|\\Theorems\\|Variables\\|Variants\\)"))
                 (setq cur-indent (+ (current-indentation) default-tab-width))
               (setq cur-indent (current-indentation))))
           (indent-line-to cur-indent))))))
  
;; The following font locks the editor so that ascii chars are translated 
;; to UNICODE.
;(defun unicode-symbol (name)
;  "Translate a symbolic name for a Unicode character -- e.g., LEFT-ARROW                                      
; or GREATER-THAN into an actual Unicode character code. "
;  (decode-char 'ucs (case name  
;;          ('defi #2259)
;;          ('oftype #2982)
;;		      ('nat  #X2115)
;; 		      ('nat1 #X2115) ;; This is subst by 2115 and 0031
;; 		      ('int  #X2124)
;; 		      ('pow  #X2119) 
;; 		      ('pow1 #X2119) ;; Same as nat1
;; 		      ;; Predicate Symbols
;; 		      ('leqv #X21d4)
;; 		      ('limp #X21d2)
;; 		      ('land #X2227)
;; 		      ('lor  #X2228)
;; 		      ('lnot #X00ac)
;; 		      ('btrue #X22a4)
;; 		      ('bfalse #X22a5)
;; 		      ('forall #X2200)
;; 		      ('exists #X2203)
;; 		      ('qdot #X00b7)
;; 		      ('notequal #X2260)
;; 		      ('le #X2264)
;; 		      ('ge #X2265)
;; 		      ('in #X2208)
;; 		      ('notin #X2209)
;; 		      ('subset #X2282)
;; 		      ('subseteq #X2286)
;; 		      ('notsubset #X2284)
;; 		      ('notsubseteq #X2288)
;; 		      ;; Expression Symbols
;; 		      ('rel #X2194)
;; 		      ('trel #Xe100)
;; 		      ('srel #Xe101)
;; 		      ('strel #Xe102)
;; 		      ('pfun #X21f8)
;; 		      ('tfun #X2192)
;; 		      ('pinj #X2914)
;; 		      ('tinj #X21a3)
;; 		      ('psur #X2900)
;; 		      ('tsur #X21a0)
;; 		      ('bij #X2916)
;; 		      ('maplet #X21a6)
;; 		      ('emptyset #X2205)
;; 		      ('binter #X2229)
;; 		      ('bunion #X222a)
;; 		      ('setminus #X2216)
;; 		      ('cprod #X00d7)
;; 		      ('ovl #Xe103)
;; 		      ('bcomp #X2218)
;; 		      ('dprod #X2297)
;; 		      ('converse #X223c)
;; 		      ('domres #X25c1)
;; 		      ('domsub #X2a64)
;; 		      ('ranres #X25b7)
;; 		      ('ransub #X2a65)
;; 		      ('lambda #X03bb)
;; 		      ('bigunion #X22c2)
;; 		      ('biginter #X22c3)
;; 		      ('upto #X2025)
;; 		      ('div #X00f7))))


;; (defun substitute-pattern-with-unicode (pattern symbol)
;;   "Add a font lock hook to replace the matched part of PATTERN with the                                       
;;      Unicode symbol SYMBOL looked up with UNICODE-SYMBOL."
;;   (interactive)
;;   (font-lock-add-keywords
;;    nil `((,pattern 
;; 	  (0 (progn (compose-region (match-beginning 1) (match-end 1)
;; 				    ,(unicode-symbol symbol)
;; 				    'decompose-region)
;; 		    nil))))))

;; (defun substitute-patterns-with-unicode (patterns)
;;   "Call SUBSTITUTE-PATTERN-WITH-UNICODE repeatedly."
;;   (mapcar #'(lambda (x)
;; 	      (substitute-pattern-with-unicode (car x)
;; 					       (cdr x)))
;; 	  patterns))

;; (defun eventb-unicode ()
;;   (interactive)
;;   (substitute-patterns-with-unicode
;;    (list ;; Identifiers
;;     (cons "\\(NAT\\)" 'nat)
;;     (cons "\\(NAT1\\)" 'nat1)
;;     (cons "\\(INT\\)" 'int)
;;     (cons "\\(POW\\)" 'pow)
;;     (cons "\\(POW1\\)" 'pow1)
;;     (cons "\\(==\\)" 'defi)
;;     (cons "\\(oftype\\)" 'oftype)
;;     ;; Predicate Symbols
;;     (cons "\\(<=>\\)" 'leqv)
;;     (cons "\\(=>\\)" 'limp)
;;     (cons "\\(&\\)" 'land)
;;     (cons "\\(or\\)" 'lor)
;;     (cons "\\(not\\)" 'lnot)
;;     (cons "\\(btrue\\)" 'btrue)
;;     (cons "\\(bfalse\\)" 'bfalse)
;;     (cons "\\(!\\)" 'forall)
;;     (cons "\\(#\\)" 'exists)
;;     (cons "\\(\\.\\)" 'qdot)
;;     (cons "\\(/=\\)" 'notequal)
;;     (cons "\\(<=\\)" 'le)
;;     (cons "\\(>=\\)" 'ge)
;;     (cons "\\(:\\)" 'in)
;;     (cons "\\(/:\\)" 'notin)
;;     (cons "\\(<:\\)" 'subset)
;;     (cons "\\(<<:\\)" 'subseteq)
;;     (cons "\\(/<:\\)" 'notsubset)
;;     (cons "\\(/<<:\\)" 'notsubseteq)
;;     ;; Expression Symbols
;;     (cons "\\(<->\\)" 'rel)
;;     (cons "\\(<<->\\)" 'trel)
;;     (cons "\\(<->>\\)" 'srel)
;;     (cons "\\(<<->>\\)" 'strel)
;;     (cons "\\(-+>\\)" 'pfun)
;;     (cons "\\(-->\\)" 'tfun)
;;     (cons "\\(>+>\\)" 'pinj)
;;     (cons "\\(>->\\)" 'tinj)
;;     (cons "\\(-+>>\\)" 'psur)
;;     (cons "\\(-->>\\)" 'tsur)
;;     (cons "\\(>->>\\)" 'bij)
;;     (cons "\\(|->\\)" 'maplet)
;;     (cons "\\({}\\)" 'emptyset)
;;     (cons "\\(/\\\\)" 'binter)
;;     (cons "\\(\\/\\)" 'bunion)
;;     (cons "\\(\\\\)" 'setminus)
;;     (cons "\\(**\\)" 'cprod)
;;     (cons "\\(<+\\)" 'ovl)
;;     (cons "\\(circ\\)" 'bcomp)
;;     (cons "\\(><\\)" 'dprod)
;;     (cons "\\(~\\)" 'converse)
;;     (cons "\\(<|\\)" 'domres)
;;     (cons "\\(<<|\\)" 'domsub)
;;     (cons "\\(|>\\)" 'ranres)
;;     (cons "\\(|>>\\)" 'ransub)
;;     (cons "\\(lambda\\)" 'lambda)
;;     (cons "\\(Union\\)" 'bigunion)
;;     (cons "\\(Inter\\)" 'biginter)
;;     (cons "\\(\\.\\.\\)" 'upto)
;;     (cons "\\(div\\)" 'div))))

;; (add-hook 'eventb-mode-hook 'eventb-unicode)

;;;###autoload
(defun eventb-mode ()
  "Major mode for Eventb. \\<eventb-mode-map>

\\[eventb-indent-line]\t- Indents current line of code.

Variables specific to this mode:

This mode can be customized by running \\[eventb-customize].

Turning on Eventb mode calls the value of the variable
`eventb-mode-mode' with no args, if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map eventb-mode-map)
  (setq mode-name "EventB")
  (setq local-abbrev-table eventb-mode-abbrev-table)
	(setq indent-tabs-mode nil)
  (setq case-fold-search nil)
  (set-syntax-table eventb-mode-syntax-table)
  (set (make-local-variable 'indent-line-function) 'eventb-indent-line)
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-start-skip) "/\\*+ *\\|//+ *")
  (set (make-local-variable 'font-lock-defaults)
       '(eventb-font-lock-keywords))
  (run-hooks 'eventb-mode-hook)
)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mch\\'" . eventb-mode))
(add-to-list 'auto-mode-alist '("\\.ctx\\'" . eventb-mode))

(provide 'eventb-mode)	     
;;; eventb-mode.el ends here