;;; tlsf-mode.el --- Temporal Logic Synthesis Format Mode

;; Author: Felix Klein
;; Maintainer: Felix Klein <fklein@cs.uni-saarland.de>
;; Created: September 2015
;; Last Modified: 18 October 2015
;; Version: 1.0
;;
;; This is a major-mode which provides syntax highlighting and
;; indentation for Specifications in the (extended) Tempral Logic
;; Synthesis Format (TLSF). The mode supports prettify-symobls-mode.


;; To use the mode load the file and put the following line in you .emacs:
;; 
;; (require 'tlsf-mode)
;;
;; If you wanna use pretty utf8 symbols (requires >= Emacs 24.4) instead
;; of the boring ascii ones also add the following two lines
;; (in the given order):
;;
;; (add-hook 'tlsf-mode-hook 'prettify-symbols-mode)
;; (add-hook 'tlsf-mode-hook 'tlsf-pretty-symbols)
;;
;; Finally, reload your emacs configuration and enjoy ;-).


;; Implementation:

(defvar tlsf-builtin
  '("G" "F" "X" "U" "R" "W" "AND" "OR"    
    "Mealy" "Moore" "Strict" "Finite" "NOT" "IN"
    "EQUIV" "IMPLIES" "true" "false" "MIN" "MAX"
    "ELEM" "SIZE" "SIZEOF" "MUL" "DIV" "MOD"
    "PLUS" "MINUS" "SETMINUS" "CAP" "CUP" "EQ"
    "NEQ" "LE" "LEQ" "GE" "GEQ" "SUM" "PROD"
    "FORALL" "EXISTS")
  "Advanced LTL Format builtins")

(defvar tlsf-kw
  '("INFO" "TITLE" "DESCRIPTION" "SEMANTICS" "TARGET"
    "GLOBAL" "PARAMETERS" "DEFINITIONS" "OUTPUTS"
    "INPUTS" "ASSUMPTIONS" "INVARIANTS" "TAGS" "ASSUME"
    "GUARANTEES" "MAIN" "ASSERT" "REQUIREMENTS"
    "INITIALLY" "PRESET" "REQUIRE" "GUARANTEE" "enum")
  "Advanced LTL Format keywords")

(defvar tlsf-connectives-regexp
  "([+])\\|([*])\\|(/)\\|(<)\\|(>)\\|\!\\|&&\\|||\\|->\\|<->\\|==\\|<=\\|>=\\|/=\\|!=\\|<\\|>\\|[+]\\|-\\|[*]\\|/\\|%\\||\\|~\\|\\.\\.")

(defvar tlsf-indent-keywords
  "^[ \t]*\\(INFO\\|GLOBAL\\|MAIN\\|PARAMETERS\\|DEFINITIONS\\|OUTPUTS\\|INPUTS\\|ASSUMPTIONS\\|INVARIANTS\\|GUARANTEES\\|SEMANTICS\\|TARGETS\\|TAGS\\)")

(defvar tlsf-function-regexp "^\\s-*\\(\\w+\\)\\s-*\\((.*)\\)?\\s-*=")
(defvar tlsf-args-regexp "\\(\\w+\\)[,\\|)]")
(defvar tlsf-enum-regexp "^\\s-*enum\\s-*\\(\\w+\\)\\s-*=")
(defvar tlsf-bus-regexp "\\[\\(\\w+\\)\\]")
(defvar tlsf-typedbus-regexp "^\\s-*\\(\\<\\w+\\>\\)\\s-*\\<\\w+\\>\\s-*;")
 
(defvar tlsf-kw-regexp (regexp-opt tlsf-kw 'words))
(defvar tlsf-builtin-regexp (regexp-opt tlsf-builtin 'words))

(setq tlsf-keywords
 `((,tlsf-kw-regexp . font-lock-keyword-face)
   (,tlsf-builtin-regexp . font-lock-builtin-face)
   (,tlsf-connectives-regexp . font-lock-variable-name-face)
   (,tlsf-function-regexp (1 font-lock-function-name-face))
   (,tlsf-bus-regexp 1 font-lock-type-face)
   (,tlsf-typedbus-regexp 1 font-lock-type-face)   
   (,tlsf-enum-regexp 1 font-lock-type-face) 
   (,tlsf-function-regexp
    "\\<\\w*\\>"
    (progn
      (re-search-backward "(")
      (save-excursion
        (search-forward-regexp "[^)]*)")))
    nil
    (0 font-lock-constant-face))))

(defun tlsf-indent-line ()
  "Simple indentation of a line in TLSF code"
  (interactive)
  (beginning-of-line)
  (if (bobp) (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (if (looking-at "^[ \t]*}")
          (progn
            (save-excursion
              (forward-line -1)
              (setq cur-indent (- (current-indentation) 2)))
            (if (< cur-indent 0) (setq cur-indent 0)))
        (save-excursion
          (while not-indented
            (forward-line -1)
            (if (looking-at "^[ \t]*}")
                (progn
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil))
              (if (looking-at tlsf-indent-keywords)
                  (progn
                    (setq cur-indent (+ (current-indentation) 2))
                    (setq not-indented nil))
                (if (bobp)
                    (setq not-indented nil)))))))
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0)))))                 

(defun tlsf-pretty-symbols ()
  "Pretty Unicode Symbols"
  (setq
   prettify-symbols-alist
   '(("(+)" . ?∪)
     ("CUP" . ?∪)
     ("(*)" . ?∩)
     ("CAP" . ?∩)     
     ("(/)" . ?∕)
     ("SETMINUS" . ?∕)
   ;  ("(<)" . ?⊆)
   ;  ("(>)" . ?⊇)
     ("&&" . ?∧)
     ("AND" . ?∧)
     ("FORALL" . ?∧)     
     ("||" . ?∨)
     ("OR" . ?∨)
     ("EXISTS" . ?∨)
     ("->" . ?→)
     ("IMPLIES" . ?→)     
     ("<-" . ?∈)
     ("IN" . ?∈)
     ("ELEM" . ?∈)     
     ("<->" . ?↔)
     ("EQUIV" . ?↔)     
     ("/=" . ?≢)
     ("!=" . ?≢)     
     ("NEQ" . ?≢)     
     ("<=" . ?≤)
     ("LEQ" . ?≤)     
     (">=" . ?≥)
     ("GEQ" . ?≥)
     ("LE" . ?<)
     ("GE" . ?>)               
     ("==" . ?≡)
     ("EQ" . ?≡)
     ("!" . ?¬)
     ("NOT" . ?¬)          
     ("~" . ?∼)
     ("{}" . ?∅)
     ("G" . ?□)
     ("F" . ?◇)
     ("X" . ?◯))))

(define-derived-mode tlsf-mode fundamental-mode
  (setq mode-name "TLSF Mode")

  (modify-syntax-entry ?/ ". 124b" tlsf-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" tlsf-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" tlsf-mode-syntax-table)
  (modify-syntax-entry ?_ "w" tlsf-mode-syntax-table)
  (modify-syntax-entry ?' "w" tlsf-mode-syntax-table)

  (setq font-lock-defaults '(tlsf-keywords))  
  
  (set (make-local-variable 'indent-line-function) 'tlsf-indent-line))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tlsf\\'" . tlsf-mode))

(provide 'tlsf-mode)


