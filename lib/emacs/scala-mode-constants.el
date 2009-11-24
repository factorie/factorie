;;; -*-Emacs-Lisp-*-
;;; scala-mode-constants.el - 

;; Copyright (C) 2009 Scala Dev Team at EPFL
;; Authors: See AUTHORS file
;; Keywords: scala languages oop
;; $Id: scala-mode-constants.el 17069 2009-02-10 08:30:51Z nielsen $

;;; License

;; SCALA LICENSE
;;  
;; Copyright (c) 2002-2009 EPFL, Lausanne, unless otherwise specified.
;; All rights reserved.
;;  
;; This software was developed by the Programming Methods Laboratory of the
;; Swiss Federal Institute of Technology (EPFL), Lausanne, Switzerland.
;;  
;; Permission to use, copy, modify, and distribute this software in source
;; or binary form for any purpose with or without fee is hereby granted,
;; provided that the following conditions are met:
;;  
;;    1. Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;  
;;    2. Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;  
;;    3. Neither the name of the EPFL nor the names of its contributors
;;       may be used to endorse or promote products derived from this
;;       software without specific prior written permission.
;;  
;;  
;; THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.

;;; Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'scala-mode-constants)

(require 'cl)
(require 'regexp-opt)

;; Helper functions

(defun scala-regexp-opt-charset (chars)
  ;;
  ;; Return a regexp to match a character in CHARS.
  ;;
  ;; The basic idea is to find character ranges.  Also we take care in the
  ;; position of character set meta characters in the character set regexp.
  ;;
  (let* ((charmap (make-char-table 'case-table))
     (start -1) (end -2)
     (charset "")
     (bracket "") (dash "") (caret ""))
    ;;
    ;; Make a character map but extract character set meta characters.
    (dolist (char chars)
      (case char
	(?\]
	 (setq bracket "]"))
	(?^
	 (setq caret "^"))
	(?-
	 (setq dash "-"))
	(otherwise
	 (aset charmap char t))))
    ;;
    ;; Make a character set from the map using ranges where applicable.
    (map-char-table
     (lambda (c v)
       (when v
	 (if (listp c) (setq start (car c) end (cdr c)) 
	   (if (= (1- c) end) (setq end c)
	     (if (> end (+ start 2))
		 (setq charset (format "%s%c-%c" charset start end))
              (while (>= end start)
                (setq charset (format "%s%c" charset start))
                (incf start)))
	     (setq start c end c)))))
     charmap)
    (when (>= end start)
      (if (> end (+ start 2))
	  (setq charset (format "%s%c-%c" charset start end))
	(while (>= end start)
	  (setq charset (format "%s%c" charset start))
	  (incf start))))
    ;;
    ;; Make sure a caret is not first and a dash is first or last.
    (if (and (string-equal charset "") (string-equal bracket ""))
	(concat "[" dash caret "]")
      (concat "[" bracket charset caret dash "]"))))


;; Constants


(defconst scala-number-re
  "[[:digit:]]+\\(\\.[[:digit:]]+\\)?\\([eE][+-]?[[:digit:]]+\\)?[fl]?"
  "Regular expression matching a Scala number (integer or float).")

(defconst scala-rawstring-re
  "\"\"\"[^\"\"\"]*\"\"\""
  "Regular expression matching a Scala raw string literal.")

(defconst scala-string-re
  "\"\\([^\"\\\\]\\|\\\\\.\\)*\""
  "Regular expression matching a Scala string literal.")

(defconst scala-char-re
  "'\\([^\\\\]\\|\\(\\\\[^']\\)\\)'"
  "Regular expression matching a Scala character literal.")

(defconst scala-literal-re
  (concat "\\(" "\\(" scala-number-re "\\)"
          "\\|" "\\(" scala-rawstring-re "\\)"
          "\\|" "\\(" scala-string-re "\\)"
          "\\|" "\\(" scala-char-re "\\)" "\\)")
  "Regular expression matching any Scala literal.")

(defconst scala-most-special-chars (mapcar 'identity "<>+-*/|@#%&!?$^`~")
  "List of almost all Scala special characters.
Not included in this list are the special characters which are
reserved keywords when used alone.")

(defconst scala-all-special-chars (append (mapcar 'identity ":;,=")
                                          scala-most-special-chars)
  "List of all Scala special characters.")

(defconst scala-most-special-char-re
  (scala-regexp-opt-charset scala-most-special-chars)
  "Regular expression matching a single Scala special character")

(defconst scala-all-special-char-re
  (scala-regexp-opt-charset scala-all-special-chars)
  "Regular expression matching a single Scala special character")

(defconst scala-keywords-re
  (regexp-opt '("abstract" "case" "class" "catch" "def" "do" "else" "extends"
                "final" "finally" "for" "forSome" "if" "implicit" "import" "lazy"
                "new" "match" "mixin" "object" "override" "package" "private"
                "protected" "requires" "return" "sealed" "super" "this" "throw"
                "trait" "try" "type" "val" "var" "with" "while" "yield")
	      'words))

(defconst scala-constants-re
  (regexp-opt '("true" "false" "null") 'words))

(defconst scala-special-ident-re
  (concat "\\(" scala-all-special-char-re "\\{2,\\}"
          "\\|" scala-most-special-char-re "+"
          "\\)"))

(defconst scala-ident-re
  (let* ((varid-re "[[:alnum:]]+")
         (id-re (concat "\\(" varid-re "\\|" scala-special-ident-re "\\)")))
    (concat id-re
            "\\(" "_+" "\\(" id-re "\\)?" "\\)*"))
  "Regular expression matching a Scala identifier.")

(defconst scala-var-ident-re
  (concat "[[:lower:]][[:alnum:]]*" "\\(_" scala-ident-re "\\)*")
  "Relgular expression matching a Scala 'variable' identifier.")

(defconst scala-qual-ident-re
  (concat scala-ident-re "\\(" "\\." scala-ident-re "\\)*"))

(defconst scala-capitalized-ident-re
  (concat "\\(\\)\\([[:upper:]]" scala-ident-re "\\)"))

(defconst scala-expr-start-re
  (concat
   (regexp-opt '("if" "else" "for" "do" "yield") 'words) "\\|"
   (regexp-opt '("=" "=>") t)))

(defconst scala-expr-starter
  (mapcar (lambda (pair) (cons (car pair) (concat "\\<" (cdr pair) "\\>")))
          '(("else" . "if")
            ("yield" . "for")
            ("do" . "for")
            ("extends" . "class")
            ("with" . "class")
            ("=>" . "case"))))

(defconst scala-expr-middle-re
  (regexp-opt (mapcar #'car scala-expr-starter) 'words))

(defconst scala-compound-expr-re
  "\\<else\\s +if\\>")

(defconst scala-comment-begin-or-end-re
  (concat "\\(" "^/\\*.*" "\\|" "^//.*" "\\|" ".*\\*/$" "\\)"))

