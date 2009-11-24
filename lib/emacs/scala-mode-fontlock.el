;;; -*-Emacs-Lisp-*-
;;; scala-mode-fontlock.el - 

;; Copyright (C) 2009 Scala Dev Team at EPFL
;; Authors: See AUTHORS file
;; Keywords: scala languages oop
;; $Id: scala-mode-fontlock.el 17069 2009-02-10 08:30:51Z nielsen $

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

(provide 'scala-mode-fontlock)

(require 'cl)
(require 'font-lock)
(require 'scala-mode-constants)
(require 'scala-mode-lib)
(require 'scala-mode-navigation)


(defun scala-mark-borders (funs)
  (loop for (fun . flag) in funs
        if flag collect (point-marker)
        while (funcall fun)
        if flag collect (point-marker)))

(defun scala-make-match (funs)
  (let ((start-mark (point-marker))
        (markers (scala-mark-borders funs))
        (end-mark (point-marker)))
    (cons start-mark (cons end-mark markers))))

(defconst scala-binding-end-re
  (regexp-opt '(":" "=" "=>" ";" "<-")))

(defun scala-match-and-skip-binding (limit)
  (skip-chars-forward " ()")
  (and (not (or (looking-at "\\<\\(extends\\|with\\)\\>\\|{")
                (scala-looking-at-special-identifier scala-binding-end-re)))
       (ignore-errors
         (save-restriction
           (narrow-to-region (point-min) limit)
           (let ((matches (scala-make-match
                           '((scala-forward-ident . t)
                             ((lambda ()
                                (scala-forward-spaces)
                                (when (scala-looking-at-special-identifier ":")
                                  (forward-char)
                                  (scala-forward-spaces)
                                  t)) . nil)
                             ((lambda ()
                                (scala-forward-type)
                                (scala-when-looking-at "\\s *\\*")
                                t) . t)))))
             (scala-when-looking-at "\\s *,")
             (set-match-data matches)))
         t)))

(defun scala-match-and-skip-ident (limit)
  (scala-forward-spaces)
  (when (and (not (looking-at scala-keywords-re))
             (looking-at scala-qual-ident-re))
    (goto-char (match-end 0))
    t))

(defun scala-match-and-skip-type-param (limit)
  (scala-when-looking-at "\\s *[[,]\\s *"
    (let ((matches (scala-make-match '((scala-forward-type-param . t)))))
      (scala-when-looking-at "\\s *\\]")
      (set-match-data matches)
      t)))

(defun scala-match-and-skip-result-type (limit)
  (scala-when-looking-at "\\s *:\\s *"
    (set-match-data (list (point-marker)
                          (progn (scala-forward-type) (point-marker))))
    t))

(defconst scala-pattern-end-re
  (regexp-opt '("if" "case" "class") 'words))

(defconst scala-pattern-end-special-re
  (regexp-opt '( "=>" "=" "<-") t))

(defun scala-match-and-skip-pattern (limit)
  (while (progn
           (skip-chars-forward "()[], ")
           (and (not (or (looking-at scala-pattern-end-re)
                         (scala-looking-at-special-identifier
                          scala-pattern-end-special-re)))
                (looking-at scala-literal-re)))
    (goto-char (match-end 0)))
  (and (not (or (looking-at scala-pattern-end-re)
                (scala-looking-at-special-identifier scala-pattern-end-special-re)))
       (let ((case-fold-search nil))
         (cond ((looking-at scala-capitalized-ident-re)
                (goto-char (match-end 0)))
               ((scala-match-and-skip-binding limit) t)))))


(defvar scala-font-lock-keywords
  `(;; keywords
    (,scala-keywords-re 0 font-lock-keyword-face nil)

    ;; constants
    (,scala-constants-re
     0 ,(if (boundp 'font-lock-constant-face)
	    'font-lock-constant-face
	  'font-lock-keyword-face)
     nil)
     
    ;; modules
    (,(concat "\\<\\(module\\|object\\)\\>\\s *\\(" scala-ident-re "\\)")
     (2 font-lock-variable-name-face nil))
     
    ;; type definitions
    (,(concat "\\<type\\>\\s *\\(" scala-ident-re "\\)")
     (1 font-lock-type-face nil))
     
    ;; variables
    ("\\<var\\>"
     (scala-match-and-skip-binding (goto-char (match-end 0))
				   nil
				   (1 font-lock-variable-name-face nil)
				   (2 font-lock-type-face nil t)))
     
    ;; functions
    (,(concat "\\(^\\|[^(,]\\)\\s *\\<def\\>" 
	      "\\s *" 
	      "\\(" 
	      scala-ident-re 
	      "\\)\\s *")
     (2 font-lock-function-name-face nil)
     (scala-match-and-skip-type-param (goto-char (match-end 0)) nil
				      (1 font-lock-type-face nil t))
     (scala-match-and-skip-binding nil nil
				   (1 font-lock-variable-name-face nil)
				   (2 font-lock-type-face nil t))
     (scala-match-and-skip-result-type nil nil
				       (0 font-lock-type-face nil)))
     
    ;; class definitions
    ("\\<\\(class\\|trait\\)\\>"
     (scala-match-and-skip-ident (goto-char (match-end 0)) nil
				 (1 font-lock-type-face nil))
     (scala-match-and-skip-type-param nil nil
				      (1 font-lock-type-face nil t))
     (scala-match-and-skip-binding nil nil
				   (1 font-lock-variable-name-face nil)
				   (2 font-lock-type-face nil t)))
     
    ;; "extends" and "with" clauses
    ("\\<\\(extends\\|with\\)\\>\\s *[^{]"
     (scala-match-and-skip-ident (goto-char (1- (match-end 0))) nil
				 (0 font-lock-type-face nil))
     (scala-match-and-skip-type-param nil nil
				      (1 font-lock-type-face nil t)))
     
    ;; patterns
    ("\\<\\(case\\|val\\)\\>\\s *"
     (scala-match-and-skip-pattern (goto-char (match-end 0)) nil
				   (1 font-lock-variable-name-face nil)
				   (2 font-lock-type-face nil t)))
    ))


(defvar scala-font-lock-syntactic-keywords
  `((,scala-char-re (0 "\"" t nil))
    (scala-search-special-identifier-forward (0 "w" nil nil))))



