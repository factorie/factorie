;;; -*-Emacs-Lisp-*-
;;; scala-mode-navigation.el - 

;; Copyright (C) 2009 Scala Dev Team at EPFL
;; Authors: See AUTHORS file
;; Keywords: scala languages oop
;; $Id: scala-mode-navigation.el 17069 2009-02-10 08:30:51Z nielsen $

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

(provide 'scala-mode-navigation)

(require 'scala-mode-constants)

(defun scala-when-looking-at* (regexp &optional thunk)
  (let ((saved-match-data (match-data)))
    (if (looking-at regexp)
        (progn (goto-char (match-end 0))
               (set-match-data saved-match-data)
               (or (not thunk) (funcall thunk)))
      (set-match-data saved-match-data)
      nil)))

(defmacro scala-when-looking-at (regexp &rest body)
  (if body
      `(scala-when-looking-at* ,regexp (lambda () ,@body))
    `(scala-when-looking-at* ,regexp)))

(defun scala-forward-spaces (&optional limit)
  (if limit
      (save-restriction
        (narrow-to-region (point) limit)
        (forward-comment 100000))
    (forward-comment 100000)))

(defun scala-backward-spaces ()
  (forward-comment -100000))

(defun scala-looking-at-backward (re)
  (save-excursion
    (when (= 0 (skip-syntax-backward "w_")) (backward-char))
    (looking-at re)))

(defmacro scala-point-after (&rest body)
  `(save-excursion
     ,@body
     (point)))


(defmacro scala-move-if (&rest body)
  (let ((pt-sym (make-symbol "point"))
	(res-sym (make-symbol "result")))
    `(let ((,pt-sym (point))
	   (,res-sym ,(cons 'progn body)))
       (unless ,res-sym (goto-char ,pt-sym))
       ,res-sym)))

(defun scala-forward-ident ()
  ;; Move forward over an identifier.
  (scala-forward-spaces)
  (if (looking-at scala-ident-re)
      (goto-char (match-end 0))
    (forward-char))
  t)

(defun scala-backward-ident ()
  ;; Move backward over an identifier.
  (scala-backward-spaces)
  (if (scala-looking-at-backward scala-ident-re)
      (goto-char (match-beginning 0))
    (backward-char))
  t)

(defun scala-forward-qual-ident ()
  ;; Move forward over a qualifier identifier.
  (scala-forward-spaces)
  (if (looking-at scala-qual-ident-re)
      (goto-char (match-end 0))
    (forward-char))
  t)

(defun scala-backward-qual-ident ()
  ;; Move backward over a qualifier identifier.
  (scala-backward-spaces)
  (if (scala-looking-at-backward scala-qual-ident-re)
      (goto-char (match-beginning 0))
    (backward-char))
  t)

(defun scala-forward-simple-type ()
  ;; Move forward over a simple type (as defined by the grammar).
  ;; Works only when point is at the beginning of a simple type
  ;; (modulo initial spaces/comments).
  (cond ((eobp) nil)
        ((= (char-after) ?\()
         ;; Parenthesized type
         (forward-sexp)
         t)
        (t
         ;; Type designator
         (scala-forward-qual-ident)
         (scala-forward-spaces)
         (cond ((eobp) nil)
               ((= (char-after) ?\[)
                ;; Type arguments
                (forward-sexp))
               ((= (char-after) ?\#)
                ;; Type selection
                (forward-char)
                (scala-forward-ident)))
         t)))

(defun scala-forward-type1 ()
  ;; Move forward over a type1 (as defined by the grammar).
  ;; Works only when point is at the beginning of a type (modulo
  ;; initial spaces/comments).
  (scala-forward-spaces)
  (scala-when-looking-at "\\<class\\>"
                         (forward-word 1) (scala-forward-spaces))
  (scala-forward-simple-type)
  (while (scala-when-looking-at "\\s *\\<with\\>\\s *")
    (if (and (not (eobp)) (= (char-after) ?\{))
        (forward-sexp)                       ;skip refinement
      (scala-forward-simple-type)))
  t)

(defun scala-forward-type ()
  ;; Move forward over a type.
  (cond ((eobp) nil)
        ((= (char-after) ?\()
         ;; Function type (several arguments)
         (forward-sexp)
         (scala-when-looking-at "\\s *=>\\s *" (scala-forward-type))
         t)
        (t
         ;; Type1 or function type with one argument
         (scala-forward-type1)
         (scala-when-looking-at "\\s *=>\\s *" (scala-forward-type))
         t)))

(defun scala-forward-type-param ()
  ;; Move over a type parameter
  ;; variance
  (scala-when-looking-at "\\s *[-+]\\s *")
  (scala-forward-ident)
  ;; bounds
  (while (scala-when-looking-at "\\s *[<>][:%]\\s *")
    (scala-forward-type))
  t)

(defun scala-forward-literal ()
  ;; Move forward over an integer, float, character or string literal.
  (scala-forward-spaces)
  (scala-when-looking-at scala-literal-re)
  t)
