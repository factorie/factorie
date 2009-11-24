;;; -*-Emacs-Lisp-*-
;;; scala-mode-lib.el - Libraries and macroes used by the scala mode.

;; Copyright (C) 2009 Scala Dev Team at EPFL
;; Authors: See AUTHORS file
;; Keywords: scala languages oop
;; $Id: scala-mode-lib.el 17069 2009-02-10 08:30:51Z nielsen $

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

(provide 'scala-mode-lib)

(eval-when-compile
  (require 'scala-mode-constants))


(defmacro scala-mode-lib:define-keys (key-map &rest key-funcs)
  "Define key bindings for KEY-MAP (create KEY-MAP, if it does
not exist."
  `(progn
     (unless (boundp ',key-map)
       (setf ,key-map (make-keymap)))
     ,@(mapcar
  #'(lambda (key-func)
      `(define-key ,key-map ,(first key-func) ,(second key-func)))
  key-funcs)))


(defun scala-special-char-p (char)
  (and char
       (string-match scala-all-special-char-re (string char))))

(defun scala-looking-at-special-identifier (regexp)
  (and (not (scala-special-char-p (char-before)))
       (looking-at regexp)
       (not (scala-special-char-p (char-after (match-end 0))))))


(defun scala-search-special-identifier-forward (limit)
  (ignore-errors
    (while (and (search-forward-regexp scala-special-ident-re limit)
                (save-match-data
                  (string-match scala-comment-begin-or-end-re
                                (match-string-no-properties 0)))))
    t))


(defun scala-mode-find-clstrtobj-name-doc ()
  (save-excursion
    (if (re-search-forward "\\(class\\|object\\|trait\\)[ \t\n]+\\([a-zA-Z0-9_:=]+\\)[ \t\n]*" nil t)
	
      (buffer-substring (match-beginning 2) (match-end 2))
      "NONAME")))


(defun scala-mode-def-and-args-doc ()
  (save-excursion
    (if (re-search-forward
	 (concat
	  ;; function name
	  "[ \t\n]*def[ \t\n]+\\([a-zA-Z0-9_:=]+\\)[ \t\n]*"
 
	  ;; arguments
	  "\\((\\([a-zA-Z0-9_:* \t\n]*\\))\\)?"
	  ) nil t)

	;; TODO: output args in a sane format to use in yasnippet, look at doxymancs line 1441 
	(let* ((func (buffer-substring (match-beginning 1) (match-end 1)))
	       ;(args (buffer-substring (match-beginning 3) (match-end 3)))
	       )
	  (concat "${1:" func "} $0"))
      "${1:name} $0")))


(defun scala-mode-file-doc ()
  (file-name-nondirectory buffer-file-name))
