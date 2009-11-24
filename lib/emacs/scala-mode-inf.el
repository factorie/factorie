;;; -*-Emacs-Lisp-*-
;;; scala-mode-inf.el - Interaction with a Scala interpreter.

;; Copyright (C) 2009 Scala Dev Team at EPFL
;; Authors: See AUTHORS file
;; Keywords: scala languages oop
;; $Id: scala-mode-inf.el 17069 2009-02-10 08:30:51Z nielsen $

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

(provide 'scala-mode-inf)


(require 'comint)

(defgroup scala-mode-inf
  nil
  "Mode to interact with a Scala interpreter."
  :group 'scala
  :tag "Inferior Scala")

(defcustom scala-interpreter "scala"
  "The interpreter that `run-scala' should run. This should
 be a program in your PATH or the full pathname of the scala interpreter."
  :type 'string
  :group 'scala-mode-inf)

(defconst scala-inf-buffer-name "*inferior-scala*")

(define-derived-mode scala-mode-inf comint-mode "Inferior Scala"
  "Major mode for interacting with a Scala interpreter.

\\{inferior-scala-mode-map\\}"
  (define-key scala-mode-inf-map [(meta return)] 'comint-accumulate)

  ;; Comint configuration
  (make-local-variable 'comint-input-sender)
  (setq comint-input-sender 'scala-input-sender))

(defun scala-input-sender (proc string)
  (comint-send-string proc string)
  ;; (comint-send-string proc "\nemacs:end\n")) ;; Heineman's contrib (06/03/2007)
  (comint-send-string proc "\n"))

;;;###autoload
(defun scala-interpreter-running-p-1 ()
  ;; True iff a Scala interpreter is currently running in a buffer.
  (comint-check-proc scala-inf-buffer-name))

(defun scala-check-interpreter-running ()
  (unless (scala-interpreter-running-p-1)
    (error "Scala interpreter not running")))

;;;###autoload
(defun scala-run-scala (cmd-line)
  "Run a Scala interpreter in an Emacs buffer"
  (interactive (list (if current-prefix-arg
			 (read-string "Scala interpreter: " scala-interpreter)
                       scala-interpreter)))
  (unless (scala-interpreter-running-p-1)
    (setq scala-interpreter cmd-line)
    (let ((cmd/args (split-string cmd-line)))
      (set-buffer
       (apply 'make-comint "inferior-scala" (car cmd/args) nil (cdr cmd/args))))
    (scala-mode-inf)
    (pop-to-buffer scala-inf-buffer-name)))

(defun scala-send-string (str &rest args)
  ;; Send string to interpreter
  (comint-send-string scala-inf-buffer-name (apply 'format str args))
  ;; (comint-send-string scala-inf-buffer-name "\nemacs:end\n")) Heineman's contrib (06/03/2007)
  (comint-send-string scala-inf-buffer-name "\n"))

;;;###autoload
(defun scala-switch-to-interpreter ()
  "Switch to buffer containing the interpreter"
  (interactive)
  (scala-check-interpreter-running)
  (switch-to-buffer scala-inf-buffer-name))

(defvar scala-tmp-file nil)

;;;###autoload
(defun scala-eval-region (start end)
  "Send current region to Scala interpreter."
  (interactive "r")
  (scala-check-interpreter-running)
  (comint-send-region scala-inf-buffer-name start end)
  (comint-send-string scala-inf-buffer-name "\n"))

;;;###autoload
(defun scala-eval-definition ()
  "Send the current 'definition' to the Scala interpreter.
This function's idea of a definition is the block of text ending
in the current line (or the first non-empty line going
backwards), and begins in the first line that is not empty and
does not start with whitespace or '{'.

For example:

println( \"aja\")
println( \"hola\" )

if the cursor is somewhere in the second print statement, the
interpreter should output 'hola'.

In the following case, if the cursor is in the second line, then
the complete function definition will be send to the interpreter:

def foo =
  1 + 2
"
  (interactive)
  (save-excursion
    ;; find the first non-empty line
    (beginning-of-line)
    (while (and (not (= (point) (point-min)))
                (looking-at "\\s-*$"))
      (next-line -1))
    (end-of-line)
    (let ((end (point)))
      ;; now we need to find the start
      (beginning-of-line)
      (while (and (not (= (point) (point-min)))
                  (looking-at (mapconcat '(lambda (x) x)
                                         '("^$"       ; empty lines
                                           "^\\s-+"   ; empty lines or lines that start with whitespace
                                           "^\\s-*}") ; lines that start with a '}'
                                         "\\|")))
        (next-line -1)
        (beginning-of-line))
      (message "region %s %s" (point) end)
      (scala-eval-region (point) end))))

;;;###autoload
(defun scala-eval-buffer ()
  "Send whole buffer to Scala interpreter."
  (interactive)
  (scala-eval-region (point-min) (point-max)))

(defvar scala-prev-l/c-dir/file nil
  "Caches the last (directory . file) pair.
Caches the last pair used in the last scala-load-file.
Used for determining the default in the next one.")

;;;###autoload
(defun scala-load-file (file-name)
  "Load a file in the Scala interpreter."
  (interactive (comint-get-source "Load Scala file: " scala-prev-l/c-dir/file
				  '(scala-mode) t))
  (scala-check-interpreter-running)
  (comint-check-source file-name)
  (setq scala-prev-l/c-dir/file (cons (file-name-directory file-name)
                                      (file-name-nondirectory file-name)))
  (scala-send-string ":load %s" file-name))

;;;###autoload
(defun scala-quit-interpreter ()
  "Quit Scala interpreter."
  (interactive)
  (scala-check-interpreter-running)
  (scala-send-string "\n:quit"))

