;;; -*-Emacs-Lisp-*-
;;; scala-mode-auto.el - Autoloads file for the scala mode

;; Copyright (C) 2009 Scala Dev Team at EPFL
;; Authors: See AUTHORS file
;; Keywords: scala languages oop
;; $Id: scala-mode-auto.el 17069 2009-02-10 08:30:51Z nielsen $

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

;; We now depend on font-locking features only in emacs 21.x and newer
(unless (<= 21 emacs-major-version)
  (error
   (format "The Scala mode require Emacs version 21.x (and not your Emacs version %s.%s)"  emacs-major-version  emacs-minor-version)))

;; TODO insert check for correct version of speedbar


;; Attach .scala files to the scala-mode
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))
(modify-coding-system-alist 'file "\\.scala$"     'utf-8)


;; Autoload from scala-mode.el
(autoload (quote scala-mode) "scala-mode" "\
Major mode for editing Scala code.

When started, run `scala-mode-hook'.

\\{scala-mode-map}" t nil)


;; Autoload from scala-mode-inf.el
(autoload (quote scala-interpreter-running-p-1) "scala-mode-inf" nil t nil)

(autoload (quote scala-run-scala) "scala-mode-inf" "Run a Scala interpreter in an Emacs buffer" t nil)

(autoload (quote scala-switch-to-interpreter) "scala-mode-inf" "Switch to buffer containing the interpreter" t nil)

(autoload (quote scala-eval-region) "scala-mode-inf" "Send current region to Scala interpreter." t nil)

(autoload (quote scala-eval-buffer) "scala-mode-inf" "Send whole buffer to Scala interpreter." t nil)

(autoload (quote scala-load-file) "scala-mode-inf" "Load a file in the Scala interpreter." t nil)

(autoload (quote scala-quit-interpreter) "scala-mode-inf" "Quit Scala interpreter." t nil)


(provide 'scala-mode-auto)
