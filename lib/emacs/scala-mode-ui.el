;;; -*-Emacs-Lisp-*-
;;; scala-mode-ui.el - Menu entries and keyboard shortcuts for scala mode

;; Copyright (C) 2009 Scala Dev Team at EPFL
;; Authors: See AUTHORS file
;; Keywords: scala languages oop
;; $Id: scala-mode-ui.el 17070 2009-02-10 08:51:50Z nielsen $

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

(provide 'scala-mode-ui)

(require 'easymenu)
(require 'scala-mode-lib)

(eval-when-compile
  (require 'scala-mode-inf))

(eval-and-compile
  (defcustom scala-mode-ui:prefix-key "\C-c"
    "Key prefix for scala mode."
    :group 'scala))

(defmacro scala-mode-ui:key (key)
  "Simple macro for appending 'scala-mode-prefix-key' to key commands"
  `(kbd ,(concat scala-mode-ui:prefix-key " " key)))

;;; Helper functions

(defun scala-mode-ui:interpreter-running-p ()
  "True iff a Scala interpreter is currently running in a buffer."
  ;; The following makes sure that we do not autoload
  ;; scala-mode-inf just to check if the interpreter is running.
  (and (fboundp 'scala-mode-inf)
       (let ((ism-def (symbol-function 'scala-mode-inf)))
         (not (and (consp ism-def) (eq (car ism-def) 'autoload))))
       (scala-interpreter-running-p-1)))

;;; Menubar

(scala-mode-lib:define-keys scala-mode-menu-bar-map

  ([scala] (cons "Scala" (make-sparse-keymap "ScalaMode")))

  ([scala version]        '(menu-item "Version"              (lambda () (interactive) (message "Using scala mode version %s (%s)" scala-mode-version scala-mode-svn-revision)) ))
  ([scala report-bug]     '(menu-item "Report bug"           scala-mode:report-bug))
  ([scala customize]      '(menu-item "Customize"            (lambda () (interactive) (customize-group 'scala))))
  ([scala browse-api]     '(menu-item "Browse Scala API"     scala-mode:browse-api))
  ([scala browse-website] '(menu-item "Browse Scala Website" scala-mode:browse-web-site))

  ([scala sep0]           '("---"))

  ([scala feature] (cons "Features" (make-sparse-keymap "Features")))

  ([scala feature apropos]  '(menu-item "Tag apropos"		        tags-apropos))
  ([scala feature search]   '(menu-item "Tag search"		        tags-search))
  ([scala feature find]     '(menu-item "Tag find"		        find-tag))
  ([scala feature comp]	    '(menu-item "Tag complete word"	        scala-mode-feature-tags-complete))
  ([scala feature load]	    '(menu-item "Load TAGS file"		scala-mode-feature-tags-load))
  ([scala feature create]   '(menu-item "Create TAGS file"		scala-mode-feature-tags-create))

  ([scala feature sep1]     '("---"))

  ([scala feature speedbar] '(menu-item "Speedbar Focus"		speedbar-get-focus))

  ([scala feature sep0]     '("---"))

  ([scala feature electric] '(menu-item "Toggle Scala Electric Mode" scala-electric-mode
					:button (:toggle . (scala-mode-feature-electric-active-p))
					:help "Toggle on/off the electric insert mode for Scala files"))

  ([scala sep1]           '("---"))

  ([scala eval-buf]       '(menu-item "Evaluate buffer"          scala-eval-buffer           :enable (scala-mode-ui:interpreter-running-p)                  ))
  ([scala eval-reg]       '(menu-item "Evaluate region"          scala-eval-region           :enable (and (scala-mode-ui:interpreter-running-p) mark-active)))
  ([scala switch-interp]  '(menu-item "Switch to interpreter"    scala-switch-to-interpreter :enable (scala-mode-ui:interpreter-running-p)                  ))
  ([scala load-file]      '(menu-item "Load file in interpreter" scala-load-file             :enable (scala-mode-ui:interpreter-running-p)                  ))
  ([scala quit-interp]    '(menu-item "Quit interpreter"         scala-quit-interpreter      :enable (scala-mode-ui:interpreter-running-p)                  ))
  ([scala run-interp]     '(menu-item "Run interpreter..."       scala-run-scala             :enable (not (scala-mode-ui:interpreter-running-p))            ))

)


;;; Shortcuts

(defvar scala-mode-map
  (let ((map (make-keymap)))
    map))

(scala-mode-lib:define-keys scala-mode-map

   ;; Attach Menubar
   ([menu-bar] scala-mode-menu-bar-map)

   ;; Attach keyboard Shortcuts
   ([(control tab)]            'scala-undent-line)
   ([backspace]                'backward-delete-char-untabify)
   		                
   ("\r"                       'scala-newline)

   ([f1]                       'speedbar-get-focus)
			        
   ([(control c)(control l)]   'scala-load-file)
   ([(control c)(control r)]   'scala-eval-region)
   ([(control c)(control b)]   'scala-eval-buffer)
			        
   ([(control c)(control c)]   'comment-region)

   ("}"                        'scala-electric-brace)

   ((scala-mode-ui:key "t n")  'scala-mode-feature-tags-create)
   ((scala-mode-ui:key "t l")  'scala-mode-feature-tags-load)
   ((scala-mode-ui:key "t c")  'scala-mode-feature-tags-complete)
   ((scala-mode-ui:key "t s")  'tags-search)
   ((scala-mode-ui:key "t a")  'tags-apropos)
   )





