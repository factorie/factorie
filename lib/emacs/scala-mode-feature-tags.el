;;; -*-Emacs-Lisp-*-
;;; scala-mode-feature-tags.el - 

;; Copyright (C) 2009 Scala Dev Team at EPFL
;; Authors: See AUTHORS file
;; Keywords: scala languages oop
;; $Id: scala-mode-feature-tags.el 17069 2009-02-10 08:30:51Z nielsen $

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

(provide 'scala-mode-feature-tags)

(require 'etags)

(defgroup scala-mode-feature:tags nil
  "Creating and using TAGS file searches"
  :group 'scala)


(defcustom scala-mode-feature:tags-command "ctags"
  "Tags command for parsing scala code. 
Please see the contrib directory for ctags options for parsing scala files."
  :type 'string
  :group 'scala-mode-feature:tags)


(defcustom scala-mode-feature:tags-option "-e -o %s -R %s"
  "Options for the ctags command"
  :type 'string
  :group 'scala-mode-feature:tags)


(defcustom scala-mode-feature:tags-ask-when-reload nil
  "Indicates whether the user should confirm reload a TAGS table or not."
  :type 'boolean
  :group 'scala-mode-feature:tags)

(defvar scala-mode-feature-tags-completion-table nil
  "")

(defvar scala-mode-feature-tags-tag-file nil
  "")

(defun scala-mode-feature-tags-create (dir-name)
  "Create TAGS file"
  (interactive "DTAGS file directory: ")
  (message "Creating TAGS, please wait...")
  (let* 
      ((tags-file-name (concat dir-name "/TAGS"))
       (args (format scala-mode-feature:tags-option tags-file-name dir-name)))
    (shell-command
     (concat scala-mode-feature:tags-command " " args))
    (flet ((yes-or-no-p (p) (if scala-mode-feature:tags-ask-when-reload
				(y-or-n-p p)
			      t)))
      (visit-tags-table tags-file-name))
    (setq scala-mode-feature-tags-tag-file tags-file-name)))


(defun scala-mode-feature-tags-load (file-name)
  "Load TAGS file"
  (interactive "fTAGS file: ")
  (if (and (file-exists-p file-name) (file-readable-p file-name))
      (progn 
	(visit-tags-table file-name)
	(setq scala-mode-feature-tags-tag-file file-name))
    (message "The TAGS file does not exist!")))


(defun scala-mode-feature-tags-complete ()
  "Perform completion on the text around point.
Completes to the set of names listed in the current tags table.
The string to complete is chosen in the same way as the default
for \\[find-tag] (which see)."
  (interactive)
  (let ((pattern (scala-mode-feature-tags-get-pattern))
        beg
        completion
        (scala-comp scala-mode-feature-tags-completion-table))
    (if (not pattern) (message "Nothing to complete")
        (search-backward pattern)
        (setq beg (point))
        (forward-char (length pattern))
        (setq completion (try-completion pattern scala-comp nil))
        (cond 
	 ((eq completion t))
	 ((null completion)
	  (message "Can't find completion for \"%s\"" pattern)
	  (ding))
	 ((not (string= pattern completion))
	  (delete-region beg (point))
	  (insert completion))
	 (t
	  (message "Making completion list...")
	  (with-output-to-temp-buffer "*Completions*"
	    (display-completion-list
	     (all-completions pattern scala-comp)))
	  (message "Making completion list...%s" "done"))))))


(defun scala-mode-feature-tags-completion-table ()
    (or (and scala-mode-feature-tags-tag-file
	     scala-mode-feature-tags-completion-table)
      (let ((tags-table
             (if (and scala-mode-feature-tags-tag-file
                      (functionp 'etags-tags-completion-table))
                 (with-current-buffer (get-file-buffer scala-mode-feature-tags-tag-file)
                   (etags-tags-completion-table))
               nil)))
        (unless tags-table
          (error "No TAGS file active!"))
        (setq scala-mode-feature-tags-completion-table tags-table))))


(defun scala-mode-feature-tags-get-pattern ()
  (save-excursion
    (while (looking-at "\\sw\\|\\s_")
      (forward-char 1))
    (if (or (re-search-backward "\\sw\\|\\s_"
				(save-excursion (beginning-of-line) (point))
				t)
	    (re-search-forward "\\(\\sw\\|\\s_\\)+"
			       (save-excursion (end-of-line) (point))
			       t))
	(progn (goto-char (match-end 0))
	       (buffer-substring-no-properties
                (point)
                (progn (forward-sexp -1)
                       (while (looking-at "\\s'")
                         (forward-char 1))
                       (point))))
      nil)))

(defun scala-mode-feature-tags-install () 
  
  t)
