;;; org-iap-mode.el --- interactive org presentations with srcblock execution

;; Copyright (C) 2013 Derek Feichtinger
 
;; Author: Derek Feichtinger <derek.feichtinger@psi.ch>
;; Keywords: org
;; Homepage:
;; Package-Requires: ((org "7")) 
;; Version:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;; Commentary:
;; TODO
;;


(defvar org-iap-frame-name "iap-presentation-frame"
  "the frame's name in which the presentation is running")

(defvar org-iap-src-frame nil
  "the control frame from which the presentation is running")

(defvar org-iap-title-buffer "*iap-title*"
  "the name of the buffer for displaying iap title and comments")

(defvar org-iap-codebuffer nil
  "structure for buffering code to be executed by org-iap in the
interactive target buffers")

(defun org-iap-frame ()
  "returns frame object for the presentation"
  (find-if
   (lambda (f) (equal  (frame-parameter f 'name) org-iap-frame-name))
   (frame-list)))

(defun org-iap-activate-frame (&optional bufname)
  "sets input focus to the presentation frame and displays the title
buffer. If BUFNAME is supplied, a window for this second buffer is
created below the title window."
  (unless (org-iap-frame) (make-frame `((name . ,org-iap-frame-name))))
  (raise-frame (org-iap-frame))
  (select-frame-set-input-focus (org-iap-frame))
  (if bufname (progn 
		  (switch-to-buffer bufname)
		  (delete-other-windows)
		  (let ((title-win (selected-window)) (main-win (split-window nil 5)))    
		    (select-window title-win)
		    (switch-to-buffer org-iap-title-buffer)
		    (org-fit-window-to-buffer title-win)
		    (select-window main-win)
		    ))
    (progn
      (switch-to-buffer org-iap-title-buffer)
      (delete-other-windows))))

(defun org-iap-cb-push (bufname codeblock)
  "pushes the command lines in CODEBLOCK into org-iap-codebuffer, so that
they are associated with the buffer name BUFNAME"
  (if (assoc bufname org-iap-codebuffer)
      (setf (cdr (assoc bufname org-iap-codebuffer))
	    (split-string codeblock "\n"))
    (setq org-iap-codebuffer
	  (cons (concatenate 'list `(,bufname) (split-string codeblock "\n"))
		org-iap-codebuffer))))

(defun org-iap-ansiterm (name &optional codestr initstr)
  "opens an emacs ansi-term with NAME as buffer name. If CODESTR
is given, the code lines will be exectuted in the ansi-term.  If
INITSTR is given, and the named ansi-term does not yet exist, the
code in the initstr will be executed in the ansi-term, and then
the ansi-term will be cleared."
  (let ((bufname (concat "*" name "*")))
    (unless (get-buffer bufname)
      (ansi-term "/bin/bash" name)
      (process-send-string bufname (concat initstr "\nclear\n")))
    (org-iap-cb-push bufname codestr)   
    (org-iap-activate-frame bufname)
    (term-show-maximum-output)
    (org-iap-execute)))

(defun org-iap-execute ()
  "execute commands from `org-iap-codebuffer' if it contains commands for the
current buffer"
  (interactive)
  (let ((bufname (buffer-name)) c nectcmd)
    (while (and (not (memq c '(?b ?e ?q)))
		(setq nextcmd (cadr (assoc bufname org-iap-codebuffer))))
      (if (not (equal c ?c))
	  (setq c (read-char-exclusive
		   (concat "next statement: " nextcmd))))
      (cond
       ((memq c '(?n ?\C-m ?c))
	(process-send-string bufname 
			     (concat 
			      (pop (cdr (assoc bufname org-iap-codebuffer)))
			      "\n")))
       ((memq c '(?e ?q)) (org-iap-cb-push bufname ""))
       ((equal c ?b))
       ((equal c ?s) (pop (cdr (assoc bufname org-iap-codebuffer))))))
    (unless (equal c ?b)
      (and (framep org-iap-src-frame)
	   (select-frame-set-input-focus org-iap-src-frame)))
    ))

(defun org-iap-display-title (&optional start end)
  "puts title and text content of the org item at point into the
`org-iap-title-buffer'"
  (get-buffer-create org-iap-title-buffer)
  (let ((hstruct (org-heading-components))
	;; (content (when (and start end) (buffer-substring-no-properties
	;; 			      start end))))
	(content (when (and start end) (buffer-substring
				      start end))))
    (with-current-buffer org-iap-title-buffer
		      (erase-buffer)
		      (insert (propertize (nth 4 hstruct) 'face 'org-level-1))
		      (when (and content (> end start))
			(insert (concat "\n" content))))))

(defun org-iap-display-slide ()
  "displays org item at point as a slide"
  (interactive)
  (setq org-iap-src-frame (window-frame nil))
  (let* ((item-start (progn (beginning-of-line)
				 (unless (looking-at "^\\*+")
				   (re-search-backward "^\\*+"))
				 (point)))
	 (item-end (progn (end-of-line)
			     (or (when (re-search-forward "^\\*+" nil t)
				   (beginning-of-line)
				   (backward-char)
				   (point))
				 (buffer-end 1))))
	    (text-start (progn (goto-char item-start)
			       (forward-line)
			       (point))))
    (goto-char item-start)
    (cond ((re-search-forward
	    "\\[\\[\\(file:\\)?\\(.*\\.\\)\\(jpg\\|gif\\|png\\)" item-end t)
	   ;; if there is a picture produced by a src bloc, we only
	   ;; use everything up to the src block for the title
	   (let ((text-end (save-excursion
			     (min (progn (beginning-of-line)
					 (backward-char)
					 (point))
				  (save-match-data (goto-char item-start)
						   (org-next-block 1)
						   (point)))))
		 (fname (concat
			 (buffer-substring-no-properties (match-beginning 2)
							 (match-end 2))
			 (buffer-substring-no-properties (match-beginning 3)
							 (match-end 3)))))
	     (org-iap-display-title text-start text-end)
	     (org-iap-display-img fname)))
	  ((progn (goto-char item-start)
		  (re-search-forward "^ *#\\+BEGIN_SRC" item-end t))
	   (let ((text-end (save-excursion (beginning-of-line)
					   (backward-char)
					   (point))))
	     (org-iap-display-title text-start text-end))
	   (org-iap-srcblock-to-session))
	  (t (org-iap-display-title text-start item-end)
	     (org-iap-activate-frame)
	     (and (framep org-iap-src-frame)
		  (select-frame-set-input-focus org-iap-src-frame)))
	  )
    )
  )

(defun org-iap-next-slide ()
  (interactive)
  (end-of-line)
  (when (re-search-forward "^\\*+" nil t)
    (org-iap-display-slide))
  )

(defun org-iap-srcblock-to-session ()
  "start an appropriate interactive shell for the code in the org
  src block at point and execute each line in the shell"
  (interactive)
  (let* ((oinf (org-babel-get-src-block-info))
	 (lang (car oinf))
	 (meta (nth 2 oinf))
	 (session (cdr (assoc :session meta)))
	 (code (nth 1 oinf)))
    (if (string= session "none")
	(error "Src block is lacking session keyword")
      (cond ((string= lang "emacs-lisp")
	     (message "shell for %s not yet available" lang))
	    ((string= lang "sh")
	     (org-iap-ansiterm session code "export PS1='\\[\\033[0;32m\\]${debian_chroot:+($debian_chroot)}\\u@\\h:\\w\\$ \\[\\033[0;30m\\]'"))
	    (t (message "no shell for %s" lang))))))


(defun org-iap-display-img (fname)
  "open an image in the iap frame"
  (save-excursion
    (save-window-excursion
      (org-iap-activate-frame (find-file fname))
      (eimp-mode 1)
      (eimp-fit-image-to-window nil))))

;; some initial snippets I tool from Nic Ferrier's org-presie 
;; https://github.com/nicferrier/org-presie/blob/master/org-presie.el


;; TODO: sometimes the title window is not fitted perfectly to the
;; title buffer

;; TODO: sometimes the term-show-maximum-output function
;; does not correctly align the buffer end of the ansi term
