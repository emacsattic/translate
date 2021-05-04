;;; translate.el --- use a wordlist to do simple translation

;; Revision 0.1
;; $Id: translate.el,v 1.3 2003/07/27 11:55:35 schauer Exp $

;; This file is not part of Emacs - but should work in any emacs
;; up from 19.28. Developed on XEmacs 19.15, should at least also
;; work with 19.14, probably earlier versions, too. Known not to work
;; in XEmacs 19.11.

;; Copyright (C) 1997,2003 Holger Schauer

;; Author: Holger Schauer <Holger.Schauer@gmx.de>
;; Keywords: translation words

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;; This little utility automizes the access to a _WORDLIST_ which contains
;; for example an english word and its german translation. The approach
;; is very very simple: grep-ing over the specified dictionary and present
;; the results. The specified default dictionary could be found on
;; ftp.uni-ulm.de/pub/misc/dict/english_german.tar.gz.
;; As of 2003, the wordlist is often already part of your friendly
;; Linux distribution. For instance, it is shipped with Debian
;; in the package trans-de-en.

;;; Acknowledgments:
;; Inspired by a small keyboard macro from 
;; Jochem Huhmann <joh@unidui.uni-duisburg.de> and a small routine
;; by Peter Baumgartner <peter@mailhost.uni-koblenz.de>.
;; Three FSF-related bugs fixed thanks to Roland Rosenfeld and Juergen Holm.

;;; Usage:
;; Besides several simple lookup routines, addition of new translations
;; is possible. These will be stored in privat dictionaries.
;; The use of several private dictionaries is encouraged, one for each
;; source-lang : target-lang pair. For example if you add a new translation
;; for a german word, say for 'car' for 'Auto', use 'german-eng.vok' instead
;; of 'english-german.vok'.

;;; Installation:
;; add the following lines to your .emacs:
;;(autoload 'translate "translate" "A simple translator." t)
;;(autoload 'translate-at-point "translate" "A simple translator." t)
;;(autoload 'translate-phrase-from-region "translate" "A translator." t)
;;(autoload 'translate-change-private-dictionary "translate" "A translator" t)
;;(autoload 'translate-change-global-dictionary "translate" "A translator" t)
;;(autoload 'translate-enter-translation "translate" "A translator" t)
;; If you like to have some of the functions bound to keys use the 
;; following commands or a variation.
;;(global-set-key '[(control c) t] 'translate)
;;(global-set-key '[(control c) T] 'translate-at-point)
;;(global-set-key '[(control c) E] 'translate-enter-translation)
;; Note that these keybindings may not work on FSF Emacs prior
;; to version 19.34.

;;; Customization:
;; Adjust *translate-glob-dir* and *translate-glob-dictname*
;; to your personal needs, e.g. I use:
;;   (setq *translate-glob-dir* "/home/schauer/misc/lib/dict/")
;; The default privat directory is "~/.translate". This is also
;; used by the shell script provided by <joh@unidui.uni-duisburg.de>.
;; If you never want to add new translations 
;;   (setq *translate-ask-user* nil)
;; If you want to use just one personal dictionary for all additions
;; you want to make 
;;   (setq *translate-ask-dicts* nil)
;;   (setq *translate-single-privdict* "~/.translate/privdict.vok") 
;; By default (see *translate-dict-regexp*), all names of 
;; dictionary files must end in .vok. If you like you might use another
;; version of grep etc., go ahead and see what's there.

;;; TODO:
;; preprocess phrase to handle all kinds of umlauts
;; enable use of possibly compressed dictionaries
;; mouse/menu interface
;; interface to the LEO-Online-Dictionary at 
;; http://www.leo.org/cgi-bin/dict-search via browse-url.

(defvar *translate-glob-dir* "/usr/share/trans/"
 "Where the dictionaries for translation are stored.")
(defvar *translate-glob-dictname* "de-en"
 "The default dictionary to use for translation.")
(defvar *translate-globdict* 
  (concat *translate-glob-dir* *translate-glob-dictname*)
 "Set this variable via translate-change-global-dictionary.")
(defvar *translate-dict-regexp* "\\.vok+"
 "Regular expressions which identify dictionaries.")
(defvar *translate-priv-dir* "~/.translate/"
  "Where the personal dictionaries are stored.")
(defvar *translate-ask-user* t
 "Whether to ask the user to enter a translation if none could be found.")
(defvar *translate-ask-dicts* t
 "Whether to ask the user which personal dictionary to use if a new
  translation is inserted.")
(defvar *translate-single-privdict* "~/.translate/privdict.vok"
 "For use when you want to use a singleton private dictionary.")

(defvar *translate-program* "egrep"
 "The utility used for finding translations.")
(defvar *translate-program-extra-args* " -ih \"(^|	| )"
 "The arguments that should be handed to the utility specified in 
 *translate-program*.

 This does not include the word/phrase to look for.")
(defvar *translate-separator* " :: "
 "Defines how a phrase and its translation are separated.")

;;; Some compatibility stuff first

;; sigh, Emacs usually doesn't load cl, but XEmacs does ...
(require 'cl)

; stolen from XEmacs 19.15 syntax.el
(if (not (fboundp 'symbol-near-point))
    (defun symbol-near-point ()
      "Return the first textual item to the nearest point."
      (interactive)
	;alg stolen from etag.el
      (save-excursion
	(if (not (memq (char-syntax (preceding-char)) '(?w ?_)))
	    (while (not (looking-at "\\sw\\|\\s_\\|\\'"))
	      (forward-char 1)))
	(while (looking-at "\\sw\\|\\s_")
	  (forward-char 1))
	(if (re-search-backward "\\sw\\|\\s_" nil t)
	    (regexp-quote
	     (progn (forward-char 1)
		    (buffer-substring (point)
				      (progn (forward-sexp -1)
					     (while (looking-at "\\s'")
					       (forward-char 1))
					     (point)))))
	  nil))))

; (re-)define a fifth parameter for directory files on Non-XEmacs, sigh
(cond ((not (string-match "XEmacs\\|Lucid" emacs-version))
       (setf (symbol-value 'orig-directory-files)
	     (symbol-function 'directory-files))
       (setf (symbol-function 'directory-files)
	     (function 
	      (lambda (DIRNAME &optional FULL MATCH NOSORT FILES-ONLY)
		"Return a list of names of files in DIRECTORY.
There are four optional arguments:
If FULL is non-nil, absolute pathnames of the files are returned.
If MATCH is non-nil, only pathnames containing that regexp are returned.
If NOSORT is non-nil, the list is not sorted--its order is unpredictable.
 NOSORT is useful if you plan to sort the result yourself.
If FILES-ONLY is the symbol t, then only the \"files\" in the directory
 will be returned; subdirectories will be excluded.  If FILES-ONLY is not
 nil and not t, then only the subdirectories will be returned.  Otherwise,
 if FILES-ONLY is nil (the default) then both files and subdirectories will
 be returned."
		(funcall orig-directory-files DIRNAME FULL MATCH NOSORT))))))


	    
;;; Okay, here we get going ....


(make-local-variable '*translate-globdict*)

;; The real work ...
(defun translate-it (phrase)
  "Translates a phrase."
 (message (concat "Looking up \"" phrase "\"..."))
 (shell-command (translate-construct-call phrase))
 (cond ((not (get-buffer "*Shell Command Output*"))
	(message "Could not find translation.")
	(if *translate-ask-user*
	    (translate-ask-user phrase)))))
 
;; construct the call to grep
(defun translate-construct-call (phrase)
 "Construct the call."
 (concat *translate-program*
	 *translate-program-extra-args*
	 phrase
	 "\" "
	 (translate-construct-dictlist)))

(defun translate-construct-dictlist ()
  "Returns a string of dictionary files."
  (let ((gl-dict-acc (file-readable-p *translate-globdict*))
	(pr-dir-acc (file-readable-p *translate-priv-dir*)))
    (if (and (not gl-dict-acc)
	     (not pr-dir-acc))
	(error "Cannot access any dictionary."))
    (translate-constdict
     (append 
      (directory-files 
       *translate-priv-dir* t 
       *translate-dict-regexp* nil t)
      (list *translate-globdict*)))))
      
(defun translate-constdict (lofn)
  ;; Doing the real work
  (let ((string " "))
    (dolist (file lofn string)
      (setq string (concat string file " ")))))

;; Ask the user for a translation
(defun translate-ask-user (phrase)
 "Ask the user what the translation of the current phrase means.

 If the user does not enter a translation nothing will be inserted."
 (let* ((trans (read-from-minibuffer 
	       (format "Unknown. Translation of %s : " phrase))))
   (if (not (equal 0 (length trans)))
       ;; re-present translation, provide minibuffer-commands
       (translate-do-insertion phrase trans))))

;; ... and insert it into ...
(defun translate-do-insertion (phrase trans)
 "Insert translation in a personal dictionary."
 (let ((persdict (translate-choose-persdict))
       (curbuf (buffer-name)))
   (save-excursion
     (let ((buf (find-file-noselect persdict)))
       (set-buffer buf)
       (goto-char (point-max))
       (insert-string 
	(concat phrase *translate-separator* trans "\n"))
       (message "Inserting \"%s%s%s\" into personal dictionary." 
		phrase *translate-separator* trans)))
   (set-buffer curbuf)))

;; ... a private dictionary
(defun translate-choose-persdict ()
  (if *translate-ask-dicts*
      (expand-file-name
       (read-file-name 
	"Personal dictionary to use: "
	*translate-priv-dir*))
    *translate-single-privdict*))

;;; ============== the interactive functions
;;; ###autoload
(defun translate (phrase)
  "Reads a phrase from the minibuffer and tries to translate it."
 (interactive "sPhrase: ")
 (translate-it phrase))
;;; ###autoload
(defun translate-at-point ()
  "Translate the word where the current point is."
  (interactive)
  (translate-it (symbol-near-point)))
;;; ###autoload
(defun translate-phrase-from-region (p m)
  "Translate the current region."
  (interactive "r")
  (translate-it (buffer-string p m)))
;;; ###autoload
(defun translate-enter-translation (phrase trans)
 "Enter a new translation into a private dictionary."
 (interactive "sPhrase to translate : \nsTranslation : ")
 (translate-do-insertion phrase trans))
;;; ###autoload
(defun translate-change-global-dictionary (path)
  "Sets the (buffer-local) global dictionary used for translations."
  (interactive "fGlobal dictionary: ")
  (setq *translate-globdict* path))
;;; ###autoload
(defun translate-change-private-directory (path)
  "Sets the (buffer-local) private directory used by translate.el."
  (interactive "fPath to private directory: ")
  (setq *translate-priv-dir* path))

(provide 'translate)

;;; translate.el ends here
