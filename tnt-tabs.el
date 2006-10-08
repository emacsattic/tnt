;;; @(#) tnt-tabs.el -- Firefox-like tab support for TNT
;;; @(#) $Id$

;; This file is not part of Emacs

;; Copyright (C) 2006 by Joseph L. Casadonte Jr.
;;
;; The contents of this file are covered under the Artistic License, a
;; copy of which should have accompanied this distribution.  In the
;; event that it did not, you can find a copy here:
;;
;;     http://www.perl.com/pub/a/language/misc/Artistic.html

;; Provides Firefox-like tabs for TNT.  Each IM or chat buffer opened
;; up will show up as a tab in every TNT buffer.
;;
;; Autoloads you may want to consider:
;;
;;     (autoload 'tnt-tabs-toggle-tabs-use "tnt-tabs" "Toggle use of tabs on and off." t)

;; This code is loosely based on ERC's tabs, which in turn are loosely
;; based on W3M's tabs.  Many thanks for the work that has been done
;; before which made my job a whole lot easier!

;; NOTE: this is not needed under XEmacs; it already has tabs.  In
;; fact, it won't work under XEmacs, because it doesn't have a
;; header-line.  Consequently, I have disabled it under XEmacs.

;; TODO: reverse-engineer XEmacs buffer-tabs stuff (in lisp/gutter-items.el)
;; to see if the controls and face changes here can be used with the
;; XEmacs gutter.


;;; ***************************************************************************
;;; Customization support
;;; ***************************************************************************
(require 'custom)

(defgroup tnt-tabs nil
  "Firefox-like tab options for TNT"
  :group 'tnt)

;; ---------------------------------------------------------------------------
(defcustom tnt-tabs-show-tabs nil
  "Indicate whether or not to use tabs.

Non-nil means to show the tabs in TNT buffers.  Should not be set
interactively; use `tnt-tabs-toggle-tabs-use' instead."
  :type 'boolean
  :group 'tnt-tabs)

;; ---------------------------------------------------------------------------
(defcustom tnt-tabs-show-activity t
  "Indicate whether or not to reflect 'activity' in tabs.

Non-nil means to show such activity using a different face."
  :type 'boolean
  :group 'tnt-tabs)

;; ---------------------------------------------------------------------------
(defcustom tnt-tabs-min-width 4
  "Minimum width of any one tab."
  :group 'tnt-tabs
  :type 'number)

;; ---------------------------------------------------------------------------
(defcustom tnt-tabs-max-width 18
  "Maximum width of any one tab."
  :group 'tnt-tabs
  :type 'number)

;; ---------------------------------------------------------------------------
(defun tnt-tabs-customize-tab-name-fn (symbol newval)
  "Reset all of the tab names as the defcustom setting changes."
  (set-default symbol newval)

  (when (and (boundp 'tnt-current-user)
			 tnt-current-user)
    (tnt-tabs-update-all-tabs)))

;; ...........................................................................
(defcustom tnt-tabs-tab-name-fn 'tnt-tabs-name-buffer-name
  "Function to call when naming tabs.

Available functions are:
`tnt-tabs-name-buffer-name' - use the buffer name as the tab name (default)
`tnt-tabs-name-buffer-name-no-stars' - as buffer-name, but strip off '*'
`tnt-tabs-name-short-buffer-name' - as buffer-name, strip off im- and chat-
`tnt-tabs-name-short-no-stars' - as short-buffer-name, but strip off '*'
`tnt-tabs-name-fullname' - uses the TNT fullname as the tab name
`tnt-tabs-name-buddy-name' - uses the actual buddy name
`tnt-tabs-name-fullname-or-buddy-name' - usez the fullname or buddy name

In each case, see the fuction's description for more information.  You
can write your own function and use it; such a function should take in
a buffer and return a name for that buffer on a tab."
  :group 'tnt-tabs
  :set 'tnt-tabs-customize-tab-name-fn
  :type 'function)

;; ---------------------------------------------------------------------------
(defface tnt-tabs-selected-face
  '((((type x w32 mac) (class color))
     :background "Gray88" :foreground "black"
     :underline "Gray88" :box (:line-width 1))
    (((class color))
     (:background "blue" :foreground "black" :underline "blue"))
    (t (:underline t)))
  "Font face of current tab."
  :group 'tnt-tabs)

;; ---------------------------------------------------------------------------
(defface tnt-tabs-selected-active-face
  '((((type x w32 mac) (class color))
     :background "Gray88" :foreground "firebrick"
     :underline "Gray88" :box (:line-width 1))
    (((class color))
     (:background "blue" :foreground "black" :underline "blue"))
    (t (:underline t)))
  "Font face of current tab with activity.

Only used when `tnt-tabs-show-activity' is non-nil (which see)."
  :group 'tnt-tabs)

;; ---------------------------------------------------------------------------
(defface tnt-tabs-unselected-face
  '((((type x w32 mac) (class color))
     :background "Gray60" :foreground "Gray20"
     :underline "Gray88" :box (:line-width 1))
    (((class color))
     (:background "cyan" :foreground "black" :underline "blue")))
  "Font face of non-current tab."
  :group 'tnt-tabs)

;; ---------------------------------------------------------------------------
(defface tnt-tabs-unselected-active-face
  '((((type x w32 mac) (class color))
     :background "Gray60" :foreground "firebrick"
     :underline "Gray88" :box (:line-width 1))
    (((class color))
     (:background "cyan" :foreground "red" :underline "blue")))
  "Font face of non-current tab with activity.

Only used when `tnt-tabs-show-activity' is non-nil (which see)."
  :group 'tnt-tabs)

;;; **************************************************************************
;;; ***** tab support
;;; **************************************************************************
(defun tnt-tabs-toggle-tabs-use ()
  "Interactively toggle use of tabs on and off."
  (interactive)
  (setq tnt-tabs-show-tabs (not tnt-tabs-show-tabs))
  (message "TNT tabs will%s be used."
		   (if tnt-tabs-show-tabs "" " not"))
  (if tnt-tabs-show-tabs
	  (tnt-tabs-update-all-tabs)
	(tnt-tabs-remove-all-tabs)))

;;; **************************************************************************
(defun tnt-tabs-remove-all-tabs ()
  "Unset the header line for all TNT buffers."
  (unless tnt-running-xemacs
	(save-excursion
	  (mapcar
	   (lambda (b)
		 (set-buffer b)
		 (setq header-line-format nil)
		 (force-mode-line-update)
		 (let ((window-min-height 1))
		   (shrink-window 1)
		   (enlarge-window 1)))
	   tnt-buffer-list)
	  )))

;;; **************************************************************************
(defun tnt-tabs-update-all-tabs ()
  "Update all tabs in all TNT buffers."
  (unless tnt-running-xemacs
	(when tnt-tabs-show-tabs
	  (mapcar 'tnt-tabs-create-buffer-tabs tnt-buffer-list))))

;;; ...........................................................................
(defun tnt-tabs-make-keymap (buffer)
  "Keymap for mouse downs in tabs for buffer BUFFER."
  (let ((map (make-sparse-keymap))
		(fn `(lambda (e) (interactive "e")
			   (select-window (car (event-start e)))
			   (switch-to-buffer ,buffer))))
    (define-key map [header-line down-mouse-1] 'ignore)
    (define-key map [header-line drag-mouse-1] fn)
    (define-key map [header-line mouse-1] fn)
    map))

;;; ...........................................................................
(defun tnt-tabs-create-buffer-tabs (buffer)
  "Update the tabs in the TNT buffer BUFFER."
  (save-excursion
	(set-buffer buffer)
	(setq header-line-format
		  (mapcar
		   (lambda (b)
			 (let ((name (tnt-tabs-buffer-to-tab-name b)))
			   (concat
				(propertize
				 (truncate-string-to-width name tnt-tabs-max-width)
				 'face (cond
						((and (eq b buffer)
							  (tnt-buffer-has-activity b))
						 'tnt-tabs-selected-active-face)
						((eq b buffer)
						 'tnt-tabs-selected-face)
						((tnt-buffer-has-event b)
						 'tnt-tabs-unselected-active-face)
						((tnt-buffer-has-activity b)
						 'tnt-tabs-unselected-active-face)
						(t
						 'tnt-tabs-unselected-face))
				 'local-map (tnt-tabs-make-keymap b))
				" ")
			   ))
		   tnt-buffer-list))

	;; ugly hack until force-mode-line-update updates the header-line
	;; when there are no other changes; should be broken out into a
	;; separate function which can be surrounded by version checking,
	;; as this is (supposedly) fixed in Emacs 23
	(let ((window-min-height 1))
	  (shrink-window 1)
	  (enlarge-window 1))

	(force-mode-line-update)))

;;; **************************************************************************
;;; ***** tab name functions
;;; **************************************************************************
(defun tnt-tabs-buffer-to-tab-name (buffer)
  "Take in BUFFER and return a name to use in the tab.

Conversion is done by the function named in `tnt-tabs-tab-name-fn'."
  (let ((name (funcall tnt-tabs-tab-name-fn b))
		(pre-pad nil))
	;; if it's too short, pad it
	(while (< (length name) tnt-tabs-min-width)
	  (setq name (concat (if pre-pad " " "") name (if pre-pad "" " ")))
	  (setq pre-pad (not pre-pad)))

	;; if it's too long, trim it
	(when (> (length name) tnt-tabs-max-width)
	  (truncate-string-to-width name tnt-tabs-max-width))
	name))

;;; **************************************************************************
(defun tnt-tabs-name-buffer-name (buffer)
  "Convert BUFFER into a tab name by simply calling `buffer-name'.

'*im-foobar*'  == '*im-foobar*'
'*chat-12345*' == '*chat-12345*'
'*buddies*     == '*buddies*'"
  (buffer-name buffer))

;;; **************************************************************************
(defun tnt-tabs-name-buffer-name-no-stars (buffer)
  "Convert BUFFER into a tab name based on `buffer-name'.

'*im-foobar*'  == 'im-foobar'
'*chat-12345*' == 'chat-12345678'
'*buddies*     == 'buddies'"
  (let ((buffer-name (tnt-tabs-name-buffer-name buffer)))
	(save-match-data
	  (if (string-match "^\\*\\(.*\\)\\*$" buffer-name)
		  (match-string 1 buffer-name)
		buffer-name))))

;;; **************************************************************************
(defun tnt-tabs-name-short-buffer-name (buffer)
  "Convert BUFFER into a tab name based on `buffer-name'.

'*im-foobar*'  == '*foobar*'
'*chat-12345*' == '*12345*'
'*buddies*     == '*buddies*'"
  (let ((buffer-name (tnt-tabs-name-buffer-name buffer)))
	(save-match-data
	  (if (or (string-match "^\\*im-\\(.*\\)" buffer-name)
			  (string-match "^\\*chat-\\(.*\\)" buffer-name))
		  (concat "*" (match-string 1 buffer-name))
		buffer-name))))

;;; **************************************************************************
(defun tnt-tabs-name-short-no-stars (buffer)
  "Convert BUFFER into a tab name based on `buffer-name'.

'*im-foobar*'  == 'foobar'
'*chat-12345*' == '12345'
'*buddies*     == 'buddies'"
  (let ((buffer-name (tnt-tabs-name-short-buffer-name buffer)))
	(save-match-data
	  (if (string-match "^\\*\\(.*\\)\\*$" buffer-name)
		  (match-string 1 buffer-name)
		buffer-name))))

;;; **************************************************************************
(defun tnt-tabs-name-fullname (buffer)
  "Convert BUFFER into a tab name based on fullname or `buffer-name'.

'*im-foobar*'  == 'John Foob'
'*im-snafu*'   == '*im-snafu*'
'*chat-12345*' == '*chat-12345*'
'*buddies*     == '*buddies*'"
  (let ((nick (with-current-buffer buffer tnt-im-user)))
	(or (and nick (tnt-fullname-for-nick nick))
		(buffer-name buffer))))

;;; **************************************************************************
(defun tnt-tabs-name-buddy-name (buffer)
  "Convert BUFFER into a tab name based on 'real' nickname.

'*im-foobar*'  == 'Foo Bar'
'*im-snafu*'   == 'SNAFU'
'*chat-12345*' == '*chat-12345*'
'*buddies*     == '*buddies*'"
  (let ((nick (with-current-buffer buffer tnt-im-user)))
	(or nick (buffer-name buffer))))

;;; **************************************************************************
(defun tnt-tabs-name-fullname-or-buddy-name (buffer)
  "Convert BUFFER into a tab name based on fullname or nickname.

'*im-foobar*'  == 'Foo Bar'
'*im-snafu*'   == 'SNAFU'
'*chat-12345*' == '*chat-12345*'
'*buddies*     == '*buddies*'"
  (let ((nick (with-current-buffer buffer tnt-im-user)))
	(or (and nick (tnt-fullname-for-nick nick))
		nick
		(buffer-name buffer))))

;;; **************************************************************************
;;; tab navigation
;;; **************************************************************************
(defun tnt-tabs-next-tab (&optional seed-list)
  "Switch to the next buffer in `tnt-buffer-list'.

Use SEED-LIST instead of `tnt-buffer-list' if provided."
  (interactive)
  (let ((orig-list (or seed-list (copy-list tnt-buffer-list)))
		(buffer (current-buffer))
		new-buffer temp-list)
	(if (not (member buffer tnt-buffer-list))
		(setq new-buffer (car orig-list))
	  (setq temp-list orig-list)
	  (while (not (and temp-list (eq buffer (car temp-list))))
		(setq temp-list (cdr-safe temp-list)))
	  (setq new-buffer (if (cdr-safe temp-list)
						   (cadr temp-list)
						 (car orig-list))))
	(switch-to-buffer new-buffer)))

;;; **************************************************************************
(defun tnt-tabs-prev-tab ()
  "Switch to the previous buffer in `tnt-buffer-list'."
  (interactive)
  (tnt-tabs-next-tab (reverse tnt-buffer-list)))

;;; **************************************************************************
;;; ***** we're done
;;; **************************************************************************
(provide 'tnt-tabs)
(run-hooks 'tnt-tabs-load-hook)

;;; tnt-tabs.el ends here
;;; **************************************************************************
;;;; *****  EOF  *****  EOF  *****  EOF  *****  EOF  *****  EOF  *************


;;; XEmacs testing
;; (add-to-list 'load-path "~/emacs/site/lisp/tnt.latest")
;; (autoload 'tnt-open "tnt" "AIM - AOL Instant Messenger" t)
;; (setq tnt-default-username "tolkien deadhead")
;; (setq tnt-default-password "foobarbaz")
;; (setq tnt-show-idle-in-mode ":I")
;; (setq tnt-show-events-in-mode t)
;; (setq tnt-show-email-in-mode ":E")
;; (setq tnt-show-away-in-mode ":A")
;; (setq tnt-show-activity-in-mode t)
;; (setq tnt-mode-indicator "TNT")
;; (setq

;;; Emacs testing
;; (tnt-switch-user)
;; (tnt-switch-user)
;; (tnt-switch-user)
;; (tnt-switch-user)
;; (tnt-proxy-switch-servers)
;; (tnt-proxy-switch-servers)
;; (tnt-proxy-switch-servers)


;; xx - I'm pre-pending to the buffer-list, not appending
;; xx - message waiting on existing buffer should show red in tab list
;; xx - message waiting on non-existing buffer should show red in buddies list
;; xx - message waiting call should set update-tabs

;; xx - have different names functions *im-foo* / *foo* / foo / Foo Bar
;; xx - have an all-bells-and-whistles function
;; xx - add :set to tnt-tabs-tab-name-fn

;; xx - handle clearing of MESSAGE WAITING better
;; xx - little line between tabs disapears when frame loses focus

;; xx - buffer/tab navigation
;; xx - add tab navigation to menus

;; xx - modeline [-TNT] delayed still