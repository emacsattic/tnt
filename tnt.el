; -*- indent-tabs-mode: nil -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TNT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Copyright (c) 1998 America Online, Inc. All Rights Reserved.
;;;;
;;;; AOL grants you ("Licensee") a non-exclusive, royalty free, license to
;;;; use, modify and redistribute this software in source and binary code
;;;; form, provided that i) this copyright notice and license appear on all
;;;; copies of the software; and ii) Licensee does not utilize the software
;;;; in a manner which is disparaging to AOL.
;;;;
;;;; This software is provided "AS IS," without a warranty of any kind. ALL
;;;; EXPRESS OR IMPLIED CONDITIONS, REPRESENTATIONS AND WARRANTIES, INCLUDING
;;;; ANY IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE
;;;; OR NON-INFRINGEMENT, ARE HEREBY EXCLUDED. AOL AND ITS LICENSORS SHALL NOT
;;;; BE LIABLE FOR ANY DAMAGES SUFFERED BY LICENSEE AS A RESULT OF USING,
;;;; MODIFYING OR DISTRIBUTING THE SOFTWARE OR ITS DERIVATIVES. IN NO EVENT
;;;; WILL AOL OR ITS LICENSORS BE LIABLE FOR ANY LOST REVENUE, PROFIT OR DATA,
;;;; OR FOR DIRECT, INDIRECT, SPECIAL, CONSEQUENTIAL, INCIDENTAL OR PUNITIVE
;;;; DAMAGES, HOWEVER CAUSED AND REGARDLESS OF THE THEORY OF LIABILITY, ARISING
;;;; OUT OF THE USE OF OR INABILITY TO USE SOFTWARE, EVEN IF AOL HAS BEEN
;;;; ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
;;;;
;;;; This software is not designed or intended for use in on-line control of
;;;; aircraft, air traffic, aircraft navigation or aircraft communications;
;;;; or in the design, construction, operation or maintenance of any nuclear
;;;; facility. Licensee represents and warrants that it will not use or
;;;; redistribute the Software for such purposes.

;;;; TODO:
;;;;   implement permit/deny
;;;;   point reset by erase-buffer to beginning of buddy buffer during update
;;;;   consider using use-hard-newlines variable
;;;;   make processed im messages read-only

(provide 'tnt)
(require 'toc)

;; only using cl functions in one place...  maybe we should just
;; implement those functions locally?
(require 'cl)


(defconst tnt-version "TNT 2.2.1")


;;; Config variables

; these generally should not be changed
(defvar tnt-toc-host "toc.oscar.aol.com")
(defvar tnt-toc-port 5190)
(defvar tnt-login-host "login.oscar.aol.com")
(defvar tnt-login-port 5190)
(defvar tnt-language "english")


; check whether this version of emacs has the "run-at-time" function
(defvar tnt-timers-available (fboundp 'run-at-time))

; among versions of emacs which do have "run-at-time", some have a
; buggy implementation of "run-with-idle-timer".  this var indicates
; whether we're running in such a version of emacs.  there's no quick
; way to automate checking for this, so the user should run "M-x
; tnt-test-for-buggy-idle" to find out.
(defvar tnt-buggy-idle t)


; these may be changed, but rather than changing them here, use
; (setq <variablename> <value>) in your .emacs file.  see INSTALL
; for more info.

(defvar tnt-default-username nil
  "*Should be nil or a string containing your username.

If you set this, you will not have to type in your username every
time you want to log in.
")

(defvar tnt-default-password nil
  "*Should be nil or your password.

If you set this, you will not have to type in your password every time
you want to log in.  However, if the file you set this in is readable
by other users on your system, they could log in as you, so if you set
this, be careful.  Also note that if you use multiple usernames, you
should not set the tnt-default-password (unless all your usernames use
the same password).
")

(defvar tnt-username-alist nil
  "*Should be nil or a list of usernames and (optionally) passwords.

If you have more than one username, set the list this way:
  (setq tnt-username-alist '((\"UserName1\" . \"Password1\")
                             (\"UserName2\" . \"Password2\")
                             (\"UserName3\")))

Then you can use \"C-x t s\" to change which username will be used the
next time you connect.  Any number of usernames can be listed in this
way, but note that each element of the list MUST be either of the form
(\"UserName\") or of the form (\"UserName\" . \"Password\"), and note the
apostrophe.  When you log in as a username for which the password is
not stored here, you will be prompted.
")

(defvar tnt-separator "\n\n"
  "*String printed between IMs.

This controls what is printed between message.  One newline is pretty
much required in order separate messages.  Two newlines is the default,
but other strings, suchs as \"\n-\n\" may be desirable.
")

(defvar tnt-use-timestamps nil
  "*If non-nil, shows timestamps in TNT conversations.

This will add a timestamp for each message you receive or send.
This is useful for logging, and for people who are on very often.
It's also convenient if you leave yourself logged in while away
from your computer, so then when you come back, you can see how
long ago it was that your friend said \"hi\" while you were gone.
")

(defvar tnt-beep-on-message-available-event 'current
  "*If non-nil, beeps when giving the \"Message from ... available\" message.

If set to 'visible, uses a visible bell.  If set to 'audible, uses an
audible bell.  If set to 'current, uses whatever emacs is currently
set to use (visible or audible).  If set to nil, does not beep.

Note that the value here is also used when receiving a chat
invitation.
")

(defvar tnt-beep-on-message-in-visible-buffer nil
  "*If non-nil, beeps every time a message comes into a visible IM buffer.

If set to 'visible, uses a visible bell.  If set to 'audible, uses an
audible bell.  If set to 'current, uses whatever emacs is currently
set to use (visible or audible).  If set to nil, does not beep.
")

(defvar tnt-beep-on-buddy-signonoff nil
  "*If non-nil, beeps when buddies sign on or off.

If set to 'visible, uses a visible bell.  If set to 'audible, uses an
audible bell.  If set to 'current, uses whatever emacs is currently
set to use (visible or audible).  If set to nil, does not beep.

Note that whatever value this variable has, you will still get
messages in your minibuffer saying \"MyBuddy online\" and \"MyBuddy
offline\".
")

(defvar tnt-beep-on-error 'current
  "*If non-nil, beeps when an error occurs.

If set to 'visible, uses a visible bell.  If set to 'audible, uses an
audible bell.  If set to 'current, uses whatever emacs is currently
set to use (visible or audible).  If set to nil, does not beep.
")
  

(defvar tnt-message-on-buddy-signonoff t
  "*If non-nil, a minibuffer message appears when buddies sign on and off.")

(defvar tnt-use-split-buddy nil
  "*If non-nil, tnt will split the window when going to the buddy list.

Note that this rule does not apply when in in the *scratch* buffer, or
when already in the *buddies* buffer.
")

(defvar tnt-use-keepalive tnt-timers-available
  "*If non-nil, sends a keepalive packet once a minute")

(defvar tnt-use-buddy-update-timer tnt-timers-available
  "*If non-nil, updates the idle times in the buddy list each minute.")

(defvar tnt-use-idle-timer (and tnt-timers-available (not tnt-buggy-idle))
  "*If non-nil, tells TOC server when emacs has been idle for 10 minutes.

NOTE: under certain versions of emacs, you become unidle any time tnt
receives any message from the toc server.  Try \"M-x
tnt-test-for-buggy-idle\", and if it tells you that your idle timers
are correct, you should set tnt-buggy-idle to nil.
")

(defvar tnt-buddy-list-backup-filename "~/.tnt-%s-buddies"
  "*If non-nil, tnt will backup buddy list into this file.

Occasionally, the toc server loses people's buddy lists.  If a
(non-nil) filename is given, tnt will backup your buddy list to a
file, and if the server loses it, tnt will restore from the backup.
If the given filename includes \"%s\", then your username will be
substituted.
")

(defvar tnt-recenter-windows t
  "*If non-nil, recenters text to bottom of window when messages are printed.")

(defvar tnt-email-to-pipe-to nil
  "*Should be nil or a string containing an email address.

Setting this allows you to toggle forwarding of all incoming IMs to
the specified address.  This might be useful if you have an email
address which goes to an alphanumeric pager, and you want to be able
to receive IMs anywhere!  Once the email address is specified, turn
forwarding on and off with \"C-x t M\".
")

(defvar tnt-email-binary "/bin/mail"
  "*Should be set to the executable of your mail binary.

Note that you only need to set this if you're using the pipe-to-email
feature.  defaults to /bin/mail
")

;; Faces for color highlighting of screen names.
;; if they already exist, we don't want to change the colors.
(if (not (member 'tnt-other-name-face (face-list)))
    (progn
      (make-face 'tnt-other-name-face)
      (set-face-foreground 'tnt-other-name-face "blue")))
(if (not (member 'tnt-my-name-face (face-list)))
    (progn
      (make-face 'tnt-my-name-face)
      (set-face-foreground 'tnt-my-name-face "red")))


;;; Key bindings

(defvar tnt-inhibit-key-bindings nil)

(if tnt-inhibit-key-bindings
    ()
  (global-set-key "\C-xt?" 'tnt-show-help)
  (global-set-key "\C-xto" 'tnt-open)
  (global-set-key "\C-xtk" 'tnt-kill)
  (global-set-key "\C-xti" 'tnt-im)
  (global-set-key "\C-xtj" 'tnt-join-chat)
  (global-set-key "\C-xtl" 'tnt-leave-chat)
  (global-set-key "\C-xtb" 'tnt-show-buddies)
  (global-set-key "\C-xta" 'tnt-accept)
  (global-set-key "\C-xtr" 'tnt-reject)
  (global-set-key "\C-xtp" 'tnt-prev-event)
  (global-set-key "\C-xtn" 'tnt-next-event)
  (global-set-key "\C-xtB" 'tnt-edit-buddies)
  (global-set-key "\C-xts" 'tnt-switch-user)
  (global-set-key "\C-xtA" 'tnt-away-toggle)
  (global-set-key "\C-xtP" 'tnt-pounce-add)
  (global-set-key "\C-xtD" 'tnt-pounce-delete)
  (global-set-key "\C-xtM" 'tnt-toggle-email)
)


;;; Globals (that need to be declared early on...)

(defvar tnt-current-user nil)
(defvar tnt-pipe-to-email-now nil)

(defvar tnt-buddy-alist nil)
(defvar tnt-buddy-blist nil)

(defvar tnt-permit-mode 1)
(defvar tnt-permit-list nil)
(defvar tnt-deny-list nil)



;;;---------------------------------------------------------------------------
;;;  Pounce Package - jnwhiteh@syr.edu
;;;---------------------------------------------------------------------------

(defvar tnt-pounce-alist nil)


;;; Pounce Code
(defun tnt-pounce-add ()
  "Allows a user to store a pounce message for a buddy"
  (interactive)
  (let* ((completion-ignore-case t)
         (nick (completing-read "Buddy to Pounce on: "
                                (mapcar 'list
                                        (tnt-extract-normalized-buddies
                                         tnt-buddy-blist))))
         (msg_tmp (read-from-minibuffer "Message to send (enter for none): "))
         (msg (if (string= msg_tmp "") "none" msg_tmp)))
    (setq tnt-pounce-alist (tnt-addassoc nick msg tnt-pounce-alist))
    (message "%s has been added to your pounce list" nick)))

(defun tnt-pounce-delete (&optional nick)
  "Deletes a stored pounce message"
  (interactive)
  (if (null tnt-pounce-alist)
      (message "No pounce messages to delete")
    (if (not nick)
        (let* ((completion-ignore-case t))
          (setq nick (toc-normalize (completing-read "Delete pounce for user: "
                                                     tnt-pounce-alist)))))
  (if (not (assoc nick tnt-pounce-alist))
      (message "There is no pounce stored for %s" nick)
    (setq tnt-pounce-alist (tnt-remassoc nick tnt-pounce-alist))
    )))

(defun tnt-send-pounce (user)
   (let* ((msg (cdr (assoc user tnt-pounce-alist))))
     (if msg
         (let ((buffer (tnt-im-buffer user)))
           (toc-send-im user msg)
           (tnt-append-message-and-adjust-window buffer msg tnt-current-user)
           (tnt-push-event (format "You have pounced on %s" user) buffer nil)
           (tnt-pounce-delete user))
     )))

;;;---------------------------------------------------------------------------
;;;  Keepalive/Away Packages - jnwhiteh@syr.edu
;;;---------------------------------------------------------------------------
(defvar tnt-keepalive-interval 60)
(defvar tnt-last-away-sent nil)
(defvar tnt-away-msg nil)
(defvar tnt-away-alist nil)
(defvar tnt-away nil)
(defvar tnt-keepalive-timer nil)
(defvar tnt-reconnecting nil)

(defun tnt-buddy-away (nick)
  (cdr (assoc (toc-normalize nick) tnt-away-alist)))

(defun tnt-away-toggle ()
  "Toggles away or not, and sets message."
  (interactive)
  (if tnt-away
      (tnt-not-away)
    (tnt-get-away-msg)))

(defun tnt-not-away ()
  "Sets you as NOT away."
  (interactive)
  (let ((away tnt-away))
    (setq tnt-away nil)
    (setq tnt-last-away-sent nil)
    (if away
        (message "You have returned."))
    (toc-set-away nil)
    (tnt-set-online-state t)
  )
)

;; gse: Added history and default away message stuff.
(defvar tnt-away-msg-history nil)
(defun tnt-get-away-msg ()
  "Gets the away msg"
  (interactive)
  (setq tnt-away-msg (read-from-minibuffer "Away Message: "
                                           (cons
                                            (if tnt-away-msg-history
                                                (car tnt-away-msg-history)
                                              "I'm away.")
                                            0)
                                           nil nil 'tnt-away-msg-history))
  (message "Set as away: %s" tnt-away-msg)
  (setq tnt-away t)
  (toc-set-away tnt-away-msg)
  (tnt-set-online-state t)
)

(defun tnt-send-away-msg (user)
  "Send the current away message to USER."
  (if (not (string= user tnt-last-away-sent))
      (let ((buffer (tnt-im-buffer user)))
        (setq tnt-last-away-sent user)
        (toc-send-im user tnt-away-msg t)
        (tnt-append-message-and-adjust-window
         buffer tnt-away-msg tnt-current-user "Auto-response"))))

;;;----------------------------------------------------------------------------
;;; telling the TOC server we've gone idle
;;;----------------------------------------------------------------------------

(defvar tnt-idle-timer nil)
(defvar tnt-send-idle-after 600)
(defvar tnt-unidle-timer nil)
(defvar tnt-send-unidle-after 1)
(defvar tnt-currently-idle nil)

;; the timers are created in tnt-handle-sign-on below

(defun tnt-send-idle ()
  (if (not tnt-currently-idle)
      (progn
        (setq tnt-currently-idle t)
        (toc-set-idle tnt-send-idle-after))))

(defun tnt-send-unidle ()
  (if tnt-currently-idle
      (progn
        (setq tnt-currently-idle nil)
        (toc-set-idle 0)
        )))


;;;----------------------------------------------------------------------------
;;; Signon/Signoff
;;;----------------------------------------------------------------------------

(defvar tnt-username)
(defvar tnt-password)

(defun tnt-open (username password)
  "Starts a new TNT session."
  (interactive "p\np")  ;; gag!
  (if tnt-current-user
      (error "Already online as %s" tnt-current-user)
    (setq tnt-username (or (and (stringp username) username)
                           tnt-default-username
                           (and tnt-username-alist
                                (caar tnt-username-alist))
                           (read-from-minibuffer "Screen name: "))
          tnt-password (or (and (stringp password) password)
                           tnt-default-password
                           (and tnt-username-alist
                                (cdar tnt-username-alist))
                           (tnt-read-from-minibuffer-no-echo
                            (format "Password for %s: " tnt-username))))
    (if (string-equal tnt-password "")
        (error "No password given")
      (message "Attempting to sign on...")
      (add-hook 'toc-opened-hooks 'tnt-handle-opened)
      (add-hook 'toc-closed-hooks 'tnt-handle-closed)
      (add-hook 'toc-sign-on-hooks 'tnt-handle-sign-on)
      (add-hook 'toc-config-hooks 'tnt-handle-config)
      (add-hook 'toc-nick-hooks 'tnt-handle-nick)
      (add-hook 'toc-im-in-hooks 'tnt-handle-im-in)
      (add-hook 'toc-update-buddy-hooks 'tnt-handle-update-buddy)
      (add-hook 'toc-error-hooks 'tnt-handle-error)
      (add-hook 'toc-eviled-hooks 'tnt-handle-eviled)
      (add-hook 'toc-chat-join-hooks 'tnt-handle-chat-join)
      (add-hook 'toc-chat-in-hooks 'tnt-handle-chat-in)
      (add-hook 'toc-chat-update-buddy-hooks 'tnt-handle-chat-update-buddy)
      (add-hook 'toc-chat-invite-hooks 'tnt-handle-chat-invite)
      (add-hook 'toc-goto-url-hooks 'tnt-handle-goto-url)
      (toc-open tnt-toc-host tnt-toc-port tnt-username))))


(defun tnt-kill ()
  "Ends the current TNT session and signs off from the host."
  (interactive)
  (if (null tnt-current-user)
      (error "Already offline")
    (toc-close)
    (tnt-shutdown)
    (message "Signed off")))


(defun tnt-switch-user ()
  "Switches the default username to log in as."
  (interactive)
  (if (null tnt-username-alist)
      (message "No username list defined.")
    (progn
      (setq tnt-username-alist (tnt-rotate-left tnt-username-alist))
      (if tnt-default-username
          (setq tnt-default-username (caar tnt-username-alist)))
      (if tnt-default-password
          (setq tnt-default-password (cdar tnt-username-alist)))
      (message "Next login will be as user %s" (caar tnt-username-alist)))
    ))



;;;----------------------------------------------------------------------------
;;; Instant message mode
;;;----------------------------------------------------------------------------

(defvar tnt-im-mode-map nil)
(defvar tnt-im-user)
(defvar tnt-message-marker)

(make-variable-buffer-local 'tnt-im-user)

(if tnt-im-mode-map
    ()
  (setq tnt-im-mode-map (make-sparse-keymap))
  (define-key tnt-im-mode-map "\r" 'tnt-send-text-as-instant-message))


(defun tnt-im-mode ()
  "Major mode for sending Instant Messages.
Special commands:
\\{tnt-im-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map tnt-im-mode-map)
  (setq mode-name "IM")
  (setq major-mode 'tnt-im-mode)
  (setq local-abbrev-table text-mode-abbrev-table)
  (set-syntax-table text-mode-syntax-table)
  (auto-fill-mode 1)
  (run-hooks 'tnt-im-mode-hook))


(defun tnt-im (user)
  "Opens an instant-message conversation with a user."
  (interactive "p")
  (let* ((completion-ignore-case t)
         (input (or (and (stringp user) user)
                    (completing-read "Send IM to: "
                                     (tnt-online-buddies-collection)))))
    (switch-to-buffer (tnt-im-buffer input))))


(defun tnt-im-buffer-name (user)
  "Returns the name of the IM buffer for USER."
  (format "*im-%s*" (toc-normalize user)))


(defun tnt-im-buffer (user)
  "Returns the IM buffer for USER."
  (let ((buffer-name (tnt-im-buffer-name user)))
    (or (get-buffer buffer-name)
        (let ((buffer (get-buffer-create buffer-name)))
          (with-current-buffer buffer
            (tnt-im-mode)
            (setq tnt-im-user user)
            (setq tnt-message-marker (make-marker))
            (insert (format "[Conversation with %s on %s]%s"
                            (tnt-buddy-official-name user)
                            (current-time-string)
                            tnt-separator))
            (set-marker tnt-message-marker (point)))
          buffer))))


(defun tnt-send-text-as-instant-message ()
  "Sends text at end of buffer as an IM."
  (interactive)
  (let* ((message (tnt-get-input-message)))
    (if (string= message "")
        (message "Please enter a message to send")
      (tnt-append-message message tnt-current-user))
    (if tnt-away (message "Reminder: You are still set as away"))
    (if tnt-recenter-windows (recenter -1))
    (if (string= message "") () (toc-send-im tnt-im-user message))))



(defun tnt-show-help ()
  "Displays help for TNT."
  (interactive)
  (let* ((buffer-name "*tnt-help*")
         (help-buffer (get-buffer buffer-name)))
    (or (and help-buffer
             (switch-to-buffer help-buffer))
        (let ((buffer (get-buffer-create buffer-name)))
          (with-current-buffer buffer
            (insert "
+-------------------+-------------+-------------------------------------------+
|  Function         | Key Binding |               Summary                     |
+-------------------+-------------+-------------------------------------------+
| tnt-show-help     |   C-x t ?   | Displays this help information            |
| tnt-open          |   C-x t o   | Starts a new TNT session                  |
| tnt-kill          |   C-x t k   | Terminates the current session            |
| tnt-im            |   C-x t i   | Starts an instant-message conversation    |
| tnt-join-chat     |   C-x t j   | Joins a chat room                         |
| tnt-leave-chat    |   C-x t l   | Leaves a chat room                        |
| tnt-show-buddies  |   C-x t b   | Shows the buddy list                      |
| tnt-edit-buddies  |   C-x t B   | Invokes the buddy list editor             |
| tnt-accept        |   C-x t a   | Accepts a message or a chat invitation    |
| tnt-reject        |   C-x t r   | Rejects a message or a chat invitation    |
| tnt-next-event    |   C-x t n   | Shows next event in notification ring     |
| tnt-prev-event    |   C-x t p   | Shows previous event in notification ring |
| tnt-switch-user   |   C-x t s   | Switches between usernames for next login |
| tnt-away-toggle   |   C-x t A   | Toggles away status, sets away message    |
| tnt-pounce-add    |   C-x t P   | Adds a user to your pounce list           |
| tnt-pounce-delete |   C-x t D   | Removes a user from your pounce list      |
| tnt-toggle-email  |   C-x t M   | Toggles forwarding incoming IMs to email  |
+-------------------+-------------+-------------------------------------------+
"))
          (tnt-switch-to-buffer buffer)))))



;;;----------------------------------------------------------------------------
;;; Chat mode
;;;----------------------------------------------------------------------------

(defvar tnt-chat-mode-map nil)
(defvar tnt-chat-alist nil)         ; room id to room name

(defvar tnt-chat-room)
(defvar tnt-chat-roomid)
(defvar tnt-chat-participants)

(make-variable-buffer-local 'tnt-chat-room)
(make-variable-buffer-local 'tnt-chat-roomid)
(make-variable-buffer-local 'tnt-chat-participants)


(if tnt-chat-mode-map
    ()
  (setq tnt-chat-mode-map (make-sparse-keymap))
  (define-key tnt-chat-mode-map "\r"   'tnt-send-text-as-chat-message)
  (define-key tnt-chat-mode-map "\n"   'tnt-send-text-as-chat-whisper)
  (define-key tnt-chat-mode-map "\t"   'tnt-send-text-as-chat-invitation)
  (define-key tnt-chat-mode-map "\M-p" 'tnt-show-chat-participants))


(defun tnt-chat-mode ()
  "Major mode for sending Instant Messages.
Special commands:
\\{tnt-chat-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map tnt-chat-mode-map)
  (setq mode-name "Chat")
  (setq major-mode 'tnt-chat-mode)
  (setq local-abbrev-table text-mode-abbrev-table)
  (set-syntax-table text-mode-syntax-table)
  (auto-fill-mode 1)
  (run-hooks 'tnt-chat-mode-hook))


(defun tnt-join-chat (room)
  "Joins a chat room.  If in a chat buffer assume that is the one to join."
  (interactive "p")
  (if (null tnt-current-user)
      (error "You must be online to join a chat room.")
    (let* ((input (or (and (stringp room) room)
                      (and (boundp 'tnt-chat-room) tnt-chat-room)
                      (read-from-minibuffer "Join chat room: "
                                            (format "%s Chat%03d"
                                                    tnt-current-user
                                                    (random 1000))))))
      (toc-chat-join input)
      (switch-to-buffer (tnt-chat-buffer input)))))

(defun tnt-leave-chat (room)
  "Leaves a chat room.  If in a chat buffer assume that is the one to leave."
  (interactive "p")
  (if (null tnt-current-user)
      (error "You must be online to leave a chat room.")
    (let* ((completion-ignore-case t)
           (input (or (and (stringp room) room)
                      (and (boundp 'tnt-chat-room) tnt-chat-room)
                      (completing-read "Leave chat room: "
                                       (mapcar (lambda (x) (list (cdr x)))
                                               tnt-chat-alist)))))
      (with-current-buffer (tnt-chat-buffer input)
        (setq tnt-chat-participants nil)
        (toc-chat-leave tnt-chat-roomid)
        (tnt-append-message (format "%s left" tnt-current-user))))))

(defun tnt-chat-buffer-name (room)
  "Returns the name of the chat buffer for ROOM."
  (format "*chat-%s*" (toc-normalize room)))


(defun tnt-chat-buffer (room)
  "Returns the chat buffer for ROOM."
  (let ((buffer-name (tnt-chat-buffer-name room)))
    (or (get-buffer buffer-name)
        (let ((buffer (get-buffer-create buffer-name)))
          (with-current-buffer buffer
            (tnt-chat-mode)
            (make-local-hook 'kill-buffer-hook)
            (add-hook 'kill-buffer-hook 'tnt-chat-buffer-killed nil t)
            (setq tnt-chat-room room)
            (setq tnt-chat-participants nil)
            (setq tnt-message-marker (make-marker))
            (insert (format "[Chat room \"%s\" on %s]%s"
                            room (current-time-string) tnt-separator))
            (set-marker tnt-message-marker (point)))
          buffer))))


(defun tnt-chat-buffer-killed ()
  (if tnt-current-user
      (tnt-leave-chat tnt-chat-room)))


(defun tnt-send-text-as-chat-message ()
  (interactive)
  (let ((message (tnt-get-input-message)))
    (toc-chat-send tnt-chat-roomid message)))


(defun tnt-send-text-as-chat-whisper (user)
  (interactive "p")
  (let* ((completion-ignore-case t)
         (user (or (and (stringp user) user)
                  (completing-read "Whisper to user: "
                                   (tnt-participant-collection))))
        (message (tnt-get-input-message)))
    (if (= (length message) 0)
        (setq message (read-from-minibuffer "Message: ")))
    (tnt-append-message
     message tnt-current-user
     (format "whispers to %s" (tnt-buddy-official-name user)))
    (if tnt-recenter-windows (recenter -1))
    (toc-chat-whisper tnt-chat-roomid user message)))


(defun tnt-participant-collection ()
  (mapcar 'list tnt-chat-participants))


(defun tnt-send-text-as-chat-invitation (users)
  (interactive "p")

  (let ((user-list
         (if (and (listp users) users)
             users
           
           ;; replace (buddy-list) groups in the user-list with the
           ;; members of those groups who are online, and not already
           ;; in the chat, then prompt again to confirm.
           
           (let ((completion-ignore-case t)
                 (user-list-typed '(""))
                 (user-list-processed nil))
             
             (while (not (equal user-list-typed user-list-processed))
               (setq user-list-typed (tnt-completing-read-list
                                      "Users to invite: "
                                      (tnt-online-buddies-and-groups-collection)
                                      user-list-processed))
               (setq user-list-processed (tnt-expand-groups-for-chat-invitation
                                          user-list-typed
                                          tnt-chat-participants))
               )
             user-list-processed)
           )
         ))
    
    (if user-list
        
        (let (msg (tnt-get-input-message))
          (if (= (length msg) 0)
              (setq msg (read-from-minibuffer "Message: "
                                              "Join me in this Buddy Chat.")))
          (tnt-append-message msg tnt-current-user
                              (format "invites %s"
                                      (mapconcat 'tnt-buddy-official-name
                                                 user-list ", ")))
          (if tnt-recenter-windows (recenter -1))
          (toc-chat-invite tnt-chat-roomid msg user-list)
          )
      )
    )
  )


(defun tnt-show-chat-participants ()
  "Append a list of chat room participants to a chat buffer."
  (interactive)
  (let ((string (mapconcat 'identity tnt-chat-participants ", ")))
    (tnt-append-message (format "Participants: %s" string))))


(defun tnt-chat-event-pop-function (accept)
  ;; Called when chat event is popped.  If event is accepted, the
  ;; current buffer is the chat buffer.
  (if accept
      (toc-chat-accept tnt-chat-roomid)))

(defun tnt-expand-groups-for-chat-invitation (user-list exclude-list)
  (remove-duplicates 
   (apply 'append
    (mapcar (lambda (name)
              (let ((group (assoc name tnt-buddy-blist)))
                (if (null group) (list name)
                  (intersection (mapcar (lambda (x)
                                          (if (null (cdr x)) nil
                                            (car x)))
                                        tnt-buddy-alist)
                                (set-difference (cdr group)
                                                exclude-list
                                                ':test 'string=)
                                ':test 'string=)
                  )))
            user-list)
    )
   ':test 'string=)
  )



;;;----------------------------------------------------------------------------
;;; Utilites for the messaging modes (im, chat)
;;;----------------------------------------------------------------------------

(make-variable-buffer-local 'tnt-message-marker)

(defun tnt-append-message-and-adjust-window (buffer message &optional user mod)
  (let ((window (get-buffer-window buffer)))
    (with-current-buffer buffer
      (tnt-append-message (tnt-strip-html message) user mod)
      (if (and window tnt-recenter-windows)
          (let ((old-window (selected-window)))
            (select-window window)
            (recenter -1)
            (select-window old-window))))))

(defun tnt-append-message (message &optional user modified)
  "Prepends USER (MODIFIED) to MESSAGE and appends the result to the buffer."
  (save-excursion
    (let ((old-point (marker-position tnt-message-marker)))
      (goto-char tnt-message-marker)

      (if tnt-use-timestamps
          (insert-before-markers (format-time-string "%T ")))
      
      (if (not user)
          (insert-before-markers "[" message "]")

        (let ((start (point)))
          (insert-before-markers user)
          (if modified
              (insert-before-markers " (" modified ")"))
          (insert-before-markers ":")
          ;; Change color of user text.
          (if (string-equal user tnt-current-user)
              (add-text-properties start (point) '(face tnt-my-name-face))
            (add-text-properties start (point) '(face tnt-other-name-face)))
          (insert-before-markers " " message)))

      (insert-before-markers tnt-separator)
      (fill-region old-point (point)))))


(defun tnt-get-input-message ()
  (let ((message (buffer-substring tnt-message-marker (point-max))))
    (delete-region tnt-message-marker (point-max))
    (goto-char (point-max))
    (if tnt-recenter-windows (recenter -1))
    (tnt-neliminate-newlines message)))


;;;----------------------------------------------------------------------------
;;; Buddy list mode
;;;----------------------------------------------------------------------------

(defvar tnt-buddy-list-mode-map nil)

(defvar tnt-idle-alist nil)
(defvar tnt-away-alist nil)

(defvar tnt-just-signedonoff-alist nil)
(defvar tnt-just-signedonoff-delay 60)

(defvar tnt-login-flag-unset-after 3)
(defvar tnt-login-flag nil)

(defvar tnt-buddy-update-timer nil)
(defvar tnt-buddy-update-interval 60)


(if tnt-buddy-list-mode-map
    ()
  (setq tnt-buddy-list-mode-map (make-sparse-keymap))
  (define-key tnt-buddy-list-mode-map "n" 'tnt-next-buddy)
  (define-key tnt-buddy-list-mode-map "p" 'tnt-prev-buddy)
  (define-key tnt-buddy-list-mode-map "N" 'tnt-next-group)
  (define-key tnt-buddy-list-mode-map "P" 'tnt-prev-group)
  (define-key tnt-buddy-list-mode-map "i" 'tnt-im-buddy)
  (define-key tnt-buddy-list-mode-map "\C-m" 'tnt-im-buddy)
  (define-key tnt-buddy-list-mode-map [down-mouse-2] 'tnt-im-buddy-mouse)
  (define-key tnt-buddy-list-mode-map [mouse-2] 'ignore)
  (define-key tnt-buddy-list-mode-map "I" 'tnt-fetch-info)
  (define-key tnt-buddy-list-mode-map " " 'tnt-show-buddies)
  (define-key tnt-buddy-list-mode-map "q" 'tnt-kill)
  )


(defun tnt-buddy-list-mode ()
  "Major mode for viewing a buddy list.
Special commands:
\\{tnt-buddy-list-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map tnt-buddy-list-mode-map)
  (setq mode-name "Buddy List")
  (setq major-mode 'tnt-buddy-list-mode)
  (set-syntax-table text-mode-syntax-table)
  (run-hooks 'tnt-buddy-list-mode-hook))


(defun tnt-show-buddies ()
  "Shows the buddy list in the selected window."
  (interactive)
  (tnt-build-buddy-buffer)
  (tnt-switch-to-buffer (tnt-buddy-buffer)))

(defun tnt-switch-to-buffer (buffer)
 (if (and tnt-use-split-buddy
          (not (string-equal (buffer-name) "*scratch*"))
          (not (string-equal (buffer-name) "*buddies*")))
     (switch-to-buffer-other-window buffer)
   (switch-to-buffer buffer)))

(defun tnt-buddy-buffer ()
  (let ((buffer-name "*buddies*"))
    (or (get-buffer buffer-name)
        (let ((buffer (get-buffer-create buffer-name)))
          (with-current-buffer buffer
            (tnt-buddy-list-mode)
            (setq buffer-read-only t))
          buffer))))


(defun tnt-build-buddy-buffer ()
  (with-current-buffer (tnt-buddy-buffer)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (tnt-blist-to-buffer tnt-buddy-blist
                           'tnt-buddy-list-filter)
      (set-buffer-modified-p nil))))

(defun tnt-buddy-list-filter (nick)
  (let* ((status (tnt-buddy-status nick))
         (unick (or status nick))
         (nnick (toc-normalize nick))
         (idle (tnt-buddy-idle nick))
         (away (tnt-buddy-away nick))
         (just-onoff (tnt-get-just-signedonoff nnick)))
    (if (or status just-onoff)
        (progn
          (put-text-property 0 (length unick)
                             'mouse-face 'highlight unick)
          (concat unick
                  (cond ((and away idle)
                         (format " (away - %s)" idle))
                        ((and away (not idle))
                         (format " (away)"))
                        ((and (not away) idle)
                         (format " (idle - %s)" idle))
                        (t ""))
                  just-onoff
                  )))))


(defun tnt-fetch-info ()
  "Requests the user info of a buddy (and launches browser with browse-url)."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (null (re-search-forward "^ +\\([^(\n]*\\)" nil t))
        (error "Position cursor on a buddy name")
      (toc-get-info (buffer-substring (match-beginning 1) (match-end 1))))))


(defun tnt-im-buddy ()
  "Initiates an IM conversation with the selected buddy."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (null (re-search-forward "^ +\\([^(\n]*\\)" nil t))
        (error "Position cursor on a buddy name")
      (let ((nick (buffer-substring (match-beginning 1) (match-end 1))))
        (if (tnt-buddy-status nick)
            (tnt-im nick)
          (error "Not online: %s" nick))))))

(defun tnt-im-buddy-mouse (event)
  "Initiates an IM conversation with the selected buddy by mouse click."
  (interactive "e")
  (mouse-set-point event)
  (tnt-im-buddy))

(defun tnt-next-buddy ()
  "Moves the cursor to the next buddy."
  (interactive)
  (beginning-of-line)
  (if (null (re-search-forward "\n " nil t))
      (error "No next buddy"))
  (goto-char (match-beginning 0))
  (forward-char))


(defun tnt-prev-buddy ()
  "Moves the cursor to the previous buddy."
  (interactive)
  (beginning-of-line)
  (if (null (re-search-backward "\n " nil t))
      (error "No previous buddy"))
  (goto-char (match-beginning 0))
  (forward-char))


(defun tnt-next-group ()
  "Moves the cursor to the first buddy of the next group."
  (interactive)
  (beginning-of-line)
  (if (null (re-search-forward "\n[^ ]" nil t))
      (error "No next group"))
  (tnt-next-buddy))


(defun tnt-prev-group ()
  "Moves the cursor to the last buddy of the previous group."
  (interactive)
  (beginning-of-line)
  (if (null (re-search-backward "\n[^ ]" nil t))
      (error "No previous buddy"))
  (goto-char (match-beginning 0))
  (tnt-prev-buddy))

(defun tnt-shutdown ()
  (tnt-set-online-state nil)
  (if tnt-keepalive-timer (cancel-timer tnt-keepalive-timer))
  (setq tnt-keepalive-timer nil)
  (if tnt-buddy-update-timer (cancel-timer tnt-buddy-update-timer))
  (setq tnt-buddy-update-timer nil)
  (if tnt-idle-timer (cancel-timer tnt-idle-timer))
  (setq tnt-idle-timer nil)
  (if tnt-unidle-timer (cancel-timer tnt-unidle-timer))
  (setq tnt-unidle-timer nil)

  (setq tnt-current-user nil
        tnt-buddy-alist nil
        tnt-buddy-blist nil
        tnt-permit-list nil
        tnt-deny-list nil
        tnt-permit-mode 1
        tnt-away-alist nil
        tnt-idle-alist nil
        tnt-just-signedonoff-alist nil
        tnt-pounce-alist nil
        tnt-away nil
        tnt-last-away-sent nil)
  (tnt-build-buddy-buffer))


(defun tnt-set-buddy-status (nick onlinep idle away)
  (let ((nnick (toc-normalize nick))
        (status (if onlinep nick))
        (idletime (if (and onlinep idle (> idle 0))
                      ;; see NOTE below about (current-time)
                      (- (cadr (current-time))
                         (* 60 idle))))
        (state (if onlinep "online" "offline")))
    (if (and (not tnt-login-flag)
             (not (string= status (tnt-buddy-status nick))))
        (progn
          (tnt-beep tnt-beep-on-buddy-signonoff)
          (let ((buffer (get-buffer (tnt-im-buffer-name nick))))
            (if buffer
                    (with-current-buffer buffer
                      (tnt-append-message (format "%s %s" nick state)))))

          (if tnt-message-on-buddy-signonoff 
              (message "%s %s" nick state))

          (if tnt-timers-available (tnt-set-just-signedonoff nnick onlinep))
          ))
    (setq tnt-buddy-alist (tnt-addassoc nnick status tnt-buddy-alist))
    (setq tnt-idle-alist (tnt-addassoc nnick idletime tnt-idle-alist))
    (setq tnt-away-alist (tnt-addassoc nnick away tnt-away-alist))

    (tnt-build-buddy-buffer)))

(defun tnt-buddy-status (nick)
  (cdr (assoc (toc-normalize nick) tnt-buddy-alist)))

(defun tnt-buddy-idle (nick)
  ;; NOTE: (current-time) doesn't actually give seconds since the
  ;; epoch, because elisp only allocates 28 bits for an integer (i
  ;; believe the remaining four bits are used to store what type that
  ;; word is storing, in this case, an int).  so current-time instead
  ;; gives a list containing the upper 16 bits and then the lower 16
  ;; bits.  so i'm just using the lower 16 bits, and assuming it won't
  ;; wrap around more than once.  which means that if someone is
  ;; actually idle for more than 65536 seconds (about 18 hours), then
  ;; it'll reset...
  (let ((idle-since (cdr (assoc (toc-normalize nick) tnt-idle-alist))))
    (if (null idle-since) nil
      (let* ((now (cadr (current-time)))
             (diff (- now idle-since))
             (idle-secs (if (< diff 0)
                               (+ diff 65536)
                             diff))
             (idle-mins (/ idle-secs 60)))
        (cond ((= 0 idle-mins) nil)
              ((< idle-mins 60) (format "%dm" idle-mins))
              (t (format "%dh%dm" (/ idle-mins 60) (mod idle-mins 60))))))))


(defun tnt-buddy-official-name (buddy)
  "Return official screen name of BUDDY if known, otherwise return BUDDY."
  (or (tnt-buddy-status buddy) buddy))

(defun tnt-online-buddies-collection ()
  ;; Return a "collection" of online buddies for completion commands.
  ;; (Remove all nil entries -- these turn up when a buddy logs off).
  (delete '(nil) (mapcar '(lambda(x) (list (cdr x))) tnt-buddy-alist)))

(defun tnt-online-buddies-and-groups-collection ()
  (append (mapcar (lambda(x) (list (car x))) tnt-buddy-blist)
          (tnt-online-buddies-collection)))

(defun tnt-set-just-signedonoff (nick onlinep)
  (let ((timestamp (cadr (current-time))))
    (setq tnt-just-signedonoff-alist
          (tnt-addassoc nick (list timestamp onlinep)
                        tnt-just-signedonoff-alist))
    (run-at-time tnt-just-signedonoff-delay nil
                 'tnt-unset-just-signedonoff nick timestamp)
    ))

(defun tnt-unset-just-signedonoff (nick timestamp-to-remove)
  (let ((most-recent (cadr (assoc nick tnt-just-signedonoff-alist))))
    (if (= most-recent timestamp-to-remove)
        (progn
          (setq tnt-just-signedonoff-alist
                (tnt-remassoc nick tnt-just-signedonoff-alist))
          (tnt-build-buddy-buffer)
          ))))

(defun tnt-get-just-signedonoff (nick)
  (let ((just-onoff (assoc nick tnt-just-signedonoff-alist)))
    (if (null just-onoff) nil
      (format " (just signed %s)"
              (if (caddr just-onoff) "on" "off")))))


(defun tnt-unset-login-flag ()
  (setq tnt-login-flag nil))



;;;----------------------------------------------------------------------------
;;; Buddy-list edit mode
;;;----------------------------------------------------------------------------

(defvar tnt-buddy-edit-mode-map nil)

(if tnt-buddy-edit-mode-map
    ()
  (setq tnt-buddy-edit-mode-map (make-sparse-keymap))
  (define-key tnt-buddy-edit-mode-map "\C-x\C-s" 'tnt-save-buddy-list)
)


(defun tnt-buddy-edit-mode ()
  "Major mode for editing a buddy list.
Special commands:
\\{tnt-buddy-edit-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map tnt-buddy-edit-mode-map)
  (setq mode-name "Buddy Edit")
  (setq major-mode 'tnt-buddy-edit-mode)
  (set-syntax-table text-mode-syntax-table)
  (run-hooks 'tnt-buddy-edit-mode-hook))


(defun tnt-edit-buddies ()
  "Shows the buddy-list editor in the selected window."
  (interactive)
  (switch-to-buffer (tnt-buddy-edit-buffer)))


(defconst tnt-buddy-edit-buffer-name "*edit-buddies*")

(defun tnt-buddy-edit-buffer ()
  (let ((buffer-name tnt-buddy-edit-buffer-name))
    (or (get-buffer buffer-name)
        (let ((buffer (get-buffer-create buffer-name)))
          (with-current-buffer buffer
            (tnt-buddy-edit-mode)
            ;; make-local-hook doesn't work here; tries to call t
            (make-local-variable 'kill-buffer-query-functions)
            (add-hook 'kill-buffer-query-functions 'tnt-buddy-edit-kill-query)
            (tnt-blist-to-buffer tnt-buddy-blist)
            (set-buffer-modified-p nil))
          buffer))))

(defun tnt-buddy-edit-kill-query ()
  (or (null (buffer-modified-p))
      (yes-or-no-p "Buddy list modified; kill anyway? ")))

(defun tnt-save-buddy-list ()
  "Saves a buddy-edit buffer on the host."
  (interactive)
  (if (null tnt-current-user)
      (error "You must be online to save a buddy list")
    (let* ((new-blist (tnt-buffer-to-blist))
           (old-blist tnt-buddy-blist)
           (new-list (tnt-extract-normalized-buddies new-blist))
           (old-list (tnt-extract-normalized-buddies old-blist))
           (diffs (tnt-sorted-list-diff old-list new-list)))
      (toc-add-buddies (cdr diffs))
      (toc-remove-buddies (car diffs))
      (toc-set-config (tnt-blist-to-config new-blist))
      (setq tnt-buddy-blist new-blist))
    (set-buffer-modified-p nil)
    (tnt-build-buddy-buffer)))

(defun tnt-restore-buddy-list-if-necessary ()
  "If buddy list is empty and backup file exists, restore."
  (interactive)
  (if (tnt-buddy-list-is-empty-p)
      (tnt-restore-buddy-list)
    tnt-buddy-blist))

(defun tnt-buddy-list-is-empty-p ()
  "Checks whether buddy list should be considered \"empty\"."
  (let ((num-groups (length tnt-buddy-blist)))
    (or (= num-groups 0)
        (and (= num-groups 1)
             (let ((len (length (car tnt-buddy-blist))))
               (or (<= len 1)
                   (and (= len 2)
                        (string= (caar tnt-buddy-blist) "Buddies")
                        (string= (toc-normalize tnt-username)
                                 (toc-normalize (cadar tnt-buddy-blist)))
                        )))
             ))))

(defun tnt-restore-buddy-list ()
  "Restores the buddy list from the backup file."
  (and tnt-buddy-list-backup-filename
       (let ((filename (format tnt-buddy-list-backup-filename
                               (toc-normalize tnt-username))))
         (if (file-readable-p filename)
             (progn
               (tnt-edit-buddies)
               (insert-file-contents filename nil nil nil t)
               (tnt-save-buddy-list)
               tnt-buddy-blist)))))

 

;;;----------------------------------------------------------------------------
;;; Buddy utilities
;;;----------------------------------------------------------------------------

(defun tnt-buffer-to-blist ()
  (save-excursion
    (goto-char (point-min))
    (let ((blist nil))
      (while (re-search-forward "\\([ \t]*\\)\\([^\n]*\\)\n" nil t)
        (let ((pref (buffer-substring (match-beginning 1) (match-end 1)))
              (body (buffer-substring (match-beginning 2) (match-end 2))))
          (goto-char (match-end 0))
          (let ((has-pref (> (length pref) 0))
                (has-body (string-match "[^ \t]" body)))
            (cond
             ((and has-body has-pref)                  ; is a buddy
              (setcar blist (cons body (car blist))))
             (has-body                                 ; is a group
              (setq blist (cons (list body) blist)))))))
      (mapcar 'nreverse (nreverse blist)))))


(defun tnt-blist-to-buffer (blist &optional filter)
  (while blist
    (let ((name-list (car blist)))
      (insert (car name-list) "\n")
      (setq name-list (cdr name-list))
      (while name-list
        (let* ((name (car name-list))
               (fname (if filter (funcall filter name) name)))
          (if fname
              (insert "  " fname "\n")))
        (setq name-list (cdr name-list)))
      (setq blist (cdr blist))
      (if blist (insert "\n")))))


(defun tnt-config-to-blist (config)
  (setq tnt-permit-list nil)
  (setq tnt-deny-list nil)
  (let ((index 0)
        (blist nil))
    (while (and config (string-match ". [^\n]*\n" config index))
      (let* ((beg (match-beginning 0))
             (end (match-end 0))
             (code (aref config beg))
             (arg (substring config (+ beg 2) (- end 1))))
        (cond
         ((= code ?g)
          (setq blist (cons (list arg) blist)))
         ((= code ?b)
          (setcar blist (cons arg (car blist))))
         ((= code ?p)
          (setq tnt-permit-list (cons arg tnt-permit-list)))
         ((= code ?d)
          (setq tnt-deny-list (cons arg tnt-deny-list)))
         ((= code ?m)
          (setq tnt-permit-mode (string-to-number arg))))
        (setq index end)))
    (mapcar 'nreverse (nreverse blist))))


(defun tnt-blist-to-config (blist)
  (concat (mapconcat '(lambda (name-list)
                        (concat "g " (car name-list) "\n"
                                (mapconcat '(lambda (b) (concat "b " b "\n"))
                                           (cdr name-list) ""))) blist "")
          (mapconcat '(lambda (d) (concat "d " d "\n")) tnt-deny-list "")
          (mapconcat '(lambda (p) (concat "p " p "\n")) tnt-permit-list "")
          (format "m %d\n" tnt-permit-mode)))


(defun tnt-extract-normalized-buddies (blist)
  (tnt-nsort-and-remove-dups (mapcar 'toc-normalize
                                     (apply 'append (mapcar 'cdr blist)))))



;;;----------------------------------------------------------------------------
;;; Pending-event ring
;;;----------------------------------------------------------------------------

(defvar tnt-event-ring nil)  ; (buffer-name . (message . callback))


(defun tnt-accept ()
  "Accepts an instant message or chat invitation."
  (interactive)
  (tnt-pop-event t))


(defun tnt-reject (warn)
  "Rejects an instant message or chat invitation; warns if prefix arg."
  (interactive "P")
  (tnt-pop-event nil))


(defun tnt-next-event ()
  "Shows the next event in the notification ring."
  (interactive)
  (setq tnt-event-ring (tnt-rotate-right tnt-event-ring))
  (tnt-show-top-event))


(defun tnt-prev-event ()
  "Show the previous event in the notification ring."
  (interactive)
  (setq tnt-event-ring (tnt-rotate-left tnt-event-ring))
  (tnt-show-top-event))


(defun tnt-push-event (message buffer-name function)
  ;; Push new event onto the event ring.
  (if (assoc buffer-name tnt-event-ring)
      ()
    (setq tnt-event-ring (cons (cons buffer-name (cons message function))
                               tnt-event-ring))
    (tnt-show-top-event)))


(defun tnt-pop-event (accept)
  ;; Remove the top event from the event ring.
  (if tnt-event-ring
      (let* ((event (car tnt-event-ring))
             (buffer-name (car event))
             (function (cdr (cdr event))))
        (setq tnt-event-ring (cdr tnt-event-ring))
        (if accept
            (switch-to-buffer buffer-name)
          (kill-buffer buffer-name))
        (if function (funcall function accept))
        (if tnt-event-ring
            (tnt-show-top-event)
          (tnt-persistent-message)))
    (message "No event to %s." (if accept "accept" "reject"))
    ))


(defun tnt-show-top-event ()
  ;; Display the message associated with the top event in the minibuffer.
  (if tnt-event-ring
      (let* ((event (car tnt-event-ring))
             (message (car (cdr event))))
        (tnt-persistent-message "%s ('%s' to accept) %s"
                                message
                                (substitute-command-keys "\\[tnt-accept]")
                                (let ((len (length tnt-event-ring)))
                                  (if (= len 1)
                                      ""
                                    (format "[%d more]" (1- len))))))))



;;;----------------------------------------------------------------------------
;;; Mode line
;;;----------------------------------------------------------------------------

(defvar tnt-mode-string "")

(defun tnt-set-online-state (is-online)
  ;; Sets or clears the mode-line online indicator.
  (setq tnt-mode-string (if is-online
                            (concat " ["
                                    (format "%s" tnt-current-user)
                                    (if tnt-away ":away" "")
                                    "]")
                          ""))
  (or global-mode-string
      (setq global-mode-string '("")))
  (or (memq 'tnt-mode-string global-mode-string)
      (setq global-mode-string (append global-mode-string '(tnt-mode-string))))
  (force-mode-line-update))

;;;----------------------------------------------------------------------------
;;; Handlers for TOC events
;;;----------------------------------------------------------------------------

(defun tnt-debug (&rest args)
  "Generic debugging handler.  Used to learn more."
  (message "Got a packet. Look in *tnt-debug* for info.")
  (let ((log-buffer (get-buffer-create "*tnt-debug*")))
    (prin1 args log-buffer)
    (princ "\n" log-buffer)))


(defun tnt-handle-opened ()
  (toc-signon tnt-login-host tnt-login-port tnt-username tnt-password
              tnt-language tnt-version))


(defun tnt-handle-closed ()
  (tnt-shutdown)
  (if (and tnt-email-to-pipe-to tnt-pipe-to-email-now)
      (tnt-pipe-message-to-program "TOC-server"
                                   "TNT connection closed by server"))
  (tnt-error "TNT connection closed")
  ;; auto-reconnect
  (setq tnt-reconnecting t)
  (message "Trying to reconnect...")
  (tnt-open tnt-username tnt-password)
  )

(defun tnt-handle-sign-on (version)
  (message "Signed on")
  (if tnt-use-keepalive
      (setq tnt-keepalive-timer
            (tnt-repeat tnt-keepalive-interval 'toc-keepalive)))
  (if tnt-use-buddy-update-timer
      (setq tnt-buddy-update-timer
            (tnt-repeat tnt-buddy-update-interval 'tnt-build-buddy-buffer)))
  (if tnt-timers-available
      (progn
        (setq tnt-login-flag t)
        (run-at-time tnt-login-flag-unset-after nil
                     'tnt-unset-login-flag)))
  (if tnt-use-idle-timer
      (progn
        (setq tnt-idle-timer (run-with-idle-timer tnt-send-idle-after t
                                                  'tnt-send-idle))
        (setq tnt-unidle-timer (run-with-idle-timer tnt-send-unidle-after t
                                                    'tnt-send-unidle))
    ))
  (toc-init-done))

(defun tnt-handle-config (config)
  (setq tnt-buddy-blist (tnt-config-to-blist config))
  (toc-add-buddies (tnt-extract-normalized-buddies tnt-buddy-blist))
  (cond ((= tnt-permit-mode 1) (toc-permit-all))
        ((= tnt-permit-mode 2) (toc-deny-all))
        ((= tnt-permit-mode 3) (toc-permit-only tnt-permit-list))
        ((= tnt-permit-mode 4) (toc-deny-only tnt-deny-list))
        (t (error "Bad permit mode %s" tnt-permit-mode))))

(defun tnt-handle-nick (nick)
  (setq tnt-current-user nick)
  (or (tnt-restore-buddy-list-if-necessary)
      (setq tnt-buddy-blist (list (list "Buddies" nick))))
  (tnt-set-online-state t)
  (if tnt-reconnecting
      (setq tnt-reconnecting nil)
    (tnt-show-buddies)))

(defun tnt-handle-im-in (user auto message)
  (let ((buffer (tnt-im-buffer user)))
    (tnt-append-message-and-adjust-window
     buffer message user (if auto "(Auto-response)"))

    (if (and tnt-email-to-pipe-to
             tnt-pipe-to-email-now)
        (tnt-pipe-message-to-program user message))
    
    (if (get-buffer-window buffer 'visible)

        (tnt-beep tnt-beep-on-message-in-visible-buffer)
      
      (tnt-beep tnt-beep-on-message-available-event)
      (tnt-push-event (format "Message from %s available" user)
                      (tnt-im-buffer-name user) nil))
    
    (if tnt-away (tnt-send-away-msg user))))

(defun tnt-toggle-email ()
  "Turns email piping on or off (only if tnt-email-to-pipe-to is set)."
  (interactive)
  (if (null tnt-email-to-pipe-to)
      (error "No email address set in variable tnt-email-to-pipe-to")
    (progn
      (setq tnt-pipe-to-email-now (not tnt-pipe-to-email-now))
      (if tnt-pipe-to-email-now
          (message (format "Now forwarding any incoming IMs to %s"
                           tnt-email-to-pipe-to))
        (message (format "No longer forwarding incoming IMs"))))))

(defun tnt-pipe-message-to-program (user message)
  (let ((proc-name "piping-process")
        (proc-out-buf "*piping-program-output*")
        (process-connection-type nil))
    ;; similar code to this could be used to pipe to something else
    (start-process proc-name proc-out-buf
                   ;; put executable here:
                   tnt-email-binary
                   ;; and now any cmd-line args:
                   ;; (note that although the subject string has spaces,
                   ;; it's all sent as one arg to the executable, i.e.
                   ;; one element of argv)
                   "-s"  ;; -s for subject
                   (format "IM from %s" user)  ;; a subject line
                   tnt-email-to-pipe-to)  ;; email address to send to
    ;; then what gets piped in
    (process-send-string proc-name
                         (format "%s: %s\n" user
                                 (tnt-strip-html message)))
    (process-send-eof proc-name))
  (message "Reminder: IMs are being forwarded to %s" tnt-email-to-pipe-to))


(defun tnt-handle-update-buddy (nick online evil signon idle away)
;; The modes for this section are listed in protocol, but here they are.
;; " U"  == Oscar Trial/Available.
;; " UU" == Oscar Trial/Away
;; " OU" == Oscar/Away
;; " O"  == Oscar/Available
;; " AU" == Aol/Oscar Trial/Here (the reason for U being here here is unknown)
;; " A"  == Aol/Here
  (if (or (string= away " UU")
          (string= away " OU")
          )
      (tnt-set-buddy-status nick online idle t)
    (tnt-set-buddy-status nick online idle nil))
  (if online
      (tnt-send-pounce (toc-normalize nick)))
  )

(defun tnt-handle-error (code args)
  (cond
   ((= code 901)
    (tnt-error "User %s not online" (car args)))
   ((= code 902)
    (tnt-error "Warning of %s is not allowed" (car args)))
   ((= code 903)
    (tnt-error "Message dropped - you are sending too fast"))
   ((= code 950)
    (tnt-error "Chat room %s is not available" (car args)))
   ((= code 960)
    (tnt-error "Message dropped - sending too fast for %s" (car args)))
   ((= code 961)
    (tnt-error "Message from %s dropped - too big" (car args)))
   ((= code 962)
    (tnt-error "Message from %s dropped - sent too fast" (car args)))
   (t (tnt-error "Unknown error %d:%S" code args))))

(defun tnt-handle-eviled (amount eviler)
  (message "You have been warned %s (%d)."
           (if (equal eviler "")
               "anonymously"
             (concat "by " eviler))
           amount))

(defun tnt-handle-chat-join (roomid room)
  (with-current-buffer (tnt-chat-buffer room)
    (setq tnt-chat-roomid roomid))
  (setq tnt-chat-alist (tnt-addassoc roomid room tnt-chat-alist)))

(defun tnt-handle-chat-in (roomid user whisperp message)
  (let ((buffer (tnt-chat-buffer (cdr (assoc roomid tnt-chat-alist)))))
    (tnt-append-message-and-adjust-window
     buffer message user (if whisperp "whispers"))))

(defun tnt-handle-chat-update-buddy (roomid inside users)
  (with-current-buffer (tnt-chat-buffer (cdr (assoc roomid tnt-chat-alist)))
    (let ((user-string (mapconcat 'identity users ", ")))
      (tnt-append-message (if tnt-chat-participants
                              (format "%s %s"
                                      user-string (if inside "joined" "left"))
                            (format "Participants: %s" user-string))))
    (if inside
        (setq tnt-chat-participants (append users tnt-chat-participants))
      (while users
        (let ((user (car users)))
          (setq tnt-chat-participants (delete user tnt-chat-participants))
          (setq users (cdr users)))))))


(defun tnt-handle-chat-invite (room roomid sender message)
  (tnt-handle-chat-join roomid room)    ; associate roomid with room
  (let ((buffer (tnt-chat-buffer room)))
    (with-current-buffer buffer
      (tnt-append-message (tnt-strip-html message) sender "invitation"))
    (tnt-push-event (format "Chat invitation from %s arrived" sender)
                    buffer 'tnt-chat-event-pop-function)
    (tnt-beep tnt-beep-on-message-available-event)
    ))

(defun tnt-handle-goto-url (windowid url)
  (setq url (concat "http://" tnt-toc-host "/" url))
  (browse-url url))

;;;----------------------------------------------------------------------------
;;; Minibuffer utilities
;;;----------------------------------------------------------------------------

(defun tnt-read-from-minibuffer-no-echo (prompt)
  ;; Reads a string from the minibuffer without echoing it.
  (let ((keymap (make-keymap))
        (i ? ))
    (while (<= i 126)
      (define-key keymap (char-to-string i)
        '(lambda ()
           (interactive)
           (insert last-command-char)
           (put-text-property (1- (point)) (point) 'invisible t)))
      (setq i (1+ i)))
    (define-key keymap "\r" 'exit-minibuffer)
    (define-key keymap "\C-g" 'keyboard-escape-quit)
    (let ((str (read-from-minibuffer prompt "" keymap)))
      (set-text-properties 0 (length str) nil str)
      str)))


(defun tnt-completing-read-list (prompt collection &optional initial-input)
  "Reads a list from the minibuffer with completion for each element
of the list, delimited by commas."
  (let* ((initial-input-str (mapconcat 'identity initial-input ", "))
         (str (completing-read prompt 'tnt-completion-func
                               nil nil initial-input-str)))
    (split-string str ",")))

(defun tnt-persistent-message (&optional fmt &rest args)
  ;; Displays a persistent message in the echo area.
  (with-current-buffer (get-buffer " *Minibuf-0*")
    (erase-buffer)
    (if fmt (insert (apply 'format fmt args)))
    (message nil)))


(defun tnt-error (&rest args)
  ;; Displays message in echo area and beeps.  Use this instead
  ;; of (error) for asynchronous errors.
  (apply 'message args)
  (tnt-beep tnt-beep-on-error))


(defvar collection) ; to shut up byte compiler

(defun tnt-completion-func (str pred flag)
  ;; Minibuffer completion function that allows lists of comma-separated
  ;; item to be entered, with completion applying to each item.  Before
  ;; calling, bind COLLECTION to the collection to be used for completion.
  (save-excursion
    (goto-char (point-min))
    (re-search-forward " *\\([^,]*\\)$"))
  (let ((first-part (buffer-substring (point-min) (match-beginning 1)))
        (last-word  (buffer-substring (match-beginning 1) (match-end 1))))
    (cond
     ((eq flag nil)
      (let ((completion (try-completion last-word collection pred)))
        (if (stringp completion) (concat first-part completion) completion)))
     ((eq flag t)
      (all-completions last-word collection pred)))))

(defun tnt-beep (beep-type)
  ;; beep-type      action
  ;; ---------      ------
  ;; nil            no beep
  ;; 'visible       visible beep
  ;; 'audible       audible beep
  ;; 'current       whichever emacs is currently set to
  (if beep-type
      (if (or (eq beep-type 'visible)
              (eq beep-type 'audible))
          (let ((orig-visible visible-bell))
            (setq visible-bell (eq beep-type 'visible))
            (beep)
            (setq visible-bell orig-visible))
        (beep))))




;;;----------------------------------------------------------------------------
;;; String list utilities
;;;----------------------------------------------------------------------------

(defun tnt-sorted-list-diff (old-list new-list)
  ;; Compares OLD-LIST and NEW-LIST.  Returns a cons of whose car is a
  ;; list of deletions and whose cdr is a list of insertions.
  (let ((insert-list nil)
        (delete-list nil))
    (while (or old-list new-list)
      (let ((old-item (car old-list))
            (new-item (car new-list)))
        (cond
         ((or (null new-item)
              (and old-item (string< old-item new-item)))
          (setq delete-list (cons old-item delete-list))
          (setq old-list (cdr old-list)))
         ((or (null old-item)
              (and new-item (string< new-item old-item)))
          (setq insert-list (cons new-item insert-list))
          (setq new-list (cdr new-list)))
         (t
          (setq new-list (cdr new-list))
          (setq old-list (cdr old-list))))))
    (cons delete-list insert-list)))


(defun tnt-nsort-and-remove-dups (list)
  ;; Sorts LIST into alphabetical order and removes duplicates.  Returns
  ;; the sorted list.  The original list is modified in the process.
  (setq list (sort list 'string<))
  (let ((p list))
    (while p
      (while (and (cdr p) (string= (car p) (car (cdr p))))
        (setcdr p (cdr (cdr p))))
      (setq p (cdr p))))
  list)



;;;----------------------------------------------------------------------------
;;; String utilities
;;;----------------------------------------------------------------------------

(defun tnt-strip-a-href (str)
  ;; Replaces the substring
  ;; <a href="http://www.derf.net/">derf!
  ;; with
  ;; ( http://www.derf.net/ ) derf!
  ;; which will not get stripped out by tnt-strip-html
  (let ((start-index 0)
        end-index the-url the-url-no-scheme
        (segs nil))
    (while (setq end-index (string-match "<a href=\"" str start-index))
      ;; skip past "<a href=\""
      (setq segs (cons (substring str start-index end-index) segs))
      (setq start-index (match-end 0))
      (setq end-index (string-match "\"" str start-index))
      ;; get stuff up to "\""
      (if (null end-index) nil
        (setq the-url (substring str start-index end-index))
        (setq start-index (match-end 0))
        (let ((first-seven (substring the-url 0 7)))
          (if (or (string= "http://" first-seven)
                  (string= "mailto:" first-seven))
              (setq the-url-no-scheme (substring the-url 7))))
        )
      ;; skip past ">"
      (setq end-index (string-match ">" str start-index))
      (if (null end-index) nil
        (setq start-index (match-end 0)))
      ;; if the link url is the same as the link text,
      ;; we don't need to see it twice
      (if (or (and the-url
                   (string-match the-url str start-index))
              (and the-url-no-scheme
                   (string-match the-url-no-scheme str start-index)))
          nil
        (setq segs (cons (format "( %s ) " the-url) segs)))
      )
    (setq segs (cons (substring str start-index) segs))
    (apply 'concat (nreverse segs))))

(defun tnt-strip-html (str)
  ;; Strips all HTML tags out of STR.
  (let ((start-index 0)
        end-index
        (segs nil))
    (setq str (tnt-strip-a-href str))
    (while (setq end-index (string-match "<[^ ][^>]*>" str start-index))
      (setq segs (cons (substring str start-index end-index) segs))
      (setq start-index (match-end 0)))
    (setq segs (cons (substring str start-index) segs))
    (apply 'concat (nreverse segs))))

(defun tnt-neliminate-newlines (str)
  ;; Converts newlines in STR to spaces.  Modifies STR.
  (let ((pos 0)
        (len (length str)))
    (while (< pos len)
      (if (= (aref str pos) ?\n)
          (aset str pos ? ))
      (setq pos (1+ pos)))
    str))


(defun tnt-repeat (interval function)
  (run-at-time interval interval function))


;;;----------------------------------------------------------------------------
;;; List utilities
;;;----------------------------------------------------------------------------
(defun tnt-rotate-left (l)
  "Moves the first element of L to the end, destructively."
  (if l (nconc (cdr l) (list (car l)))))

(defun tnt-rotate-right (l)
  "Moves the last element of L to the front destructively."
  (nreverse (tnt-rotate-left (nreverse l))))

(defun tnt-addassoc (key value alist)
  "Add an association between KEY and VALUE to ALIST and return the new alist."
  (let ((pair (assoc key alist)))
    (if (null pair)
        (cons (cons key value) alist)
      (setcdr pair value)
      alist)))

(defun tnt-remassoc (key alist)
  "Remove an association KEY from ALIST, and return the new ALIST."
  (delete (assoc key alist) alist))



;;;----------------------------------------------------------------------------
;;; testing emacs for the idle timers bug
;;;----------------------------------------------------------------------------

(defun tnt-test-for-buggy-idle ()
  "Runs a little test to see whether idle timers are handled correctly."
  (interactive)

  (switch-to-buffer "*tnt-test-idle-bug*")
  (insert "\n\n")
  
  (if (not (fboundp 'run-at-time))
      (insert "
This version of emacs does not have timers at all, let alone buggy
idle timers.
")

    (insert "
IMPORTANT: After running this test, some timers will have been set
which will make it really annoying to continue to use this emacs
process, so you'll want to exit this emacs process to get rid of them.
Therefore you should run the test in a nice new emacs process which
you won't mind exiting afterward.

Also, after you acknowledge that you have read the explanation of the
test, do not press any keys until the test finishes (it will take 3-5
seconds, and will tell you when it has finished).


")

    (if (not (y-or-n-p "Have you read the explanation of what this will do? "))
        (message "read it!")
      
      (message "")

      (defvar tnt-idle-bug-sent-string-p nil)
      (defvar tnt-idle-bug-idle-1-sec-times 0)
      (defvar tnt-idle-bug-idle-3-sec-run-p nil)
      
      (defun tnt-idle-bug-test-proc-filter (proc str) (insert str))
      
      (defun tnt-idle-bug-send-string ()
        (if tnt-idle-bug-sent-string-p
            (insert "...\n")
          (setq tnt-idle-bug-sent-string-p t)
          (process-send-string "test" "sending a string to process-filter\n")
          (process-send-eof "test")))
      
      (defun tnt-idle-bug-idle-1-sec ()
        (insert "idle 1 second\n")
        (setq tnt-idle-bug-idle-1-sec-times
              (+ 1 tnt-idle-bug-idle-1-sec-times))
        )
      
      (defun tnt-idle-bug-idle-3-sec ()
        (insert "idle 3 seconds\n")
        (if tnt-idle-bug-idle-3-sec-run-p
            nil
          (if (= tnt-idle-bug-idle-1-sec-times 2)
              (insert "\nSorry, but this emacs has the buggy idle handling.\n")
            (insert "
Congratulations, this emacs has correct idle handling.
Feel free to set tnt-buggy-idle to nil.
"))
          (setq tnt-idle-bug-idle-3-sec-run-p t)
          
          (insert "
With the buggy handler, the string being sent to the process-filter
resets the idle timers, so that it believes it has become unidle (and
then idle again).  So in the above test, it will say \"idle 1 second\"
twice -- once before the string is sent and once after.  The correct
idle handler will only say it before.

You should now exit this emacs process.
")
          )
        )
      
      (run-with-idle-timer 1 t 'tnt-idle-bug-idle-1-sec)
      (run-with-idle-timer 2 t 'tnt-idle-bug-send-string)
      (run-with-idle-timer 3 t 'tnt-idle-bug-idle-3-sec)
      
      (let ((test-proc (start-process "test" "*test*" "/bin/cat")))
        (set-process-filter test-proc 'tnt-idle-bug-test-proc-filter))

      )
    )
  )

(defun tnt-turn-on-debugging ()
  (interactive)
  (tnt-affect-debugging 'add-hook))

(defun tnt-turn-off-debugging ()
  (interactive)
  (tnt-affect-debugging 'remove-hook))

(defun tnt-affect-debugging (f)
  (funcall f 'toc-opened-hooks 'tnt-debug)
  (funcall f 'toc-closed-hooks 'tnt-debug)
  (funcall f 'toc-sign-on-hooks 'tnt-debug)
  (funcall f 'toc-config-hooks 'tnt-debug)
  (funcall f 'toc-nick-hooks 'tnt-debug)
  (funcall f 'toc-im-in-hooks 'tnt-debug)
  (funcall f 'toc-update-buddy-hooks 'tnt-debug)
  (funcall f 'toc-error-hooks 'tnt-debug)
  (funcall f 'toc-eviled-hooks 'tnt-debug)
  (funcall f 'toc-chat-join-hooks 'tnt-debug)
  (funcall f 'toc-chat-in-hooks 'tnt-debug)
  (funcall f 'toc-chat-update-buddy-hooks 'tnt-debug)
  (funcall f 'toc-chat-invite-hooks 'tnt-debug)
  (funcall f 'toc-chat-left-hooks 'tnt-debug)
  (funcall f 'toc-goto-url-hooks 'tnt-debug)
  (funcall f 'toc-pause-hooks 'tnt-debug))
