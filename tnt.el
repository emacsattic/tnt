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
;;;;   mouse mappings

(provide 'tnt)
(require 'toc)

(defconst tnt-version "TNT 2.1")


;;; Config variables

(defvar tnt-toc-host    "toc.oscar.aol.com")
(defvar tnt-toc-port    5190)
(defvar tnt-login-host  "login.oscar.aol.com")
(defvar tnt-login-port  5190)
(defvar tnt-language    "english")

(defvar tnt-default-username nil)
(defvar tnt-default-username-list nil)
(defvar tnt-default-password nil)

(defvar tnt-show-timestamps nil
  "*If t, show timestamps in TNT conversations.")

(defvar tnt-recenter-windows t
  "*If t, recenters text to bottom of window when messages are printed.")

(defvar tnt-inhibit-key-bindings nil)


;;; Key bindings

(if tnt-inhibit-key-bindings
    ()
  (global-set-key "\C-xt?" 'tnt-show-help)
  (global-set-key "\C-xto" 'tnt-open)
  (global-set-key "\C-xtk" 'tnt-kill)
  (global-set-key "\C-xti" 'tnt-im)
  (global-set-key "\C-xtj" 'tnt-join-chat)
  (global-set-key "\C-xtb" 'tnt-show-buddies)
  (global-set-key "\C-xta" 'tnt-accept)
  (global-set-key "\C-xtr" 'tnt-reject)
  (global-set-key "\C-xtp" 'tnt-prev-event)
  (global-set-key "\C-xtn" 'tnt-next-event)
  (global-set-key "\C-xtB" 'tnt-edit-buddies)
  (global-set-key "\C-xts" 'tnt-switch-user)
  (global-set-key "\C-xtA" 'tnt-away-toggle)
  (global-set-key "\C-xtP" 'tnt-pounce-add)
  (global-set-key "\C-xtD" 'tnt-pounce-del)
)


;;; Globals

(defvar tnt-current-user    nil)


;;;---------------------------------------------------------------------------
;;;  Pounce Package - jnwhiteh@syr.edu
;;;---------------------------------------------------------------------------

(defvar tnt-pounce-list nil)


;;; Pounce Code
(defun pounce-rm-from-sequence (nick sequence result)
  (if (string= (car (car sequence)) nick)
      (if (eq nil result)
	  (setq tnt-pounce-list (cdr sequence))
	(setq tnt-pounce-list (list result (car (cdr sequence)))))
    (pounce-rm-from-sequence nick (cdr sequence) (car sequence))))

(defun tnt-pounce-add ()
  "Allows a user to store a pounce msg for a buddy"
  (interactive)
  (let* ((completion-ignore-case t)
	 (nick_tmp (format "%s" (completing-read "Buddy to Pounce on: " 
 						 (tnt-buddy-collection))))
 	 (nick (toc-normalize nick_tmp))
 	 (msg_tmp (format "%s" 
			  (read-from-minibuffer "Msg to send (enter for none): ")))
 	 (msg (if (string= msg_tmp "") "none" msg_tmp))
 	 (pair (assoc nick tnt-pounce-list)))
    
    ;; They use (cons (cons )) list
    
    (if pair (setcdr pair msg)
      (setq tnt-pounce-list (cons (list nick msg) tnt-pounce-list)))
    (message (format "%s has been added to your pounce list" nick))
    )
  )

(defun tnt-pounce-del ()
  "Let's a user delete a stored pounce msg"
  (interactive)
  (let* ((completion-ignore-case t)
	 (nick (format "%s" (completing-read "Delete pounce for user: "
 					     (tnt-buddy-collection)))))
    (tnt-pounce-delete
     (toc-normalize nick)))
  )


(defun tnt-pounce-delete (nick)
  (let* ((pair (assoc nick tnt-pounce-list)))
    (if pair
	  (pounce-rm-from-sequence nick tnt-pounce-list nil))
    (if pair
	   (message "The pounce for %s has been deleted." nick)
      (message "There is no pounce stored for %s" nick)))
  )

(defun tnt-send-pounce (user)
   (let* ((pair (assoc user tnt-pounce-list)))
     (if pair
	 (let ((buffer (tnt-im-buffer user)))
 	  (toc-send-im user (car (cdr pair)))
 	  (tnt-append-message-and-adjust-window buffer tnt-current-user (car (cdr pair)))
 	  (tnt-push-event (format "You have pounced on %s" user)
			  buffer nil)
 	  (tnt-pounce-delete user))
       )))

;;;---------------------------------------------------------------------------
;;;  Keepalive/Away Packages - jnwhiteh@syr.edu
;;;---------------------------------------------------------------------------
(defvar tnt-keepalive-interval 60)
(defvar tnt-last-away-sent nil)
(defvar tnt-away-msg nil)
(defvar tnt-away-alist nil)
(defvar tnt-away 0)
(defvar tnt-keepalive-timer nil)
(defvar tnt-use-keepalive nil 
  "*If t, sends a keepalive packet once a minute")

(defun tnt-keepalive ()
  "Sends a keepalive packet to the server"
  (interactive)
  (tocstr-send-flap1 5 "")  
  (setq tnt-keepalive-timer (run-at-time tnt-keepalive-interval nil 
					 'tnt-keepalive))
)

(defun tnt-buddy-away (nick)
  (let* ((nnick (toc-normalize nick))
         (pair (assoc nnick tnt-away-alist)))
    (if pair (cdr pair))))

(defun tnt-away-toggle ()
  "Toggles away or not, and sets message."
  (interactive)
  (if (eq tnt-away 0) (tnt-get-away-msg)
      (eq tnt-away 1) (tnt-not-away)
      )     
)

(defun tnt-not-away ()
  "Sets you as NOT away."
  (interactive)
  (setq tnt-away 0)
  (setq tnt-last-away-sent nil)
  (message "You have returned.")
  (tocstr-send (format "toc_set_away"))
  (tnt-set-online-state t)
)

;; gse: Added history and default away message stuff.
(defvar tnt-away-msg-history nil)
(defun tnt-get-away-msg ()
  "Gets the away msg"
  (interactive)
  (setq tnt-away 1)
  (setq tnt-away-msg (read-from-minibuffer "Away Message: "
                                           (cons
                                            (if tnt-away-msg-history
                                                (car tnt-away-msg-history)
                                              "I'm away.")
                                            0)
                                           nil nil 'tnt-away-msg-history))
  (message "You are away: %s" tnt-away-msg)
  (message "Set as away: %s" tnt-away-msg)
  (tocstr-send (format "toc_set_away %s" (toc-encode tnt-away-msg)))
  (tnt-set-online-state t)
)

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
                           (read-from-minibuffer "Screen name: "))
          tnt-password (or (and (stringp password) password)
                           tnt-default-password
                           (tnt-read-from-minibuffer-no-echo 
                            (format "Password for %s: " tnt-username))))
    (setq toc-opened-function            'tnt-handle-opened
          toc-closed-function            'tnt-handle-closed
          toc-sign-on-function           'tnt-handle-sign-on
          toc-config-function            'tnt-handle-config
          toc-nick-function              'tnt-handle-nick
          toc-update-buddy-function      'tnt-handle-update-buddy
          toc-im-in-function             'tnt-handle-im-in
          toc-chat-join-function         'tnt-handle-chat-join
          toc-chat-in-function           'tnt-handle-chat-in
          toc-chat-invite-function       'tnt-handle-chat-invite
          toc-chat-update-buddy-function 'tnt-handle-chat-update-buddy
          toc-error-function             'tnt-handle-error)
    (toc-open tnt-toc-host tnt-toc-port tnt-username)))


(defun tnt-kill ()
  "Ends the current TNT session and signs off from the host."
  (interactive)
  (if (null tnt-current-user)
      (error "Already offline")
    ;; gse addition: turn off "away" setting
    (setq tnt-away 0)
    (toc-close)
    (tnt-set-online-state nil)
    (setq tnt-current-user nil)
    (tnt-buddy-shutdown)
    (message "Signed off")))



;;;----------------------------------------------------------------------------
;;; Instant message mode
;;;----------------------------------------------------------------------------

(defvar tnt-im-mode-syntax-table nil)
(defvar tnt-im-mode-abbrev-table nil)
(defvar tnt-im-mode-map nil)
(defvar tnt-im-user)
(defvar tnt-message-marker)

(make-variable-buffer-local 'tnt-im-user)


(define-abbrev-table 'tnt-im-mode-abbrev-table ())

(if tnt-im-mode-syntax-table
    ()
  (setq tnt-im-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\" ".   " tnt-im-mode-syntax-table)
  (modify-syntax-entry ?\\ ".   " tnt-im-mode-syntax-table)
  (modify-syntax-entry ?'  "w   " tnt-im-mode-syntax-table))

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
  (setq local-abbrev-table tnt-im-mode-abbrev-table)
  (set-syntax-table tnt-im-mode-syntax-table)
  (auto-fill-mode)
  (run-hooks 'tnt-im-mode-hook))


(defun tnt-im (user)
  "Opens an instant-message conversation with a user."
  (interactive "p")
  (let* ((completion-ignore-case t)
         (input (or (and (stringp user) user)
                    (completing-read "Send IM to: " (tnt-buddy-collection)))))
    (switch-to-buffer (tnt-im-buffer input))))


(defun tnt-im-buffer-name (user)
  ;; Returns the name of the IM buffer for USER.
  (format "*im-%s*" (toc-normalize user)))


(defun tnt-im-buffer (user)
  ;; Returns the IM buffer for USER.
  (let ((buffer-name (tnt-im-buffer-name user)))
    (or (get-buffer buffer-name)
        (let ((buffer (get-buffer-create buffer-name)))
          (save-excursion
            (set-buffer buffer)
            (tnt-im-mode)
            (setq tnt-im-user user)
            (setq tnt-message-marker (make-marker))
            (insert (format "[Conversation with %s on %s]\n\n"
                            (tnt-buddy-official-name user)
                            (current-time-string)))
            (set-marker tnt-message-marker (point)))
          buffer))))


(defun tnt-send-text-as-instant-message ()
  "Sends text at end of buffer as an IM."
  (interactive)
  (let* ((message (tnt-get-input-message)))
    (if (string= message "") (message "Please enter a message to send")
      (tnt-append-message tnt-current-user message))
    (if (eq tnt-away 1) (message "Reminder: You are still set as away") ())
    (if tnt-recenter-windows (recenter -1))
    (if (string= message "") () (toc-send-im tnt-im-user message))))


;;;---------------------------------------------------------------------------
;;;  Show help
;;;---------------------------------------------------------------------------

(defun tnt-show-help ()
  "Displays help for TNT."
  (interactive)
  (let ((buffer-name "*tnt-help*"))
    (or (get-buffer buffer-name)
	(let ((buffer (get-buffer-create buffer-name)))
	  (save-excursion
	    (set-buffer buffer)
	    (insert "
+------------------+-------------+-------------------------------------------+
|  Function        | Key Binding |               Summary                     |
+------------------+-------------+-------------------------------------------+
| tnt-show-help    |   C-x t ?   | Displays this help information            |
| tnt-open         |   C-x t o   | Starts a new TNT session                  |
| tnt-kill         |   C-x t k   | Terminates the current session            |
| tnt-im           |   C-x t i   | Starts an instant-message conversation    |
| tnt-join-chat    |   C-x t j   | Joins a chat room                         |
| tnt-show-buddies |   C-x t b   | Shows the buddy list                      |
| tnt-edit-buddies |   C-x t B   | Invokes the buddy list editor             |
| tnt-accept       |   C-x t a   | Accepts a message or a chat invitation    |
| tnt-reject       |   C-x t r   | Rejects a message or a chat invitation    |
| tnt-next-event   |   C-x t n   | Shows next event in notification ring     |
| tnt-prev-event   |   C-x t p   | Shows previous event in notification ring |
| tnt-switch-user  |   C-x t s   | Switch between usernames for next login   |
| tnt-away-toggle  |   C-x t A   | This allows you to toggle your away status|
| tnt-pounce-add   |   C-x t P   | Add a user to your pounce list            |
| tnt-pounce-del   |   C-x t D   | Remove a user from your pounce list       |
+------------------+-------------+-------------------------------------------+
"))
	  (apply (tnt-switch-to-buffer-function) (list buffer))))))



;;;----------------------------------------------------------------------------
;;; Chat mode
;;;----------------------------------------------------------------------------

(defvar tnt-chat-mode-syntax-table nil)
(defvar tnt-chat-mode-abbrev-table nil)
(defvar tnt-chat-mode-map nil)
(defvar tnt-chat-alist nil)         ; room id to room name

(defvar tnt-chat-room)
(defvar tnt-chat-roomid)
(defvar tnt-chat-participants)

(make-variable-buffer-local 'tnt-chat-room)
(make-variable-buffer-local 'tnt-chat-roomid)
(make-variable-buffer-local 'tnt-chat-participants)


(define-abbrev-table 'tnt-chat-mode-abbrev-table ())

(if tnt-chat-mode-syntax-table
    ()
  (setq tnt-chat-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\" ".   " tnt-chat-mode-syntax-table)
  (modify-syntax-entry ?\\ ".   " tnt-chat-mode-syntax-table)
  (modify-syntax-entry ?'  "w   " tnt-chat-mode-syntax-table))

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
  (setq local-abbrev-table tnt-chat-mode-abbrev-table)
  (set-syntax-table tnt-chat-mode-syntax-table)
  (auto-fill-mode)
  (run-hooks 'tnt-chat-mode-hook))


(defun tnt-join-chat (room)
  "Joins a chat room."
  (interactive "p")
  (if (null tnt-current-user)
      (error "You must be online to join a chat room.")
    (let* ((input (or (and (stringp room) room)
                      (read-from-minibuffer "Join chat room: "
                                            (format "%s Chat%03d"
                                                    tnt-current-user
                                                    (random 1000))))))
      (toc-chat-join 4 input)
      (switch-to-buffer (tnt-chat-buffer input)))))


(defun tnt-chat-buffer-name (room)
  ;; Returns the name of the chat buffer for ROOM.
  (format "*chat-%s*" (toc-normalize room)))


(defun tnt-chat-buffer (room)
  ;; Returns the chat buffer for ROOM.
  (let ((buffer-name (tnt-chat-buffer-name room)))
    (or (get-buffer buffer-name)
        (let ((buffer (get-buffer-create buffer-name)))
          (save-excursion
            (set-buffer buffer)
            (tnt-chat-mode)
            (make-local-hook 'kill-buffer-hook)
            (add-hook 'kill-buffer-hook 'tnt-chat-buffer-killed nil t)
            (setq tnt-chat-room room)
            (setq tnt-chat-participants nil)
            (setq tnt-message-marker (make-marker))
            (insert (format "[Chat room \"%s\" on %s]\n\n"
                            room
                            (current-time-string)))
            (set-marker tnt-message-marker (point)))
          buffer))))


(defun tnt-chat-buffer-killed ()
  (if tnt-current-user
      (toc-chat-leave tnt-chat-roomid)))


(defun tnt-send-text-as-chat-message ()
  (interactive)
  (let ((message (tnt-get-input-message)))
    (toc-chat-send tnt-chat-roomid message)))


(defun tnt-send-text-as-chat-whisper (user)
  (interactive "p")
  (let ((user (or (and (stringp user) user)
                  (completing-read "Whisper to user: "
                                   (tnt-participant-collection))))
        (message (tnt-get-input-message)))
    (if (= (length message) 0)
        (setq message (read-from-minibuffer "Message: ")))
    (tnt-append-message (format "%s (whispers to %s)"
                                tnt-current-user
                                (tnt-buddy-official-name user))
                        message)
    (if tnt-recenter-windows (recenter -1))
    (toc-chat-whisper tnt-chat-roomid user message)))


(defun tnt-participant-collection ()
  (mapcar '(lambda(x) (list x)) tnt-chat-participants))


(defun tnt-send-text-as-chat-invitation (users)
  (interactive "p")
  (let ((user-list (or (and (listp users) users)
                       (tnt-completing-read-list "Users to invite: "
                                                 (tnt-buddy-collection)))))
    (if user-list
        (let ((msg (tnt-get-input-message)))
          (if (= (length msg) 0)
              (setq msg (read-from-minibuffer "Message: "
                                              "Join me in this Buddy Chat.")))
          (tnt-append-message (format "%s (invites %s)"
                                      tnt-current-user
                                      (mapconcat 'tnt-buddy-official-name
                                                 user-list ", "))
                              msg)
          (if tnt-recenter-windows (recenter -1))
          (apply 'toc-chat-invite tnt-chat-roomid msg user-list)))))


(defun tnt-show-chat-participants ()
  "Append a list of chat room participants to a chat buffer."
  (interactive)
  (let ((string (mapconcat '(lambda (x) x) tnt-chat-participants ", ")))
    (tnt-append-message nil (format "Participants: %s" string))))


(defun tnt-chat-event-pop-function (accept)
  ;; Called when chat event is popped.  If event is accepted, the
  ;; current buffer is the chat buffer.
  (if accept
      (toc-chat-accept tnt-chat-roomid)))



;;;----------------------------------------------------------------------------
;;; Utilites for the messaging modes (im, chat)
;;;----------------------------------------------------------------------------

(make-variable-buffer-local 'tnt-message-marker)

(defun tnt-append-message-and-adjust-window (buffer user message)
  (let ((window (get-buffer-window buffer)))
    (save-excursion
      (set-buffer buffer)
      (tnt-append-message user (tnt-strip-html message))
      (if window
          (let ((old-window (selected-window)))
            (select-window window)
            (if tnt-recenter-windows (recenter -1))
            (select-window old-window))))))


;; Should this be done with defface?  I don't know what the current
;; way of doing this is, but I think this method will work on both
;; FSF and XEmacs.
(make-face 'tnt-other-name-face)
(make-face 'tnt-my-name-face)
(set-face-foreground 'tnt-other-name-face "blue")
(set-face-foreground 'tnt-my-name-face "red")


;; gse: Added the above faces and modded tnt-append-message to
;;      change the color of the 'user' text when inserting it.
;; gse todo: Add timestamps to messages.
(defun tnt-append-message (user message)
  ;; Prepends USER to MESSAGE and appends the result to the buffer.
  (save-excursion
    (let ((old-point (marker-position tnt-message-marker)))
      (goto-char tnt-message-marker)

       (if user
		   (progn
			(if tnt-show-timestamps
				(insert-before-markers (format-time-string "%T ")))
			 
			(let ((start (point)))
			  (insert-before-markers (format "%s:" user))
			  ;; Change color of user text.
			  (if (string-equal user tnt-current-user)
				  (add-text-properties start (point) '(face tnt-my-name-face))
				(add-text-properties start (point) '(face tnt-other-name-face)))
			  (insert-before-markers (format " %s\n\n" message))))
			(insert-before-markers (format "[%s]\n\n" message)))
		 (fill-region old-point (point)))))


(defun tnt-get-input-message ()
  (let ((message (buffer-substring tnt-message-marker (point-max))))
    ;; gse: This used to be kill-region, but that hosed the kill ring.
    ;;      Which was very annoying.
    (delete-region tnt-message-marker (point-max))
    (goto-char (point-max))
    (if tnt-recenter-windows (recenter -1))
    (tnt-neliminate-newlines message)))


;;;----------------------------------------------------------------------------
;;; Buddy list mode
;;;----------------------------------------------------------------------------

(defvar tnt-buddy-list-mode-syntax-table nil)
(defvar tnt-buddy-list-mode-abbrev-table nil)
(defvar tnt-buddy-list-mode-map nil)

(defvar tnt-buddy-blist nil)
(defvar tnt-buddy-alist nil)
(defvar tnt-idle-alist nil)
(defvar tnt-away-alist nil)

(define-abbrev-table 'tnt-buddy-list-mode-abbrev-table ())


(if tnt-buddy-list-mode-syntax-table
    ()
  (setq tnt-buddy-list-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\" ".   " tnt-buddy-list-mode-syntax-table)
  (modify-syntax-entry ?\\ ".   " tnt-buddy-list-mode-syntax-table)
  (modify-syntax-entry ?'  "w   " tnt-buddy-list-mode-syntax-table))


(if tnt-buddy-list-mode-map
    ()
  (setq tnt-buddy-list-mode-map (make-sparse-keymap))
  (define-key tnt-buddy-list-mode-map "n" 'tnt-next-buddy)
  (define-key tnt-buddy-list-mode-map "p" 'tnt-prev-buddy)
  (define-key tnt-buddy-list-mode-map "N" 'tnt-next-group)
  (define-key tnt-buddy-list-mode-map "P" 'tnt-prev-group)
  (define-key tnt-buddy-list-mode-map "i" 'tnt-im-buddy)
  (define-key tnt-buddy-list-mode-map "\C-m" 'tnt-im-buddy)
  (define-key tnt-buddy-list-mode-map [mouse-2] 'tnt-im-buddy-mouse))


(defun tnt-buddy-list-mode ()
  "Major mode for viewing a buddy list.
Special commands:
\\{tnt-buddy-list-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map tnt-buddy-list-mode-map)
  (setq mode-name "Buddy List")
  (setq major-mode 'tnt-buddy-list-mode)
  (setq local-abbrev-table tnt-buddy-list-mode-abbrev-table)
  (set-syntax-table tnt-buddy-list-mode-syntax-table)
  (run-hooks 'tnt-buddy-list-mode-hook))


(defun tnt-show-buddies ()
  "Shows the buddy list in the selected window."
  (interactive)
  (tnt-build-buddy-buffer)
  (apply (tnt-switch-to-buffer-function) (list (tnt-buddy-buffer))))

(defvar tnt-use-split-buddy nil 
  "*If t, splits screen automagically when you invoke buddy-view")

(defun tnt-switch-to-buffer-function ()
 (if (and (not (string-equal (buffer-name) "*scratch*"))
	  (eq tnt-use-split-buddy t))
    #'switch-to-buffer-other-window 
      #'switch-to-buffer))

(defun tnt-buddy-buffer ()
  (let ((buffer-name "*buddies*"))
    (or (get-buffer buffer-name)
        (let ((buffer (get-buffer-create buffer-name)))
          (save-excursion
            (set-buffer buffer)
            (tnt-buddy-list-mode)
            (setq buffer-read-only t))
          buffer))))


(defun tnt-build-buddy-buffer ()
  (save-excursion
    (set-buffer (tnt-buddy-buffer))
      (let ((buffer-read-only nil))
        (erase-buffer)
        (tnt-blist-to-buffer tnt-buddy-blist
                             '(lambda (nick)
                                 (let ((unick (tnt-buddy-status nick))
				       (idle (tnt-buddy-idle nick))
				       (away (tnt-buddy-away nick)))
                                   (if unick (format "  %s%s%s" 
	    unick 
	    (if (eq away 1) 
		(if (> idle 0) (format " (away - %d)" idle)
		  (format " (away)")) "")
		  
	    (if (> idle 0)
		(if (eq away 0) 
		    (format " (idle %d)" idle) "") "" )
	    )))))

        (set-buffer-modified-p nil))))

(defun tnt-im-buddy ()
  "Initiates an IM conversation with the selected buddy."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (null (re-search-forward "^ +\\([^(\n]*\\)" nil t))
        (error "Position cursor on a buddy name")
      (tnt-im (buffer-substring (match-beginning 1) (match-end 1))))))

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


(defun tnt-initialize-buddy-list (config)
  (setq tnt-buddy-blist (tnt-config-to-blist config))
  (let ((buddies (tnt-extract-normalized-buddies tnt-buddy-blist)))
    (apply 'toc-add-buddy buddies)))


(defun tnt-buddy-shutdown ()
  (setq tnt-buddy-blist nil
        tnt-buddy-alist nil
	tnt-away-alist nil
	tnt-idle-alist nil
	tnt-pounce-list nil)

  (tnt-build-buddy-buffer))


(defun tnt-set-buddy-status (nick onlinep idle away)
  (let* ((nnick (toc-normalize nick))
         (pair (assoc nnick tnt-buddy-alist))
		 (pair2 (assoc nnick tnt-idle-alist))
		 (pair3 (assoc nnick tnt-away-alist))
         (status (if onlinep nick))	 
		 (awayflag (if away away))
		 (idletime (if onlinep idle))
         (old-status (and pair (cdr pair)))
		 (old-away (and pair (cdr pair3)))
		 (old-idle (and pair2 (cdr pair2))))
    (if pair
		(setcdr pair status)
      (setq tnt-buddy-alist (cons (cons nnick status)
								  tnt-buddy-alist)))
    (if pair2
		(setcdr pair2 idletime)
      (setq tnt-idle-alist (cons (cons nnick idletime) tnt-idle-alist)))

    (if pair3
		(setcdr pair3 awayflag)
      (setq tnt-away-alist (cons (cons nnick awayflag) tnt-away-alist)))

    ;; Online or not?
    (if (not (string= status old-status))
        (progn
          ;; Message.
          ;; I think I prefer vanilla messages to tnt-events for this,
          ;; but just in case, here's the code for a tnt-event:
          ;;   (tnt-push-event (format "%s online" nick) nil nil)
          (if onlinep
              (message (format "%s online" nick))
            (message (format "%s offline" nick)))))
	
	(tnt-build-buddy-buffer)))

(defun tnt-buddy-status (nick)
  (let* ((nnick (toc-normalize nick))
         (pair (assoc nnick tnt-buddy-alist)))
    (if pair (cdr pair))))

(defun tnt-buddy-idle (nick)
  (let* ((nnick (toc-normalize nick))
         (pair (assoc nnick tnt-idle-alist)))
    (if pair (cdr pair))))

(defun tnt-buddy-official-name (buddy)
  ;; Return official screen name of buddy if known, otherwise
  ;; just return buddy.
  (or (tnt-buddy-status buddy) buddy))

(defun tnt-buddy-collection ()
  ;; Return a "collection" of online buddies for completion commands.
  ;; (Remove all nil entries -- these turn up when a buddy logs off).
  (delete '(nil) (mapcar '(lambda(x) (list (cdr x))) tnt-buddy-alist)))


;;;----------------------------------------------------------------------------
;;; Buddy-list edit mode
;;;----------------------------------------------------------------------------

(defvar tnt-buddy-edit-mode-syntax-table nil)
(defvar tnt-buddy-edit-mode-abbrev-table nil)
(defvar tnt-buddy-edit-mode-map nil)

(define-abbrev-table 'tnt-buddy-edit-mode-abbrev-table ())


(if tnt-buddy-edit-mode-syntax-table
    ()
  (setq tnt-buddy-edit-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\" ".   " tnt-buddy-edit-mode-syntax-table)
  (modify-syntax-entry ?\\ ".   " tnt-buddy-edit-mode-syntax-table)
  (modify-syntax-entry ?'  "w   " tnt-buddy-edit-mode-syntax-table))


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
  (setq local-abbrev-table tnt-buddy-edit-mode-abbrev-table)
  (set-syntax-table tnt-buddy-edit-mode-syntax-table)
  (run-hooks 'tnt-buddy-edit-mode-hook))


(defun tnt-edit-buddies ()
  "Shows the buddy-list editor editor in the selected window."
  (interactive)
  (switch-to-buffer (tnt-buddy-edit-buffer)))


(defconst tnt-buddy-edit-buffer-name "*edit-buddies*")

(defun tnt-buddy-edit-buffer ()
  (let ((buffer-name tnt-buddy-edit-buffer-name))
    (or (get-buffer buffer-name)
        (let ((buffer (get-buffer-create buffer-name)))
          (save-excursion
            (set-buffer buffer)
            (tnt-buddy-edit-mode)
            ;; make-local-hook doesn't work here; tries to call t
            (make-local-variable 'kill-buffer-query-functions)
            (add-hook 'kill-buffer-query-functions 'tnt-buddy-edit-kill-query)
            (tnt-build-buddy-edit-buffer)
            (set-buffer-modified-p nil))
          buffer))))


(defun tnt-build-buddy-edit-buffer ()
  (save-excursion
    (set-buffer (tnt-buddy-edit-buffer))
    (erase-buffer)
    (tnt-blist-to-buffer tnt-buddy-blist)))


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
      (if (cdr diffs)
          (apply 'toc-add-buddy (cdr diffs)))
      (if (car diffs)
          (apply 'toc-remove-buddy (car diffs)))
      (toc-set-config (tnt-blist-to-config new-blist))
      (setq tnt-buddy-blist new-blist))
    (set-buffer-modified-p nil)
    (tnt-build-buddy-buffer)))



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
      (insert (format "%s\n" (car name-list)))
      (setq name-list (cdr name-list))
      (while name-list
        (let* ((name (car name-list))
               (fname (if filter (funcall filter name) (format "  %s" name))))
          (if fname
              (insert (format "%s\n" fname))))
        (setq name-list (cdr name-list)))
      (setq blist (cdr blist))
      (if blist (insert "\n")))))


(defun tnt-config-to-blist (config)
  (let ((index 0)
        (blist nil))
    (while (and config
                (string-match ". [^\n]*\n" config index))
      (let* ((beg (match-beginning 0))
             (end (match-end 0))
             (code (aref config beg))
             (arg (substring config (+ beg 2) (- end 1))))
        (cond
         ((= code ?g)
          (setq blist (cons (list arg) blist)))
         ((= code ?b)
          (setcar blist (cons arg (car blist)))))
        (setq index end)))
    (mapcar 'nreverse (nreverse blist))))


(defun tnt-blist-to-config (blist)
  (let ((config ""))
    (while blist
      (let ((name-list (car blist)))
        (setq config (format "%sg %s\n" config (car name-list)))
        (setq name-list (cdr name-list))
        (while name-list
          (setq config (format "%sb %s\n" config (car name-list)))
          (setq name-list (cdr name-list)))
        (setq blist (cdr blist))))
    config))
  

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
          (tnt-persistent-message)))))


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
				    (if (eq 1 tnt-away)
					":away"
				      "")
				    "]")
			  ""))
  (or global-mode-string
      (setq global-mode-string '("")))
  (or (memq 'tnt-mode-string global-mode-string)
      (setq global-mode-string (append global-mode-string '(tnt-mode-string))))
  (force-mode-line-update))

(defun send-away-msg (user)
  (if (not (string= user tnt-last-away-sent))
      (let ((buffer (tnt-im-buffer user)))
	(setq tnt-last-away-sent user)
	(toc-send-im user tnt-away-msg 1)
	(tnt-append-message-and-adjust-window buffer
					      (format "%s (Auto-response)"
						      tnt-current-user)
					      tnt-away-msg)
	))
)

;;;----------------------------------------------------------------------------
;;; Handlers for TOC events
;;;----------------------------------------------------------------------------

(defun tnt-handle-opened ()
  (toc-signon tnt-login-host tnt-login-port tnt-username tnt-password
              tnt-language tnt-version))


(defun tnt-handle-closed ()
  (tnt-set-online-state nil)
  (setq tnt-current-user nil)
  (tnt-buddy-shutdown)
  (if tnt-use-keepalive 
      (cancel-timer tnt-keepalive-timer))
  (tnt-error "TNT connection closed"))


(defun tnt-handle-sign-on (version)
  (message "Signed on")
  (tnt-show-buddies)
  (if tnt-use-keepalive
      (run-at-time tnt-keepalive-interval nil 'tnt-keepalive))
  (toc-init-done))


(defun tnt-handle-config (config)
  (tnt-initialize-buddy-list config))


(defun tnt-handle-nick (nick)
  (setq tnt-current-user nick)
  (or tnt-buddy-blist
      (setq tnt-buddy-blist (list (list "Buddies" nick))))
  (tnt-set-online-state t))

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
      (tnt-set-buddy-status nick online idle 1)
    (tnt-set-buddy-status nick online idle 0))
  (if online
      (tnt-send-pounce (toc-normalize nick)))
  )
  

(defun tnt-handle-im-in (user auto message)
  (let ((buffer (tnt-im-buffer user)))
    (if auto
    (tnt-append-message-and-adjust-window buffer 
					  (format "%s (Auto-response)" user)
					  message)
    (tnt-append-message-and-adjust-window buffer user message))
    ;; to pipe all incoming messages somewhere, modify the function
    ;; below to correctly call the program you want to pipe into,
    ;; then uncomment this next line:
    ;(tnt-pipe-message-to-program user message)
    ;(beep)
    (if (null (get-buffer-window buffer))
	(progn
	  (beep)
	  (tnt-push-event (format "Message from %s available" user)
			  (tnt-im-buffer-name user) nil)))
    (if (eq tnt-away 1) (send-away-msg user))))


(defun tnt-pipe-message-to-program (user message)
  (let ((proc-name "piping-process")
	(proc-out-buf "*piping-program-output*")
	(process-connection-type nil))
    ;; this is a sample process to pipe to -- sending an email
    (start-process proc-name proc-out-buf
                 ;; put executable here:
		   "/usr/bin/mail"
                 ;; and now any cmd-line args:
                 ;; (note that although the subject string has spaces,
                 ;; it's all sent as one arg to the executable, i.e.
                 ;; one element of argv)
		   "-s"  ;; -s for subject
		   (format "IM from %s" user)  ;; a subject line
		   "foo@bar.com")  ;; an email address to send to
    (process-send-string proc-name 
			 (format "%s: %s\n" user 
				 (tnt-strip-html message)))
    (process-send-eof proc-name)))
  



(defun tnt-handle-chat-join (roomid room)
  (let ((buffer (tnt-chat-buffer room)))
    (save-excursion
      (set-buffer buffer)
      (setq tnt-chat-roomid roomid)))
  (let ((assoc (assoc roomid tnt-chat-alist)))
    (if assoc
        ()
      (setq tnt-chat-alist (cons (cons roomid room) tnt-chat-alist)))))


(defun tnt-handle-chat-in (roomid user whisperp message)
  (let ((buffer (tnt-chat-buffer (cdr (assoc roomid tnt-chat-alist)))))
    (tnt-append-message-and-adjust-window buffer
                                          (if whisperp
                                              (format "%s (whispers)" user)
                                            user)
                                          message)))


(defun tnt-handle-chat-invite (room roomid sender message)
  (tnt-handle-chat-join roomid room)    ; associate roomid with room
  (let ((buffer (tnt-chat-buffer room)))
    (save-excursion
      (set-buffer buffer)
      (tnt-append-message (format "%s (invitation)" sender)
                          (tnt-strip-html message)))
    (tnt-push-event (format "Chat invitation from %s arrived" sender)
                    buffer 'tnt-chat-event-pop-function)
    (beep)))


(defun tnt-handle-chat-update-buddy (roomid inside users)
  (save-excursion
    (set-buffer (tnt-chat-buffer (cdr (assoc roomid tnt-chat-alist))))
    (let ((user-string (mapconcat '(lambda (x) x) users ", ")))
      (tnt-append-message nil (if tnt-chat-participants
                                  (format "%s %s"
                                          user-string
                                          (if inside "joined" "left"))
                                (format "Participants: %s" user-string))))
    (if inside
        (setq tnt-chat-participants (append users tnt-chat-participants))
      (while users
        (let ((user (car users)))
          (setq tnt-chat-participants (delete user tnt-chat-participants))
          (setq users (cdr users)))))))
                                               
  
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
    (tnt-error "Message from %s dropped - sent too fast" (car args)))))



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
    (let ((str (read-from-minibuffer prompt "" keymap)))
      (set-text-properties 0 (length str) nil str)
      str)))


(defun tnt-completing-read-list (prompt collection)
  ;; Reads a list from the minibuffer with completion.
  (let ((str (let ((collection collection))
               (completing-read prompt 'tnt-completion-func)))
        (index 0)
        (list nil))
    (while (and (< index (length str))
                (string-match "\\([^,]*\\),?" str index))
      (setq list (cons (substring str (match-beginning 1) (match-end 1)) list))
      (setq index (match-end 0)))
    (nreverse list)))

    
(defun tnt-persistent-message (&optional fmt &rest args)
  ;; Displays a persistent message in the echo area.
  (save-excursion
    (set-buffer (get-buffer " *Minibuf-0*"))
    (erase-buffer)
    (if fmt (insert (apply 'format fmt args)))
    (message nil)))


(defun tnt-error (&rest args)
  ;; Displays message in echo area and beeps.  Use this instead
  ;; of (error) for asynchronous errors.
  (apply 'message args)
  (beep))

  
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
  ;; replaces the substring
  ;; <a href="http://www.derf.net/">derf!
  ;; with
  ;; ( http://www.derf.net/ ) derf!
  ;; which will not get stripped out by tnt-strip-html
  (let ((start-index 0)
        end-index
        (segs nil))
    (while (setq end-index (string-match "<a href=\"" str start-index))
      (setq segs (cons (substring str start-index end-index) segs))
      (setq start-index (match-end 0))
      (setq end-index (string-match "\"" str start-index))
      (if (null end-index) nil
        (setq segs (cons "( " segs))
        (setq segs (cons (substring str start-index end-index) segs))
        (setq segs (cons " ) " segs))
        (setq start-index (match-end 0)))
      (setq end-index (string-match ">" str start-index))
      (if (null end-index) nil
        (setq start-index (match-end 0))))
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



;;;----------------------------------------------------------------------------
;;; List utilities
;;;----------------------------------------------------------------------------

(defun tnt-rotate-left (list)
  ;; Rotates LIST left.
  (cond (list
         (setcdr list (nreverse (cdr list)))
         (nreverse list))))


(defun tnt-rotate-right (list)
  ;; Rotates LIST right.
  (nreverse (tnt-rotate-left (nreverse list))))


;; not sure where in the file to put this, so i stuck it at the end
(defun tnt-switch-user ()
  "Switches the default username to log in as."
  (interactive)
  (if (null tnt-default-username-list)
      (message "No username list defined.")
      (progn
	(setq tnt-default-username-list 
	      (tnt-rotate-left tnt-default-username-list))
	(setq tnt-default-username (car tnt-default-username-list))
	(message 
	 (format "Next login will be as user %s" tnt-default-username)))))
