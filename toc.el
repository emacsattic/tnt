;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TOC
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
;;;;   turn callbacks into hooks
;;;;   signon time overflows 24 bits


(provide 'toc)
(require 'tocstr)



;;;----------------------------------------------------------------------------
;;; Callback functions
;;;----------------------------------------------------------------------------

(defvar toc-opened-function            nil)
(defvar toc-closed-function            nil)
(defvar toc-sign-on-function           nil)
(defvar toc-config-function            nil)
(defvar toc-nick-function              nil)
(defvar toc-update-buddy-function      nil)
(defvar toc-im-in-function             nil)
(defvar toc-chat-join-function         nil)
(defvar toc-chat-in-function           nil)
(defvar toc-chat-invite-function       nil)
(defvar toc-chat-update-buddy-function nil)
(defvar toc-error-function             nil)



;;;----------------------------------------------------------------------------
;;; Public functions
;;;----------------------------------------------------------------------------

(defun toc-open (host port sname)
  (setq tocstr-opened-function  'toc-handle-opened
        tocstr-closed-function  'toc-handle-closed
        tocstr-receive-function 'toc-handle-receive)
  (tocstr-open host port sname))


(defun toc-close ()
  (tocstr-close))


(defun toc-signon (host port username password language version)
  (tocstr-send (format "toc_signon %s %d %s %s %s %s"
                       host
                       port
                       (toc-normalize username)
                       (toc-roast password)
                       language
                       (toc-encode version))))


(defun toc-init-done ()
  (tocstr-send "toc_init_done"))


(defun toc-set-config (config)
  (tocstr-send (format "toc_set_config %s"
                       (toc-encode config))))

(defun toc-set-away (message)
  (tocstr-send (format "toc_set-away %s"
	               (toc-encode message))))


(defun toc-send-im (user message &optional auto)
  (tocstr-send (format "toc_send_im %s %s%s"
                       (toc-normalize user)
                       (toc-encode message)
                       (if auto " auto" ""))))


(defun toc-add-buddy (&rest buddies)
  (tocstr-send (format "toc_add_buddy %s"
                       (substring (format "%S" buddies) 1 -1))))


(defun toc-remove-buddy (&rest buddies)
  (tocstr-send (format "toc_remove_buddy %s"
                       (substring (format "%S" buddies) 1 -1))))


(defun toc-chat-join (exchange room)
  (tocstr-send (format "toc_chat_join %d %s"
                       exchange
                       (toc-encode room))))


(defun toc-chat-send (roomid message)
  (tocstr-send (format "toc_chat_send %s %s"
                       roomid
                       (toc-encode message))))


(defun toc-chat-whisper (roomid user message)
  (tocstr-send (format "toc_chat_whisper %s %s %s"
                       roomid
                       user
                       (toc-encode message))))


(defun toc-chat-accept (roomid)
  (tocstr-send (format "toc_chat_accept %s"
                       roomid)))


(defun toc-chat-leave (roomid)
  (tocstr-send (format "toc_chat_leave %s"
                       roomid)))


(defun toc-chat-invite (roomid message &rest buddies)
  (tocstr-send (format "toc_chat_invite %s %s %s"
                       roomid
                       (toc-encode message)
                       (mapconcat 'toc-normalize buddies " "))))
                       



;;;----------------------------------------------------------------------------
;;; Handlers for tocstr events
;;;----------------------------------------------------------------------------

(defun toc-handle-opened ()
  (funcall toc-opened-function))


(defun toc-handle-closed ()
  (funcall toc-closed-function))


(defun toc-handle-receive (str)
  (let* ((index 0)
         (cmd (toc-lop-field str 'index)))
    (cond
     ((string= cmd "SIGN_ON")
      (let ((version (toc-lop-field str 'index)))
        (funcall toc-sign-on-function version)))

     ((string= cmd "CONFIG")
      (let ((config (toc-lop-field str 'index)))
        (funcall toc-config-function config)))

     ((string= cmd "NICK")
      (let ((nick   (toc-lop-field str 'index)))
        (funcall toc-nick-function nick)))

     ((string= cmd "UPDATE_BUDDY")
      (let ((nick   (toc-lop-field str 'index))
            (online (string= "T" (toc-lop-field str 'index)))
            (evil   (string-to-number (toc-lop-field str 'index)))
            (signon (string-to-number (toc-lop-field str 'index)))
            (idle   (string-to-number (toc-lop-field str 'index)))
	    (away (toc-lop-field str 'index)))
        (funcall toc-update-buddy-function nick online evil signon idle away)))

     ((string= cmd "IM_IN")
      (let ((user    (toc-lop-field str 'index))
            (auto    (string= "T" (toc-lop-field str 'index)))
            (message (substring str index)))
        (funcall toc-im-in-function user auto message)))

     ((string= cmd "CHAT_JOIN")
      (let ((roomid  (toc-lop-field str 'index))
            (room    (toc-lop-field str 'index)))
        (funcall toc-chat-join-function roomid room)))

     ((string= cmd "CHAT_IN")
      (let ((roomid  (toc-lop-field str 'index))
            (user    (toc-lop-field str 'index))
            (whisper (string= "T" (toc-lop-field str 'index)))
            (message (substring str index)))
        (funcall toc-chat-in-function roomid user whisper message)))

     ((string= cmd "CHAT_INVITE")
      (let ((room    (toc-lop-field str 'index))
            (roomid  (toc-lop-field str 'index))
            (sender  (toc-lop-field str 'index))
            (message (substring str index)))
        (funcall toc-chat-invite-function room roomid sender message)))

     ((string= cmd "CHAT_UPDATE_BUDDY")
      (let ((roomid  (toc-lop-field str 'index))
            (inside  (string= "T" (toc-lop-field str 'index)))
            (users   (let (user (users nil))
                       (while (setq user (toc-lop-field str 'index))
                         (setq users (cons user users)))
                       users)))
        (funcall toc-chat-update-buddy-function roomid inside users)))

     ((string= cmd "ERROR")
      (let ((code   (string-to-number (toc-lop-field str 'index)))
            (args   nil)
            (arg    nil))
        (while (setq arg (toc-lop-field str 'index))
          (setq args (cons arg args)))
        (funcall toc-error-function code (nreverse args)))))))



;;;----------------------------------------------------------------------------
;;; String utilities
;;;----------------------------------------------------------------------------

(defun toc-lop-field (str index-var)
  ;; Returns the substring of STR that starts at the index given by the
  ;; value of INDEX-VAR and ends at the next colon.  Updates INDEX-VAR
  ;; to index of the character after colon.
  (let ((start-index (eval index-var)))
    (if (< start-index (length str))
        (let ((colon-index (or (string-match ":" str start-index)
                               (length str))))
          (set index-var (1+ colon-index))
          (substring str start-index colon-index)))))


(defun toc-roast (str)
  ;; Obfuscates password STR for network transmission.
  (let* ((roaster "Tic/Toc")
         (rstr "0x")
         (slen (length str))
         (rlen (length roaster))
         (i 0))
    (while (< i slen)
      (setq rstr (concat rstr
                         (format "%02x" (logxor (aref str i)
                                                (aref roaster (% i rlen))))))
      (setq i (1+ i)))
    rstr))


(defun toc-encode (str)
  ;; Encloses STR in quotes and backslashes special characters in it.
  (let ((list nil)
        (pos 0))
    (while (string-match "\\([^][{}()\\'\"$]*\\)\\([][{}()\\'\"$]\\)" str pos)
      (setq list (cons (substring str (match-beginning 1) (match-end 1)) list))
      (setq list (cons "\\" list))
      (setq list (cons (substring str (match-beginning 2) (match-end 2)) list))
      (setq pos (match-end 0)))
    (if (< pos (length str))
        (setq list (cons (substring str pos) list)))
    (apply 'concat "\"" (nreverse (cons "\"" list)))))


(defun toc-normalize (str)
  ;; Removes spaces and smashes STR to lowercase.
  (let* ((len (length str))
         (len2 len))
    (let ((index 0))
      (while (< index len)
        (if (= (aref str index) ? )
            (setq len2 (1- len2)))
        (setq index (1+ index))))
    (let ((str2 (make-string len2 0))
          (index 0)
          (index2 0))
      (while (< index len)
        (if (= (aref str index) ? )
            ()
          (aset str2 index2 (downcase (aref str index)))
          (setq index2 (1+ index2)))
        (setq index (1+ index)))
      str2)))
