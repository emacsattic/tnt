; -*- indent-tabs-mode: nil -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TOCSTR
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
;;;;   drop connection on flap errors (framing, sequence)
;;;;   proxy support (socks, https)

(provide 'tocstr)

(defvar tocstr-process)
(defvar tocstr-flap-index)

;;;----------------------------------------------------------------------------
;;; Callback functions
;;;----------------------------------------------------------------------------

(defvar tocstr-opened-function  nil)
(defvar tocstr-closed-function  nil)
(defvar tocstr-receive-function nil)


;;;----------------------------------------------------------------------------
;;; Public functions
;;;----------------------------------------------------------------------------

(defvar tocstr-sname)

(defun tocstr-open (host port sname)
  (setq tocstr-sname sname)
  (tocstr-init-sender)
  (tocstr-init-receiver)
  (setq tocstr-process (open-network-stream "toc" nil host port))
  (set-process-filter tocstr-process 'tocstr-filter)
  (set-process-sentinel tocstr-process 'tocstr-sentinel)
  (process-send-string tocstr-process "FLAPON\r\n\r\n"))


(defun tocstr-close ()
  (delete-process tocstr-process)
  (setq tocstr-process nil))


(defun tocstr-send (string)
  (tocstr-send-flap 2 (format "%s%c" string 0)))



;;;----------------------------------------------------------------------------
;;; FLAP Sender
;;;----------------------------------------------------------------------------

(random t)
(defvar tocstr-seq-num 0)

(defvar tocstr-send-flap-function 'tocstr-send-flap1)
(if (string-match "\\([0-9]*\\)\\.\\([0-9]*\\)\\." emacs-version)
    (let ((major (string-to-number (substring emacs-version
                                              (match-beginning 1)
                                              (match-end 1))))
          (minor (string-to-number (substring emacs-version
                                              (match-beginning 2)
                                              (match-end 2)))))
      (if (and (= major 19) (< minor 30))
          (setq tocstr-send-flap-function 'tocstr-send-flap2))))


(defun tocstr-init-sender ()
  (setq tocstr-seq-num (random 65536)))


(defun tocstr-send-flap (type payload)
  (funcall tocstr-send-flap-function type payload))


(defun tocstr-send-flap1 (type payload)
  ;; This implementation is for 19.30 and later.
  (let ((len (length payload)))
    (setq tocstr-seq-num (logand (1+ tocstr-seq-num) 65535))
    (process-send-string tocstr-process
                         (format "*%c%c%c%c%c%s"
                                 type
                                 (lsh tocstr-seq-num -8)
                                 (logand tocstr-seq-num 255)
                                 (lsh len -8)
                                 (logand len 255)
                                 payload))))


(defun tocstr-send-flap2 (type payload)
  ;; This implementation is for 19.29 and earlier.  These versions had
  ;; a bug where %s trunctated the string at the first null. 
  (let ((len (length payload)))
    (setq tocstr-seq-num (logand (1+ tocstr-seq-num) 65535))
    (process-send-string tocstr-process
                         (concat (format "*%c%c%c%c%c"
                                         type
                                         (lsh tocstr-seq-num -8)
                                         (logand tocstr-seq-num 255)
                                         (lsh len -8)
                                         (logand len 255))
                                 payload))))



;;;----------------------------------------------------------------------------
;;; FLAP Receiver
;;;----------------------------------------------------------------------------

(defvar tocstr-sname      "")
(defvar tocstr-flap-state 'tocstr-flap-await-frame)
(defvar tocstr-flap-type  0)
(defvar tocstr-flap-size  0)
(defvar tocstr-flap-data  "")


(defun tocstr-init-receiver ()
  )


(defun tocstr-filter (proc str)
  (let ((len (length str))
        (i 0))
    (while (< i len)
      (funcall tocstr-flap-state (aref str i))
      (setq i (1+ i)))))


(defun tocstr-sentinel (proc str)
  (funcall tocstr-closed-function))


(defun tocstr-flap-await-frame (byte)
  (if (= byte ?*)
      (setq tocstr-flap-state 'tocstr-flap-await-type)))


(defun tocstr-flap-await-type (byte)
  (setq tocstr-flap-type byte)
  (setq tocstr-flap-state 'tocstr-flap-await-seq1))


(defun tocstr-flap-await-seq1 (byte)
  (setq tocstr-flap-state 'tocstr-flap-await-seq2))


(defun tocstr-flap-await-seq2 (byte)
  (setq tocstr-flap-state 'tocstr-flap-await-len1))


(defun tocstr-flap-await-len1 (byte)
  (setq tocstr-flap-size (lsh byte 8))
  (setq tocstr-flap-state 'tocstr-flap-await-len2))


(defun tocstr-flap-await-len2 (byte)
  (setq tocstr-flap-size (logior tocstr-flap-size byte))
  (setq tocstr-flap-data (make-string tocstr-flap-size 0))
  (setq tocstr-flap-index 0)
  (setq tocstr-flap-state 'tocstr-flap-collect-data))


(defun tocstr-flap-collect-data (byte)
  (aset tocstr-flap-data tocstr-flap-index byte)
  (if (< tocstr-flap-index (1- tocstr-flap-size))
      (setq tocstr-flap-index (1+ tocstr-flap-index))
    (cond
     ((= tocstr-flap-type 1)
      (tocstr-send-flap 1 (format "%c%c%c%c%c%c%c%c%s"
                                  0 0 0 1
                                  0 1
                                  0 (length tocstr-sname)
                                  tocstr-sname))
      (funcall tocstr-opened-function))
     ((= tocstr-flap-type 2)
      (funcall tocstr-receive-function tocstr-flap-data)))
    (setq tocstr-flap-state 'tocstr-flap-await-frame)))
