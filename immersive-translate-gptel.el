;;; immersive-translate-gptel.el --- gptel backend for immersive-translate -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Eli Qian

;; Author: Eli Qian <eli.q.qian@gmail.com>
;; URL: https://github.com/Elilif/emacs-immersive-translate
;; Keywords: convenience
;; Version: 0.3.0

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; gptel-based backend.

;;; Code:

(require 'subr-x)

(defvar immersive-translate-failed-message)

(defvar gptel-backend)
(defvar gptel-model)
(declare-function gptel-request "gptel-request" (&optional prompt &rest args))
(declare-function immersive-translate-callback "ext:immersive-translate" (response info))

(defgroup immersive-translate-gptel nil
  "Immersive translate gptel backend."
  :group 'immersive-translate)

(defcustom immersive-translate-gptel-system-prompt
  "You are a professional translator."
  "System prompt sent with each request."
  :group 'immersive-translate-gptel
  :type 'string)

(defcustom immersive-translate-gptel-user-prompt
  (concat "You will be provided with text delimited by triple backticks, "
          "your task is to translate the wrapped text into Chinese. "
          "You should only output the translated text.\n```%s```")
  "User prompt template used for gptel requests."
  :group 'immersive-translate-gptel
  :type 'string)

(defcustom immersive-translate-gptel-backend nil
  "gptel backend object used for translation.
When nil use `gptel-backend'."
  :group 'immersive-translate-gptel
  :type '(choice (const :tag "Use global gptel-backend" nil)
                 (sexp :tag "gptel backend")))

(defcustom immersive-translate-gptel-model nil
  "Optional model override for gptel integration.
When nil keep the current `gptel-model'."
  :group 'immersive-translate-gptel
  :type '(choice (const :tag "Use current gptel-model" nil)
                 (string :tag "Model")))

(defcustom immersive-translate-gptel-stream nil
  "Whether to stream gptel responses."
  :group 'immersive-translate-gptel
  :type 'boolean)

(defcustom immersive-translate-gptel-error-prefix "gptel request failed"
  "Prefix used when displaying gptel errors."
  :group 'immersive-translate-gptel
  :type 'string)

(defun immersive-translate-gptel-create-prompt (content)
  "Format CONTENT for gptel request."
  (format immersive-translate-gptel-user-prompt content))

(defun immersive-translate-gptel--ensure ()
  "Ensure gptel is available."
  (unless (fboundp 'gptel-request)
    (require 'gptel-request nil 'noerror))
  (unless (fboundp 'gptel-request)
    (user-error "gptel is not available")))

(defun immersive-translate-gptel--effective-backend ()
  "Return backend used for gptel requests."
  (or immersive-translate-gptel-backend
      (and (boundp 'gptel-backend) gptel-backend)
      (user-error "No gptel backend configured")))

(defun immersive-translate-gptel--effective-model ()
  "Return model used for gptel requests."
  (or immersive-translate-gptel-model
      (and (boundp 'gptel-model) gptel-model)))

(defun immersive-translate-gptel--error-response (status error)
  "Build error response string using STATUS and ERROR."
  (let ((summary (string-join
                  (delq nil (list immersive-translate-gptel-error-prefix status error))
                  ": ")))
    (propertize (string-trim (concat summary " " immersive-translate-failed-message))
                'error t)))

(defun immersive-translate-gptel-translate (info &optional callback)
  "Translate the content in INFO using gptel.

INFO is a plist with the following keys:
- :content (the text needed to be translated)
- :buffer (the current buffer)
- :position (marker at which to insert the response).

Call CALLBACK with the response and INFO afterwards.  If omitted
the response is inserted into the current buffer after point."
  (immersive-translate-gptel--ensure)
  (let* ((base-info (copy-sequence info))
         (buffer (or (plist-get base-info :buffer) (current-buffer)))
         (position (plist-get base-info :position))
         (prompt (let ((content (plist-get base-info :content)))
                   (if (stringp content)
                       content
                     (immersive-translate-gptel-create-prompt content))))
         (target-callback (or callback #'immersive-translate-callback))
         (backend (immersive-translate-gptel--effective-backend))
         (model (immersive-translate-gptel--effective-model)))
    (plist-put base-info :content prompt)
    (let ((gptel-backend backend)
          (gptel-model model))
      (gptel-request prompt
        :buffer buffer
        :position position
        :system immersive-translate-gptel-system-prompt
        :stream immersive-translate-gptel-stream
        :context base-info
        :callback
        (lambda (response request-info)
          (let* ((orig-info (or (plist-get request-info :context) base-info))
                 (final-info (copy-sequence orig-info))
                 (status (plist-get request-info :status))
                 (error-msg (plist-get request-info :error)))
            (when status (plist-put final-info :status status))
            (when error-msg (plist-put final-info :error error-msg))
            (plist-put final-info :gptel-backend backend)
            (when model (plist-put final-info :gptel-model model))
            (pcase response
              ((pred stringp)
               (funcall target-callback response final-info))
              ((or 'abort (pred null))
               (funcall target-callback
                        (immersive-translate-gptel--error-response status error-msg)
                        final-info))
              (`(reasoning . ,_)
               nil)
              (`(tool-call . ,_)
               nil)
              (`(tool-result . ,_)
               nil)
              (_ nil))))))))

(provide 'immersive-translate-gptel)
;;; immersive-translate-gptel.el ends here
