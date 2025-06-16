;;; tapmacs.el ---  mode for using a TAP systems strap  -*- lexical-binding: t; -*-
;;; Commentary:
;; Since the basic kayboard mode is not very
;; suited to be used with control characters
;; this module implements control from scratch
;; over raw bluetooth instead of the HID interface.
;;; Code:

(require 'gl-bluetooth)
(require 'glbt-gatt)
(require 'bindat)

(defconst glbt-tap-strap-data-uuid
  "c3ff0005-1d8b-40fd-a56f-c7bd5d0f3370"
  "Tap Strap Data characteristic")

(defconst glbt-tap-strap-raw-uuid
  "6e400003-b5a3-f393-e0a9-e50e24dcca9e"
  "Tap Strap Raw Data characteristic")

(defconst glbt-tap-strap-mode-uuid
  "6e400002-b5a3-f393-e0a9-e50e24dcca9e"
  "Tap Strap Input Mode characteristic")

;; tap_service = 'c3ff0001-1d8b-40fd-a56f-c7bd5d0f3370'
;;    nus_service = '6e400001-b5a3-f393-e0a9-e50e24dcca9e'

;; (glbt-find-gatt-characteristic glbthr-hr-monitor-path
;;                                   glbthr-hr-measurement-uuid)
;; (add-hook 'glbt-gatt-characteristic-changed-hook
;; #'glbthr--handle-characteristic-changes)

;; (glbt-connect-device glbthr-hr-monitor-path)

(defvar tap-device nil)
(defvar tap-data-path nil)
(defvar tap-raw-path nil)

(defun tapmacs--hook-discovery (path name)
  ""
  (message "discover %s %s" path name)
  (setq tap-device path)
  (message "paried %s" (glbt-is-paired-p path))
  ;; (glbt-pair-device tap-device)
  ;; (glbt-connect-device tap-device)
  )

(defun tap-battery (path percent)
  ""
  (message "bat %s %s" path percent)
  )

(defun tap-changed (path value)
  ""
  (message "changed %s %s" path (string-to-multibyte (apply #'unibyte-string value)))
  )

(defun tap-device-changed (path event)
  ""
  (message "device %s %s" path event)
  (when (eq event :resolved)
    (message "resoved")
    )
  )

(defun connect-tap ()
  ""
  (interactive)
  (setq glbt-debug t)
  (add-hook 'glbt-discovery-complete-hook #'tapmacs--hook-discovery)
  (add-hook 'glbt-gatt-characteristic-changed-hook
            #'tap-changed)
  (add-hook 'glbt-battery-changed-hook  #'tap-battery)
  (add-hook 'glbt-device-changed-hook #'tap-device-changed)
  (glbt-start-discovery ;; `(("UUIDs" . (,glbt-tap-strap-uuid)))
   ))

(defun list-tap ()
  ""
  (interactive)
  (glbt-current-device-state tap-device)
  (let ((a (glbt-find-gatt-characteristic tap-device glbt-tap-strap-data-uuid)))
    (message "path %s" a)
    (setq tap-data-path a)
    (glbt-start-notify a))
  (let ((a (glbt-find-gatt-characteristic tap-device glbt-tap-strap-raw-uuid)))
    (message "path2 %s" a)
    (setq tap-raw-path a)
    (glbt-start-notify a)))

(defun read-input-mode ()
  ""
  (interactive)
  (let ((a (glbt-gatt-characteristic-value tap-device glbt-tap-strap-mode-uuid)))
    (message "mode %s" a))
  )

(defconst tap-mode-text "\003\00c\000\000")
(defconst tap-mode-controller "\003\00c\000\001")
(defconst tap-mode-controller-text "\003\00c\000\003")
(defconst tap-mode-raw "\003\00c\000\00a")

(defconst tap-type-mouse "\003\00d\000\001")
(defconst tap-type-kb "\003\00d\000\002")
(defconst tap-type-auto "\003\00d\000\003")

(defun set-input-mode ()
  ""
  (interactive)
  (let ((a (glbt-find-gatt-characteristic tap-device glbt-tap-strap-mode-uuid)))
    (message "path3 %s" a)
    (glbt-call-method
     a
     glbt-gatt-characteristic-interface
     "WriteValue"
     (dbus-string-to-byte-array tap-type-mouse)
     '(:array :signature "{sv}")))
  )

(defun tapmacs-mode
    ()
  )

(provide 'tapmacs-mode)

;;; tapmacs.el ends here
