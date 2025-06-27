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

;; (defconst glbt-tap-strap-raw-uuid
;;   "6e400003-b5a3-f393-e0a9-e50e24dcca9e"
;;   "Tap Strap Raw Data characteristic")

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
(defvar tap-timer-mode nil)

(defun tapmacs--hook-discovery (path name)
  ""
  (message "discover %s %s" path name)
  (when (string-prefix-p "Tap" name)  ;; FIXME
    (setq tap-device path)
    (unless (glbt-is-paired-p path)
      (set-input-mode)
      (glbt-pair-device tap-device))))

(defun tap-battery (path percent)
  ""
  (message "bat %s %s" path percent)
  )

(setq tap-bindat-spec
      (bindat-type
        (tap      bits 1)
        (interval uint 16)))

(setq tap-level :chars)

(setq tap-stack nil)

(defun keycode-from (taps)
  ""
  (message "matching %s" taps)
  (pcase taps
    ('(0) "a")
    ('(1) "e")
    ('(2) "i")
    ('(3) "o")
    ('(4) "u")

    ('(0 1) "n")
    ('(1 2) "t")
    ('(2 3) "l")
    ('(3 4) "s")

    ('(0 2) "d")
    ('(0 3) "k")
    ('(0 4) "y")

    ('(1 3) "m")
    ('(1 4) "b")

    ;; ('(0 1 2) "shift")
    ;; ('(1 2 3) "bckspc")
    ;; ('(2 3 4) "num")

    ('(0 1 3) "f")
    ('(0 1 4) "p")

    ('(0 2 3) "g")
    ('(0 2 4) "p")

    ;; ('(0 3 4) "enter")
    ;; ('(1 3 4) "???")

    ('(1 2 4) "x")

    ('(0 1 2 3) "r")
    ('(1 2 3 4) "h")

    ;; ('(0 2 3 4) "???")
    ;; ('(0 1 3 4) "???")

    ('(0 1 2 3 4) " ")

    (_ tap-stack)
    ))

(defun keycodes-from-stack (stack)
  "returns one or more keycodes from the stack or nil if incomplete"
  (message "stack %s" stack)
  (keycode-from (car (last stack))))

(defun translate-taps (taps)
  (message "taps %s" taps)
  (if (eq tap-level :chars)
      (let* ((stack (append tap-stack (list taps)))
             (keycode (keycodes-from-stack stack)))
        (message "keycode %s" keycode)
        (if keycode
            (progn
              (message "chars %s" keycode)
              `(:cmd :char :keycode ,keycode))
          (progn
            (message "end stack %s" tap-stack)
            (setq tap-stack nil))))))

(defvar tap-riff-timer nil)

(defun tap-changed (path value)
  ""
  (when (string= path tap-data-path)
    (when tap-riff-timer
      (cancel-timer tap-riff-timer))
    (let* ((s (apply #'unibyte-string value))
           (taps (bindat-get-field (bindat-unpack tap-bindat-spec s) 'tap))
           (cmd (translate-taps taps)))
      (when (not cmd) (setq tap-riff-timer (run-with-timer 0.5 nil (lambda () (translate-taps nil)))))
      (message "test %s %s" taps cmd))))

(defun stop-controller-mode ()
  ""
  (interactive)
  (when tap-timer-mode
    (cancel-timer tap-timer-mode)
    (setq tap-timer-mode nil)))

(defun tap-device-changed (path event)
  ""
  (when (eq event :resolved)
    (message "resolved")
    (list-tap)
    (set-controller-mode))
  (when (eq event :disconnected)
    (message "disconnect")
    (run-with-idle-timer 0.1 nil #'stop-controller-mode)))

(defun connect-tap ()
  ""
  (interactive)
  (setq glbt-debug t)
  (add-hook 'glbt-discovery-complete-hook #'tapmacs--hook-discovery)
  (add-hook 'glbt-gatt-characteristic-changed-hook
            #'tap-changed)
  (add-hook 'glbt-battery-changed-hook  #'tap-battery)
  (add-hook 'glbt-device-changed-hook #'tap-device-changed)
  (glbt-start-discovery ;; (("UUIDs" . (,glbt-tap-strap-raw-uuid)))
   ))

(defun list-tap ()
  ""
  (interactive)
  (glbt-current-device-state tap-device)
  (let ((a (glbt-find-gatt-characteristic tap-device glbt-tap-strap-data-uuid)))
    (message "path %s" a)
    (setq tap-data-path a)
    (glbt-start-notify a))
  ;; (let ((a (glbt-find-gatt-characteristic tap-device glbt-tap-strap-raw-uuid)))
  ;;   (message "path2 %s" a)
  ;;   (setq tap-raw-path a)
  ;;   (glbt-start-notify a))
  )

(defun read-input-mode ()
  ""
  (interactive)
  (let ((a (glbt-gatt-characteristic-value tap-device glbt-tap-strap-mode-uuid)))
    (message "mode %s" a))
  )

(defconst tap-mode-text "\003\00C\000\000")
;; (defconst tap-mode-controller "\03\0C\00\01")
(defconst tap-mode-controller "\x3\xC\x0\x1")
(defconst tap-mode-controller-text "\003\00C\000\003")
(defconst tap-mode-raw "\003\00C\000\00A")

(defconst tap-type-mouse "\003\00D\000\001")
(defconst tap-type-kb "\003\00D\000\002")
(defconst tap-type-auto "\003\00D\000\003")

(defun set-input-mode ()
  ""
  (interactive)
  (let ((a (glbt-find-gatt-characteristic tap-device glbt-tap-strap-mode-uuid)))
    (glbt-call-method
     a
     glbt-gatt-characteristic-interface
     "WriteValue"
     (dbus-string-to-byte-array tap-mode-controller)
     '(:array :signature "{sv}"))))

(defun set-controller-mode ()
  ""
  (when tap-timer-mode (stop-controller-mode))
  (setq tap-timer-mode
        (run-with-timer
         0.1 5 #'set-input-mode)))

(defun tapmacs-mode
    ()
  )

(provide 'tapmacs-mode)

;;; tapmacs.el ends here
