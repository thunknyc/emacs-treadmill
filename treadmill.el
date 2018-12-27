;; -*- lexical-binding t -*-

;; (setq gp (treadmill-start-server))
;; (treadmill-eval gp '(import :thunknyc/ioenv))
;; (treadmill-eval gp '(begin/string-io "(+ 1 2)" (displayln (eval (read)))))
;; (treadmill-eval gp '(map 1+ (iota 10)))
;; (treadmill-eval gp '(list + 1 2))
;; (treadmill-clean-up gp)

(defconst treadmill-interpreter-path "/Users/edw/dev/gerbil/bin/gxi")
(defconst treadmill-runner-script
  "/Users/edw/dev/emacs-treadmill/treadmill.ss")

(defvar-local treadmill-port nil)
(defvar-local treadmill-repl-process nil)
(defvar-local treadmill-repl-awaiting-value nil)

(defun treadmill-command ()
  (list treadmill-interpreter-path treadmill-runner-script))

(defun treadmill-repl-filter (p s)
  ;; Boilerplate
  (when (buffer-live-p (process-buffer p))
    (with-current-buffer (process-buffer p)
      (let ((moving (= (point) (process-mark p))))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark p))
          (insert s)
          (set-marker (process-mark p) (point)))
        (if moving (goto-char (process-mark p))))))
  ;; Not boilerplate
  (with-current-buffer (process-buffer p)
    (when treadmill-repl-awaiting-value
      (save-excursion
        (goto-char 0)
        (when (search-forward-regexp "\\(\\(.+\\)\r\n\\|\\)[0-9]*> $" nil t)
          (let ((result (match-string 1)))
            (if (zerop (length result))
                (message "=> No value")
              (message "=> %S" (match-string 2))))
          (setq treadmill-repl-awaiting-value nil))))))

(defun treadmill-process-filter (p s)
  ;; Boilerplate
  (when (buffer-live-p (process-buffer p))
    (with-current-buffer (process-buffer p)
      (let ((moving (= (point) (process-mark p))))
        (save-excursion
          ;; Insert the text, advancing the process marker.
          (goto-char (process-mark p))
          (insert s)
          (set-marker (process-mark p) (point)))
        (if moving (goto-char (process-mark p))))))
  ;; Not boilerplate
  (with-current-buffer (process-buffer p)
    (when (null treadmill-port)
      (save-excursion
        (goto-char 0)
        (when (search-forward-regexp "Running net repl on port \\([0-9]+\\)."
                                     nil t)
          (let ((port (string-to-number (match-string 1))))
            (setq treadmill-port port)
            (message "Net repl starting on port %d." port)
            (treadmill-connect-to-repl port)))))))

(defun treadmill-connect-to-repl (port)
  (let* ((repl-b (generate-new-buffer "*treadmill-repl*"))
         (repl-p (open-network-stream "treadmill-repl"
                                      repl-b "127.0.0.1"
                                      port)))
    (set-process-filter repl-p 'treadmill-repl-filter)
    (setq treadmill-repl-process repl-p)
    (message "Repl process is `%s'." repl-p)
    (message "Connected to repl on port %d." port)))


(defun treadmill-start-server ()
  (let* ((b (generate-new-buffer "*treadmill-process*"))
         (p (make-process :name "treadmill" :buffer b :coding 'utf-8
                          :type 'pipe :command (treadmill-command)
                          :filter 'treadmill-process-filter)))
    (message "Started `%s' in `%s'" p b)
    p))

(defun treadmill-clean-up (p)
  (let* ((repl-p (treadmill-repl-process p))
         (repl-b (process-buffer repl-p))
         (n (process-name p))
         (b (process-buffer p)))
    (delete-process repl-p)
    (kill-buffer repl-b)
    (delete-process p)
    (kill-buffer b)
    (message "Deleted process `%s' and associated buffers." n)))

(defun treadmill-repl-process (p)
  (with-current-buffer (process-buffer p)
    treadmill-repl-process))

(defun treadmill-repl-buffer (p)
  (with-current-buffer (process-buffer p)
    (process-buffer treadmill-repl-process)))

(defun treadmill-send-string (p s)
  (process-send-string (treadmill-repl-process p) s))

(defun treadmill-eval (p e)
  (with-current-buffer (treadmill-repl-buffer p)
    (erase-buffer)
    (setq treadmill-repl-awaiting-value t)
    (treadmill-send-string p (format "%S\n" e))))

;; (setq gp (treadmill-start-server))
;; (treadmill-eval gp '(import :thunknyc/ioenv))
;; (treadmill-eval gp '(begin/string-io "(+ 1 2)" (displayln (eval (read)))))
;; (treadmill-eval gp '(map 1+ (iota 10)))
;; (treadmill-eval gp '(list + 1 2))
;; (treadmill-clean-up gp)
