;; -*- lexical-binding: t -*-

;; (setq gp (treadmill-start-server))
;; (treadmill-eval-lowlevel* gp "(import :thunknyc/treadmill)")
;; (treadmill-eval-lowlevel* gp "(import :thunknyc/apropos)")
;; (treadmill-eval-lowlevel* gp "(+ 1 1)")
;; (treadmill-eval* gp "(read)" "(+ 1 1)")
;; (treadmill-eval* gp "(string-append \"foo\" \"bar\")" "")
;; (treadmill-eval* gp "(apropos-re \"^disp\")" "")
;; (treadmill-apropos-prefix* gp "call-with-current-")
;; (treadmill-clean-up* gp)

(defun treadmill-spawn ()
  (interactive)
  (treadmill-start-server))

(require 'subr-x)

(defconst treadmill-interpreter-path "/Users/edw/dev/gerbil/bin/gxi")
(defconst treadmill-host "127.0.0.1")

(defvar-local treadmill-spawn-port nil)
(defvar-local treadmill-spawn-process nil)
(defvar-local treadmill-repl-awaiting-value nil)

(defvar-local treadmill-interaction-buffer nil)
(defvar-local treadmill-repl-process nil)

(defvar-local treadmill-ia-mark nil)


(defun treadmill-command ()
  (list treadmill-interpreter-path
        "-e" "(import :thunknyc/treadmill) (start-treadmill!)"))

(defun treadmill-lowlevel-completion-filter (proc)
  (let ((proc proc))
    (lambda (p s)
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
            (when (search-forward-regexp
                   "^\\(\\(.*\r\n\\)+\\)[0-9]*> $"
                   nil t)
              (setq treadmill-repl-awaiting-value nil)
              (let ((result (string-trim (match-string 1))))
                (if (zerop (length result))
                    (message "No value in completion filter")
                  (funcall proc result))))))))))

(defun treadmill-repl-completion-filter (proc)
  (lambda (p s)
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
          (when (search-forward-regexp
                 "^\\(\\(.*\r\n\\)+\\)[0-9]*> $"
                 nil t)
            (setq treadmill-repl-awaiting-value nil)
            (let ((result (string-trim (match-string 1))))
              (if (zerop (length result))
                  (message "=> No value")
                (funcall proc (read result))))))))))

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
        (when (search-forward-regexp
               "^\\(\\(.*\r\n\\)+\\)[0-9]*> $"
               nil t)
          (setq treadmill-repl-awaiting-value nil)
          (let ((result (string-trim (match-string 1))))
            (if (zerop (length result))
                (message "=> No value")
              (message "=> %S" (read result)))))))))

(defun treadmill-spawn-filter (p s)
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
    (when (null treadmill-spawn-port)
      (save-excursion
        (goto-char 0)
        (when (search-forward-regexp
               "Running net repl on port \\([0-9]+\\)."
               nil t)
          (let ((port (string-to-number (match-string 1))))
            (setq treadmill-spawn-port port)
            (message "Net repl starting on port %d." port)
            (treadmill-connect treadmill-host port)))))))

(defun treadmill-secure-history ()
  (let ((saved-inhibit-read-only inhibit-read-only))
    (setq inhibit-read-only t)
    (add-text-properties (point-min) (point-max)
                         '(front-sticky t rear-nonsticky t read-only t))
    (setq inhibit-read-only saved-inhibit-read-only)))

(defun treadmill-insert (what)
  (let ((saved-inhibit-read-only inhibit-read-only))
    (setq inhibit-read-only t)
    (insert what)
    (setq inhibit-read-only saved-inhibit-read-only)))

(defun treadmill-issue-prompt ()
  (interactive)
  (goto-char (point-max))
  (treadmill-insert "> ")
  (setq treadmill-ia-mark (point-max-marker))
  (treadmill-secure-history))

(defun treadmill-eval ()
  (interactive)
  (let ((s (buffer-substring treadmill-ia-mark (point-max)))
        (stdin "")
        (b (current-buffer)))
    (goto-char (point-max))
    (treadmill-insert (format-message "\nEvaluating `%s' with STDIN `%s'\n"
                                      s stdin))
    (treadmill-eval-complete
     s stdin
     (lambda (val)
       (let ((results (car val))
             (stdout (cadr val))
             (stderr (caddr val)))
         (with-current-buffer b
           (goto-char (point-max))
           (treadmill-insert (format-message "=> %s\nSTDOUT:%s\nSTDERR:%s\n"
                                             results stdout stderr))
           (treadmill-issue-prompt)))))))

(defun treadmill-connect (host port)
  (interactive
   "sConnect to network REPL at host: \nsREPL port on %s: \n")
  (let* ((repl-b (generate-new-buffer "*treadmill-repl*"))
         (repl-p (open-network-stream "treadmill-repl"
                                      repl-b host port)))
    ;; If treadmill-spawn-process is defined it means we're in the
    ;; spawn buffer and we should connect the spawn buffer with the
    ;; repl buffer, so we can tear down the spawn process when we kill
    ;; the repl.
    (let ((spawn-process treadmill-spawn-process))
      (message "connect via spawn")
      (when spawn-process
        (setq treadmill-repl-process repl-p)
        (with-current-buffer repl-b
          (setq treadmill-spawn-process spawn-process))))
    (message "Repl process is `%s'." repl-p)
    (message "Connected to repl on port %d." port)
    (let ((b (generate-new-buffer "*treadmill*")))
      (with-current-buffer repl-b
        (setq treadmill-interaction-buffer b)
              (setq treadmill-repl-process repl-p))
      (switch-to-buffer b)
      (setq treadmill-repl-process repl-p)
      (insert ";; Welcome to the Gerbil Treadmill\n")
      (treadmill-eval-lowlevel-complete
       "(import :thunknyc/apropos)" (lambda (ignore) 'ignore))
      (treadmill-issue-prompt)
      (treadmill-mode))))

(defun treadmill-start-server ()
  (let* ((b (generate-new-buffer "*treadmill-spawn*"))
         (p (make-process :name "treadmill-spawn" :buffer b :coding 'utf-8
                          :type 'pipe :command (treadmill-command)
                          :filter 'treadmill-spawn-filter)))
    (with-current-buffer b
      (setq treadmill-spawn-process p))
    (message "Started `%s' in `%s'" p b)
    p))

(defun treadmill-clean-up* (p)
  (let* ((repl-p (treadmill-repl-process* p))
         (repl-b (process-buffer repl-p))
         (ia-b (buffer-local-value 'treadmill-interaction-buffer repl-b))
         (n (process-name p))
         (b (process-buffer p)))
    (kill-buffer ia-b)
    (delete-process repl-p)
    (kill-buffer repl-b)
    (delete-process p)
    (kill-buffer b)
    (message "Deleted process `%s' and associated buffers." n)))

(defun treadmill-repl-process* (p)
  (buffer-local-value 'treadmill-repl-process (process-buffer p)))

(defun treadmill-repl-buffer* (p)
  (process-buffer (treadmill-repl-process* p)))

(defun treadmill-send-string* (p s)
  (process-send-string (treadmill-repl-process* p) s))

(defun treadmill-eval-lowlevel* (p s)
  (with-current-buffer (treadmill-repl-buffer* p)
    (erase-buffer)
    (setq treadmill-repl-awaiting-value t)
    (set-process-filter (treadmill-repl-process* p)
                        'treadmill-repl-filter-lowlevel)
    (treadmill-send-string* p s)))

(defun treadmill-eval* (p expr-string input-string)
  (with-current-buffer (treadmill-repl-buffer* p)
    (erase-buffer)
    (setq treadmill-repl-awaiting-value t)
    (set-process-filter (treadmill-repl-process* p)
                        'treadmill-repl-filter)
    (let ((s (format "(eval-string/input-string %S %S)"
                     expr-string input-string)))
      (treadmill-send-string* p s))))

(defun treadmill-eval-lowlevel-complete (s completion)
  (let ((p treadmill-repl-process))
    (with-current-buffer (process-buffer p)
      (erase-buffer)
      (setq treadmill-repl-awaiting-value t)
      (set-process-filter (treadmill-repl-process* p)
                          (treadmill-lowlevel-completion-filter completion))
      (process-send-string p s))))

(defun treadmill-eval-complete* (p expr-string input-string completion)
  (with-current-buffer (treadmill-repl-buffer* p)
    (erase-buffer)
    (setq treadmill-repl-awaiting-value t)
    (set-process-filter (treadmill-repl-process* p)
                        (treadmill-completion-filter completion))
    (let ((s (format "(eval-string/input-string %S %S)"
                     expr-string input-string)))
      (treadmill-send-string* p s))))

;; Needs to be called inside an interaction buffer. Procs ending with
;; `*' star need to be passed a spawn process, which sucks, because
;; spawning is not necessary.
(defun treadmill-eval-complete (expr-string input-string completion)
  (let ((p treadmill-repl-process))
    (with-current-buffer (process-buffer p)
      (erase-buffer)
      (setq treadmill-repl-awaiting-value t)
      (set-process-filter p (treadmill-repl-completion-filter completion))
      (let ((s (format "(eval-string/input-string %S %S)"
                       expr-string input-string)))
        (process-send-string p s)))))

(defun treadmill-eval-lowlevel-complete* (p s completion)
  (with-current-buffer (treadmill-repl-buffer* p)
    (erase-buffer)
    (setq treadmill-repl-awaiting-value t)
    (set-process-filter (treadmill-repl-process* p)
                        (treadmill-lowlevel-completion-filter completion))
    (treadmill-send-string* p s)))

(defun treadmill-apropos-prefix* (p prefix)
  (treadmill-eval-lowlevel-complete*
   p
   (format "(apropos-re \"^%s\")" prefix)
   (lambda (v) (message "Apropos result: %S" v))))

(defun treadmill-repl-quit ()
  (let* ((repl-p treadmill-repl-process)
         (repl-b (current-buffer))
         (spawn-p (buffer-local-value 'treadmill-spawn-process repl-b))
         (spawn-b (if spawn-p (process-buffer spawn-p) nil)))
    (delete-process repl-p)
    (kill-buffer repl-b)
    (when spawn-p
      (delete-process spawn-p)
      (kill-buffer spawn-b))))

(defun treadmill-quit ()
  (interactive)
  (with-current-buffer (process-buffer treadmill-repl-process)
    (treadmill-repl-quit))
  (kill-buffer))

(defun treadmill-symbol-at-point ()
  (when (re-search-backward "")
    (match-string 0)))

(defun treadmill-symbol-at-point ()
  (when (re-search-backward "[^-0\\^-9A-Za-z#_%#@!*|+><./?]\\([-0\\^-9A-Za-z#_%#@!*|+><./?]+\\)")
    (match-string 1)))

(defun treadmill-complete ()
  (interactive)
  (save-excursion
    (let ((partial-symbol (treadmill-symbol-at-point)))
      (message "%s" partial-symbol)
      (treadmill-eval-lowlevel-complete
       (format "(apropos-re \"^%s\")" partial-symbol)
       (lambda (str)
         (let* ((val (read str))
                (names (car val))
                (match-list (cadr names))
                (matching-names (mapcar (lambda (el) (car el)) match-list)))
                      (message "Apropos found: %s" matching-names)))))))

(defvar treadmill-mode-hook nil)

(defvar treadmill-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'treadmill-eval)
    (define-key map (kbd "C-c q") 'treadmill-quit)
    (define-key map (kbd "M-TAB") 'treadmill-complete)
    map))

(defun treadmill-mode ()
  "Major mode for interacting with Gerbil Scheme"
  (interactive)
  (use-local-map treadmill-mode-map)
  (setq mode-name "Treadmill Interaction")
  (run-hooks 'treadmill-mode-hook))

(provide 'treadmill-mode)
