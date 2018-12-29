;; -*- lexical-binding: t -*-

(require 'gerbil)
(require 'subr-x)

(defconst treadmill-interpreter-path "/Users/edw/dev/gerbil/bin/gxi")
(defconst treadmill-host "127.0.0.1")

(defvar treadmill-current-interaction-buffer nil)

(defvar-local treadmill-spawn-port nil)
(defvar-local treadmill-spawn-process nil)
(defvar-local treadmill-repl-awaiting-value nil)

(defvar-local treadmill-interaction-buffer nil)
(defvar-local treadmill-repl-process nil)

(defvar-local treadmill-ia-mark nil)

(defun treadmill-spawn ()
  (interactive)
  (treadmill-start-server))

(defun treadmill-command ()
  (list treadmill-interpreter-path
        "-e" "(import :thunknyc/treadmill) (start-treadmill!)"))

(defvar-local treadmill-repl-error-level nil)

(defun treadmill-repl-value ()
  (goto-char (point-max))
  (when (search-backward-regexp "\r\n\\([0-9]*\\)> " nil t)
    (let ((prompt (match-string 0))
          (error-level (match-string 1)))
      (if (string-empty-p error-level)
          (setq treadmill-repl-error-level nil)
        (setq treadmill-repl-error-level (string-to-number error-level)))
      (buffer-substring 1 (- (point-max) (length prompt))))))

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
            (goto-char (point-max))
            (when (search-backward-regexp "\r\n[0-9]*> " nil t)
              (setq treadmill-repl-awaiting-value nil)
              (let ((result (string-trim (treadmill-repl-value))))
                (when (not (zerop (length result)))
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
          (goto-char (point-max))
          (when (search-backward-regexp "\r\n[0-9]*> " nil t)
            (setq treadmill-repl-awaiting-value nil)
            (let ((result (string-trim (treadmill-repl-value))))
              (when (not (zerop (length result)))
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
      (setq treadmill-current-interaction-buffer (buffer-name b))
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

(defmacro with-treadmill (&rest exprs)
  (let ((temp-b (make-symbol "buffer")))
    `(if (bound-and-true-p treadmill-repl-process)
         (progn ,@exprs)
       (let ((,temp-b
              (and treadmill-current-interaction-buffer
                   (get-buffer treadmill-current-interaction-buffer))))
         (with-current-buffer ,temp-b ,@exprs)))))

(defvar-local treadmill-eval-waiting nil)
(defvar-local treadmill-eval-value nil)

(defun treadmill-eval-lowlevel (s)
  (with-treadmill
   (setq treadmill-eval-waiting t)
     (let ((b (current-buffer)))
       (treadmill-eval-lowlevel-complete
        s
        (lambda (val)
          (with-current-buffer b
            (setq treadmill-eval-value val)
            (setq treadmill-eval-waiting nil)))))
     (while treadmill-eval-waiting
       (sleep-for 0 50))
     treadmill-eval-value))

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

(defvar treadmill-mode-hook nil)

(defvar treadmill-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'treadmill-eval)
    (define-key map (kbd "C-c q") 'treadmill-quit)
    map))

(defun treadmill-mode ()
  "Major mode for interacting with Gerbil"
  (interactive)
  (use-local-map treadmill-mode-map)
  (setq mode-name "Treadmill Interaction")
  (setq major-mode 'treadmill-mode)
  (company-mode-maybe)
  (run-hooks 'treadmill-mode-hook))

(defun treadmill-eval-last ()
  (interactive)
  (let ((ia-b (and treadmill-current-interaction-buffer
                   (get-buffer treadmill-current-interaction-buffer))))
    (if ia-b
        (let* ((sexp (elisp--preceding-sexp))
              (str (format "%S" sexp)))
          (message "Evaluating %s" str)
          (with-current-buffer ia-b
            (treadmill-eval-lowlevel-complete
             str (lambda (val) (message "=> %s" val)))))
      (error "Treadmill: No current interaction buffer."))))

(defun treadmill-symbol-at-point ()
  (save-excursion
    (when (re-search-backward "[^-0\\^-9A-Za-z#_%#@!*|+><./?]\\([-0\\^-9A-Za-z#_%#@!*|+><./?]+\\)$")
      (match-string 1))))

(defun treadmill-complete (prefix)
  (read (treadmill-eval-lowlevel (format "(complete \"^%s\")" prefix))))

(defun treadmill-complete-meta (name)
  (let ((meta (read (treadmill-eval-lowlevel
                     (format "(completion-meta \"%s\")"
                             name)))))
    (if meta (format "Modules: %s" (string-join meta " "))
      (format "No information for %s" name))))

(defvar treadmill-use-company nil)

(when (boundp 'company-mode)
  (require 'cl-lib)
  (defun treadmill-company-backend (command &optional arg &rest ignored)
    (interactive (list 'interactive))
    (cl-case command
      (interactive (company-begin-backend 'treadmill-company-backend))
      (prefix (and (or (eq major-mode 'treadmill-mode)
                       (bound-and-true-p treadmill-gerbil-mode))
                   (let ((sym (company-grab-symbol)))
                     (and (> (length sym) 1)
                          sym))))
      (candidates (treadmill-complete arg))
      (meta (treadmill-complete-meta arg))))
  (add-to-list 'company-backends 'treadmill-company-backend)
  (add-hook 'treadmill-mode-hook #'company-mode)
  (setq treadmill-use-company t))

(defun company-mode-maybe ()
  (if treadmill-use-company
      (company-mode)))

(define-minor-mode treadmill-gerbil-mode
  "Mode for talking to Treadmill in Gerbil buffers"
  :lighter " TM"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-x C-e") 'treadmill-eval-last)
            map)
  (company-mode-maybe))

;;###autoload
(add-hook 'gerbil-mode-hook 'treadmill-gerbil-mode)
;;###autoload

(provide 'treadmill-mode)
(provide 'treadmill-gerbil-mode)
