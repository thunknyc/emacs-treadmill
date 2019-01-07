;;; treadmill-history.el --- History facility for Treadmill -*- lexical-binding: t -*-

;; Copyright © 2018-9 Thunk NYC Corp.
;;
;; Author: Edwin Watkeys <edw@poseur.com>
;;
;; URL: https://github.com/thunknyc/emacs-treadmill
;; Keywords: languages gerbil scheme lisp
;; Version: 0.1-snapshot
;; Package-Requires: ((emacs "25.1") (company "0.9.0") (cl-lib "0.3"))

;;; Commentary:

;; This package is part of thw Treadmill project.  It provides a
;; history facility in for Treadmill interaction buffers.

;;; License:

;; Copyright © 2018-9 Thunk NYC Corp.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code:

(declare-function treadmill-plugin-null-hook "treadmill")

(defvar-local treadmill-history--buffer nil
  "Buffer in which history items are stored and retrieved.")

(defvar-local treadmill-history--input-is-history nil
  "True is user has not touched interaction input after inserted by history.")

(defvar-local treadmill-history--changing-buffer nil
  "Flag to prevent simultaneous manipulation of the history.")

(defun treadmill-history--reset (_b _e _l)
  "Indicate that input is nonhistorical due to user editing."
  (when (not treadmill-history--changing-buffer)
    (setq treadmill-history--input-is-history nil)
    (with-current-buffer treadmill-history--buffer (goto-char (point-max)))))

(defun treadmill-history--replace-input (s)
  "Kill the current input, replacing it with S."
  (setq treadmill-history--changing-buffer t)
  (if treadmill-history--input-is-history
      (delete-region treadmill-ia-mark (point-max))
    (kill-region treadmill-ia-mark (point-max)))
  (insert (string-trim s))
  (setq treadmill-history--changing-buffer nil)
  (setq treadmill-history--input-is-history t))

(defun treadmill-history--advance ()
  "Move POINT to the next history item in history buffer."
  (cond ((equal (point) (point-max))    ; nothing to do
         nil)
        (t
         (goto-char (+ (point) 11))
         (if (search-forward ";;;;;;;;;;\n" nil t)
             (goto-char (match-beginning 0))
           (goto-char (point-max))))))

(defun treadmill-history--next ()
  "Return next history item from history buffer."
  (with-current-buffer treadmill-history--buffer
    (treadmill-history--advance)
    (cond ((equal (point) (point-max))
           (message "No next history item")
           "")
          (t (let ((expr-start (+ (point) 11)))
               (goto-char expr-start)
               (if (search-forward-regexp ";;;;;;;;;;\n" nil t)
                   (let* ((expr-end (match-beginning 0))
                          (expr (buffer-substring expr-start expr-end)))
                     (goto-char expr-end)
                     expr)
                 (let* ((expr-end (point-max))
                        (expr (buffer-substring expr-start expr-end)))
                   (goto-char expr-end)
                   expr)))))))

(defun treadmill-history--previous ()
  "Return previous history item from history buffer."
  (with-current-buffer treadmill-history--buffer
    (let ((expr-end (point)))
      (cond ((search-backward-regexp ";;;;;;;;;;\n" nil t)
             (let* ((expr-start (match-end 0))
                    (expr (buffer-substring expr-start expr-end)))
               (goto-char (match-beginning 0))
               expr))
            (t (error "No previous history item"))))))

(defun treadmill-history-ia-next ()
  "Replace the current input with the next history item."
  (interactive)
  (let ((h (treadmill-history--next)))
    (treadmill-history--replace-input h)))

(defun treadmill-history-ia-previous ()
  "Replace the current input with the previous history item."
  (interactive)
  (let ((h (treadmill-history--previous)))
    (treadmill-history--replace-input h)))

(defun treadmill-history--push-item (input)
  "Add INPUT to the end of the history buffer and make it current item."
  (let ((cleaned (string-trim input)))
    (when (not (string-empty-p cleaned))
      (with-current-buffer treadmill-history--buffer
        (goto-char (point-max))
        (insert ";;;;;;;;;;\n")
        (insert cleaned)
        (insert "\n")))))

(defun treadmill-history-plugin-proc (e arg)
  "Process Treadmill event E with arg ARG."
  (cond ((eq e 'keymap)
         (define-key arg (kbd "M-p") 'treadmill-history-ia-previous)
         (define-key arg (kbd "M-n") 'treadmill-history-ia-next)
         arg)

        ((eq e 'connected)
         (setq treadmill-history--buffer
               (generate-new-buffer "*treadmill-history*"))
         (add-hook 'after-change-functions 'treadmill-history--reset))

        ((eq e 'quitting)
         (kill-buffer treadmill-history--buffer))

        ((eq e 'expression)
         (treadmill-history--push-item arg)
         arg)

        (t (treadmill-plugin-null-hook e arg))))

(add-hook 'treadmill-plugin-functions 'treadmill-history-plugin-proc)

(provide 'treadmill-history)

;;; treadmill-history.el ends here
