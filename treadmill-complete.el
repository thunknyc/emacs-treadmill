;;; treadmill-complete.el --- Competion facility for Treadmill -*- lexical-binding: t -*-

;; Copyright © 2018-9 Thunk NYC Corp.
;;
;; Author: Edwin Watkeys <edw@poseur.com>
;;
;; URL: https://github.com/thunknyc/emacs-treadmill
;; Keywords: languages gerbil scheme lisp
;; Version: 0.1-snapshot
;; Package-Requires: ((emacs "25.1") (company "0.9.0") (cl-lib "0.3"))

;;; Commentary:

;; This package is part of thw Treadmill project.  It provides symbol
;; completion in Treadmill interaction buffers as well as Gerbil
;; buffers where treadmill-gerbil-mode is enabled.

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

(require 'cl-lib)
(require 'company)
(require 'subr-x)

(declare-function treadmill-eval1 "treadmill")
(declare-function treadmill-plugin-null-hook "treadmill")

(defun treadmill-complete--symbol-meta (name)
  "Return completion metadata for NAME using network REPL."
  (let ((meta
         (treadmill-eval1 (format "(completion-meta \"%s\")" name))))
    (if meta (format "Modules: %s" (string-join meta " "))
      (format "No information for %s" name))))

(defun treadmill-complete--complete (prefix)
  "Return completion candidates for symbol PREFIX using network REPL."
  (let ((expr (format "(complete \"^%s\")" prefix)))
    (treadmill-eval1 expr)))

(defun treadmill-complete--company-backend
    (command &optional arg &rest _ignored)
  "Working with Company by evaluating COMMAND, using ARG if approporiate."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'treadmill-company-backend))
    (prefix (and (or (eq major-mode 'treadmill-mode)
                     (bound-and-true-p treadmill-gerbil-mode))
                 (let ((sym (company-grab-symbol)))
                   (and (> (length sym) 1)
                        sym))))
    (candidates (treadmill-complete--complete arg))
    (meta (treadmill-complete--symbol-meta arg))))
(add-to-list 'company-backends 'treadmill-complete--company-backend)

(defun treadmill-complete-plugin-proc (e arg)
  "Process Treadmill event E with arg ARG."
  (cond ((eq e 'connected)
         (treadmill-eval1
          "(begin (import :thunknyc/apropos)
                  (thread-start! (make-thread (lambda ()
                                                (current-apropos-db))))
                  'started)"))

        (t (treadmill-plugin-null-hook e arg))))

(add-hook 'treadmill-plugin-functions 'treadmill-complete-plugin-proc)

(provide 'treadmill-complete)

;;; treadmill-complete.el ends here
