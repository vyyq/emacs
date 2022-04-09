;;; erc-scenarios-common.el --- common helpers for ERC scenarios -*- lexical-binding: t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.
;;
;; This file is part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file should not contain any test cases.

(require 'ert-x) ; cl-lib

(eval-and-compile (let ((dir (getenv "EMACS_TEST_DIRECTORY")))
                    (when dir
                      (load (concat dir "/lisp/erc/erc-d/erc-d-t") nil t)
                      (load (concat dir "/lisp/erc/erc-d/erc-d") nil t))))
(require 'erc-d)
(require 'erc-d-t)
(require 'erc-backend)

(defvar erc-scenarios-common--resources-dir
  (expand-file-name (concat (ert-resource-directory)
                            "../erc-scenarios-resources/")))

;; Because teardown is already inhibited when running interactively,
;; which prevents subsequent tests from succeeding, we might as well
;; treat inspection as the goal.
(unless noninteractive
  (setq erc-server-auto-reconnect nil))

(defvar erc-scenarios-common-dialog nil)
(defvar erc-scenarios-common-extra-teardown nil)

(defun erc-scenarios-common--add-silence ()
  (advice-add #'erc-login :around #'erc-d-t-silence-around)
  (advice-add #'erc-handle-login :around #'erc-d-t-silence-around)
  (advice-add #'erc-server-connect :around #'erc-d-t-silence-around))

(defun erc-scenarios-common--remove-silence ()
  (advice-remove #'erc-login #'erc-d-t-silence-around)
  (advice-remove #'erc-handle-login #'erc-d-t-silence-around)
  (advice-remove #'erc-server-connect #'erc-d-t-silence-around))

(defun erc-scenarios-common--print-trace ()
  (when (and (boundp 'trace-buffer) (get-buffer trace-buffer))
    (with-current-buffer trace-buffer
      (message "%S" (buffer-string))
      (kill-buffer))))

(defun erc-scenarios-common--make-bindings (bindings)
  `((erc-d-u-canned-dialog-dir (expand-file-name
                                (or erc-scenarios-common-dialog
                                    (cadr (assq 'erc-scenarios-common-dialog
                                                ',bindings)))
                                erc-scenarios-common--resources-dir))
    (erc-d-spec-vars `(,@erc-d-spec-vars
                       (quit . ,(erc-quit/part-reason-default))
                       (erc-version . ,erc-version)))
    (erc-modules (copy-sequence erc-modules))
    (inhibit-interaction t)
    (auth-source-do-cache nil)
    (erc-autojoin-channels-alist nil)
    (erc-server-auto-reconnect nil)
    ,@bindings))

(defmacro erc-scenarios-common-with-cleanup (bindings &rest body)
  "Provide boilerplate cleanup tasks after calling BODY with BINDINGS.

If an `erc-d' process exists, wait for it to start before running BODY.
If `erc-autojoin-mode' mode is bound, restore it during cleanup if
disabled by BODY.  Other defaults common to these test cases are added
below and can be overridden, except when wanting the \"real\" default
value, which must be looked up or captured outside of the calling form.

Dialog resource directories are located by expanding the variable
`erc-scenarios-common-dialog' or its value in BINDINGS."
  (declare (indent 1))

  (let* ((orig-autojoin-mode (make-symbol "orig-autojoin-mode"))
         (combind `((,orig-autojoin-mode (bound-and-true-p erc-autojoin-mode))
                    ,@(erc-scenarios-common--make-bindings bindings))))

    `(erc-d-t-with-cleanup (,@combind)

         (ert-info ("Restore autojoin, etc., kill ERC buffers")
           (dolist (buf (buffer-list))
             (when-let ((erc-d-u--process-buffer)
                        (proc (get-buffer-process buf)))
               (erc-d-t-wait-for 5 "Dumb server dies on its own"
                 (not (process-live-p proc)))))

           (erc-scenarios-common--remove-silence)

           (when erc-scenarios-common-extra-teardown
             (ert-info ("Running extra teardown")
               (funcall erc-scenarios-common-extra-teardown)))

           (when (and (boundp 'erc-autojoin-mode)
                      (not (eq erc-autojoin-mode ,orig-autojoin-mode)))
             (erc-autojoin-mode (if ,orig-autojoin-mode +1 -1)))

           (when noninteractive
             (erc-scenarios-common--print-trace)
             (erc-d-t-kill-related-buffers)))

       (erc-scenarios-common--add-silence)

       (ert-info ("Wait for dumb server")
         (dolist (buf (buffer-list))
           (with-current-buffer buf
             (when erc-d-u--process-buffer
               (erc-d-t-search-for 3 "Starting")))))

       (ert-info ("Activate erc-debug-irc-protocol")
         (unless (and noninteractive (not erc-debug-irc-protocol))
           (erc-toggle-debug-irc-protocol)))

       ,@body)))

(defun erc-scenarios-common-assert-initial-buf-name (id port)
  ;; Assert no limbo period when explicit ID given
  (should (string= (if id
                       (symbol-name id)
                     (format "127.0.0.1:%d" port))
                   (buffer-name))))

(defun erc-scenarios-common-buflist (prefix)
  "Return list of buffers with names sharing PREFIX."
  (let (case-fold-search)
    (erc-networks--id-sort-buffers
     (delq nil
           (mapcar (lambda (b)
                     (when (string-prefix-p prefix (buffer-name b)) b))
                   (buffer-list))))))

(provide 'erc-scenarios-common)

;;; erc-scenarios-common.el ends here
