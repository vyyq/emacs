;;; loaddefs-gen.el --- generate loaddefs.el files  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Keywords: maint
;; Package: emacs

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package generates the main lisp/loaddefs.el file, as well as
;; all the other loaddefs files, like calendar/diary-loaddefs.el, etc.

;; The main entry point is `loaddefs-gen--generate' (normally called
;; from batch-loaddefs-gen via lisp/Makefile).
;;
;; The "other" loaddefs files are specified either via a file-local
;; setting of `generated-autoload-file', or by specifying
;;
;;   ;;;###foo-autoload
;;
;; This makes the autoload go to foo-loaddefs.el in the current directory.
;; Normal ;;;###autoload specs go to the main loaddefs file.

;; This file currently contains a bunch of things marked FIXME that
;; are only present to create identical output from the older files.
;; These should be removed.

;;; Code:

(require 'autoload)

(defun loaddefs-gen--parse-file (file main-outfile &optional package-only)
  "Examing FILE for ;;;###autoload statements.
MAIN-OUTFILE is the main loaddefs file these statements are
destined for, but this can be overriden by the buffer-local
setting of `generated-autoload-file' in FILE, and
by ;;;###foo-autoload statements.

If PACKAGE-ONLY, only return the package info."
  (let ((defs nil)
        (load-name (autoload-file-load-name file main-outfile))
        (compute-prefixes t)
        local-outfile package-defs
        inhibit-autoloads)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-max))
      ;; We "open-code" this version of `hack-local-variables',
      ;; because it's really slow in bootstrap-emacs.
      (when (search-backward ";; Local Variables:" (- (point-max) 1000) t)
        (save-excursion
          (when (re-search-forward "generated-autoload-file: *" nil t)
            ;; Buffer-local file that should be interpreted relative to
            ;; the .el file.
            (setq local-outfile (expand-file-name (read (current-buffer))
                                                  (file-name-directory file)))))
        (save-excursion
          (when (re-search-forward "generated-autoload-load-name: *" nil t)
            (setq load-name (read (current-buffer)))))
        (when (re-search-forward "no-update-autoloads: *" nil t)
          (setq inhibit-autoloads (read (current-buffer))))
        (when (re-search-forward "autoload-compute-prefixes: *" nil t)
          (setq compute-prefixes (read (current-buffer)))))

      ;; We always return the package version (even for pre-dumped
      ;; files).
      (let ((version (lm-header "version"))
            package)
        (when (and version
                   (setq version (ignore-errors (version-to-list version)))
                   (setq package (or (lm-header "package")
                                     (file-name-sans-extension
                                      (file-name-nondirectory file)))))
          ;; FIXME: Push directly to defs.
          (setq package-defs
                `(push (purecopy ',(cons (intern package) version))
                       package--builtin-versions))))

      ;; Obey the `no-update-autoloads' file local variable.
      (when (and (not inhibit-autoloads)
                 (not package-only))
        (goto-char (point-min))
        ;; The cookie might be like ;;;###tramp-autoload...
        (while (re-search-forward lisp-mode-autoload-regexp nil t)
          ;; ... and if we have one of these names, then alter outfile.
          (let* ((aname (match-string 2))
                 (to-file (if aname
                              (expand-file-name
                               (concat aname "-loaddefs.el")
                               (file-name-directory file))
                            (or local-outfile main-outfile))))
            (if (eolp)
                ;; We have a form following.
                (let* ((form (prog1
                                 (read (current-buffer))
                               (unless (bolp)
                                 (forward-line 1))))
                       (autoload (or (make-autoload form load-name) form)))
                  ;; We get back either an autoload form, or a tree
                  ;; structure of `(progn ...)' things, so unravel that.
                  (let ((forms (if (eq (car autoload) 'progn)
                                   (cdr autoload)
                                 (list autoload))))
                    (while forms
                      (let ((elem (pop forms)))
                        (if (eq (car elem) 'progn)
                            ;; More recursion; add it to the start.
                            (setq forms (nconc (cdr elem) forms))
                          ;; We have something to add to the defs; do it.
                          (push (list to-file file
                                      (loaddefs-gen--prettify-autoload elem))
                                defs))))))
              ;; Just put the rest of the line into the loaddefs.
              ;; FIXME: We skip the first space if there's more
              ;; whitespace after.
              (when (looking-at-p " [\t ]")
                (forward-char 1))
              (push (list to-file file
                          (buffer-substring (point) (line-end-position)))
                    defs))))

        (when (and autoload-compute-prefixes
                   compute-prefixes)
          (when-let ((form (loaddefs-gen--compute-prefixes load-name)))
            ;; This output needs to always go in the main loaddefs.el,
            ;; regardless of `generated-autoload-file'.

            ;; FIXME: Not necessary.
            (setq form (loaddefs-gen--prettify-autoload form))

            ;; FIXME: For legacy reasons, many specs go elsewhere.
            (cond ((and (string-match "/cedet/" file) local-outfile)
                   (push (list local-outfile file form) defs))
                  ((string-match "/cedet/\\(semantic\\|srecode\\)/"
                                 file)
                   (push (list (concat (substring file 0 (match-end 0))
                                       "loaddefs.el")
                               file form)
                         defs))
                  (local-outfile
                   (push (list local-outfile file form) defs))
                  (t
                   (push (list main-outfile file form) defs)))))))

    (if package-defs
        (nconc defs (list (list (or local-outfile main-outfile) file
                                package-defs)))
      defs)))

(defun loaddefs-gen--compute-prefixes (load-name)
  (goto-char (point-min))
  (let ((prefs nil))
    ;; Avoid (defvar <foo>) by requiring a trailing space.
    (while (re-search-forward
            "^(\\(def[^ ]+\\) ['(]*\\([^' ()\"\n]+\\)[\n \t]" nil t)
      (unless (member (match-string 1) autoload-ignored-definitions)
        (let ((name (match-string-no-properties 2)))
          (when (save-excursion
                  (goto-char (match-beginning 0))
                  (or (bobp)
                      (progn
                        (forward-line -1)
                        (not (looking-at ";;;###autoload")))))
            (push name prefs)))))
    (autoload--make-defs-autoload prefs load-name)))

(defun loaddefs-gen--prettify-autoload (autoload)
  ;; FIXME: All this is just to emulate the current look -- it should
  ;; probably all go.
  (with-temp-buffer
    (prin1 autoload (current-buffer) '(t (escape-newlines . t)
                                         (escape-control-characters . t)))
    (goto-char (point-min))
    (when (memq (car autoload)
                '( defun autoload defvar defconst
                   defvar-local defsubst defcustom defmacro
                   cl-defsubst))
      (forward-char 1)
      (ignore-errors
        (forward-sexp 3)
        (skip-chars-forward " "))
      (when (looking-at-p "\"")
        (let* ((start (point))
               (doc (read (current-buffer))))
          (delete-region start (point))
          (prin1 doc (current-buffer) t)
          (goto-char start))
        (save-excursion
          (forward-char 1)
          (insert "\\\n"))
        (narrow-to-region (point)
                          (progn
                            (forward-sexp 1)
                            (point)))
        (goto-char (point-min))
        (while (search-forward "\n(" nil t)
          (replace-match "\n\\(" t t))
        (widen)))
    (goto-char (point-min))
    (insert "\n")
    (buffer-string)))

(defun loaddefs-gen--generate (dir output-file &optional excluded-files)
  "Generate loaddefs files for Lisp files in the directories DIRS.
DIR can be either a single directory or a list of
directories.

The autoloads will be written to OUTPUT-FILE.  If any Lisp file
binds `generated-autoload-file' as a file-local variable, write
its autoloads into the specified file instead.

The function does NOT recursively descend into subdirectories of the
directory or directories specified."
  (let* ((files-re (let ((tmp nil))
		     (dolist (suf (get-load-suffixes))
                       ;; We don't use module-file-suffix below because
                       ;; we don't want to depend on whether Emacs was
                       ;; built with or without modules support, nor
                       ;; what is the suffix for the underlying OS.
		       (unless (string-match "\\.\\(elc\\|so\\|dll\\)" suf)
                         (push suf tmp)))
                     (concat "\\`[^=.].*" (regexp-opt tmp t) "\\'")))
	 (files (apply #'nconc
		       (mapcar (lambda (d)
				 (directory-files (expand-file-name d)
                                                  t files-re))
			       (if (consp dir) dir (list dir)))))
         (defs nil))

    ;; Collect all the autoload data.
    (let ((progress (make-progress-reporter
                     (byte-compile-info
                      (concat "Scraping files for autoloads"))
                     0 (length files) nil 10))
          (file-count 0))
      (dolist (file files)
        (progress-reporter-update progress (setq file-count (1+ file-count)))
        ;; Do not insert autoload entries for excluded files.
        (setq defs (nconc
		    (loaddefs-gen--parse-file
                     file output-file
                     (member (expand-file-name file) excluded-files))
                    defs)))
      (progress-reporter-done progress))

    ;; Generate the loaddef files.  First group per output file.
    (dolist (fdefs (seq-group-by #'car defs))
      (with-temp-buffer
        (insert (autoload-rubric (car fdefs) nil t))
        (search-backward "\f")
        ;; The group by source file (and sort alphabetically).
        (dolist (section (sort (seq-group-by #'cadr (cdr fdefs))
                               (lambda (e1 e2)
                                 (string<
                                  (file-name-sans-extension
                                   (file-name-nondirectory (car e1)))
                                  (file-name-sans-extension
                                   (file-name-nondirectory (car e2)))))))
          (pop section)
          (let ((relfile (file-relative-name
                          (cadar section)
                          (file-name-directory (car fdefs)))))
            (autoload-insert-section-header
             (current-buffer) nil
             (file-name-sans-extension
              (file-name-nondirectory relfile))
             relfile '(0 0 0 0))
            (insert ";;; Generated autoloads from " relfile "\n")
            (dolist (def (reverse section))
              (setq def (caddr def))
              (if (stringp def)
                  (princ def (current-buffer))
                (prin1 def (current-buffer) t))
              (unless (bolp)
                (insert "\n")))
            (insert "\n;;;***\n")))
        ;; FIXME: Remove.
        (goto-char (point-min))
        (while (re-search-forward
                "^;;; Generated autoloads.*\n\\(\n\\)(push" nil t)
          (goto-char (match-end 1))
          (delete-char -1))
        (write-region (point-min) (point-max) (car fdefs) nil 'silent)
        (byte-compile-info (file-relative-name (car fdefs) lisp-directory)
                           t "GEN")))))

(defun loaddefs-gen--excluded-files ()
  ;; Exclude those files that are preloaded on ALL platforms.
  ;; These are the ones in loadup.el where "(load" is at the start
  ;; of the line (crude, but it works).
  (let ((default-directory (file-name-directory lisp-directory))
        (excludes nil)
	file)
    (with-temp-buffer
      (insert-file-contents "loadup.el")
      (while (re-search-forward "^(load \"\\([^\"]+\\)\"" nil t)
	(setq file (match-string 1))
	(or (string-match "\\.el\\'" file)
	    (setq file (format "%s.el" file)))
	(or (string-match "\\`site-" file)
	    (push (expand-file-name file) excludes))))
    ;; Don't scan ldefs-boot.el, either.
    (cons (expand-file-name "ldefs-boot.el") excludes)))

;;;###autoload
(defun batch-loaddefs-gen ()
  "Generate lisp/loaddefs.el autoloads in batch mode."
  ;; For use during the Emacs build process only.
  (let ((args command-line-args-left))
    (setq command-line-args-left nil)
    (loaddefs-gen--generate
     args (expand-file-name "loaddefs.el" lisp-directory)
     (loaddefs-gen--excluded-files))))

(provide 'loaddefs-gen)

;;; loaddefs-gen.el ends here
