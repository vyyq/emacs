;;; erc-services-tests.el --- Tests for erc-services.  -*- lexical-binding:t -*-

;; Copyright (C) 2020-2021 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; For convenience, some tests involving core auth-source
;; functionality have been stashed here for the time being.

;;; Code:

(require 'ert-x)
(require 'erc-services)
(require 'erc-compat)

;;;; Core auth-source

;; Some of the following may be related to bug#23438.

(defvar erc-join-tests--auth-source-entries
  '("machine irc.gnu.org port irc user \"#chan\" password bar"
    "machine my.gnu.org port irc user \"#chan\" password baz"
    "machine GNU.chat port irc user \"#chan\" password foo"))

(defun erc-services-tests--auth-source-shuffle (&rest extra)
  (string-join `(,@(sort (append erc-join-tests--auth-source-entries extra)
                         (lambda (&rest _) (zerop (random 2))))
                 "")
               "\n"))

(ert-deftest erc--auth-source-search--standard ()
  (ert-with-temp-file netrc-file
    :prefix "erc--auth-source-search--standard"
    :text (erc-services-tests--auth-source-shuffle)
    (let ((auth-sources (list netrc-file))
          (auth-source-do-cache nil))

      (ert-info ("Normal ordering")

        (ert-info ("Session wins")
          (let ((erc-session-server "irc.gnu.org")
                (erc-server-announced-name "my.gnu.org")
                (erc-session-port 6697)
                (erc-network 'fake)
                (erc-server-current-nick "tester")
                (erc-networks--id (erc-networks--id-create 'GNU.chat)))
            (should (string= (erc--auth-source-search :user "#chan")
                             "foo"))))

        (ert-info ("Network wins")
          (let* ((erc-session-server "irc.gnu.org")
                 (erc-server-announced-name "my.gnu.org")
                 (erc-session-port 6697)
                 (erc-network 'GNU.chat)
                 (erc-server-current-nick "tester")
                 (erc-networks--id (erc-networks--id-create nil)))
            (should (string= (erc--auth-source-search :user "#chan")
                             "foo"))))

        (ert-info ("Announced wins")
          (let ((erc-session-server "irc.gnu.org")
                (erc-server-announced-name "my.gnu.org")
                (erc-session-port 6697)
                erc-network
                (erc-networks--id (erc-networks--id-create nil)))
            (should (string= (erc--auth-source-search :user "#chan")
                             "baz"))))))))

(ert-deftest erc--auth-source-search--announced ()
  (ert-with-temp-file netrc-file
    :prefix "erc--auth-source-search--announced"
    :text (erc-services-tests--auth-source-shuffle)
    (let* ((auth-sources (list netrc-file))
           (auth-source-do-cache nil)
           (erc--isupport-params (make-hash-table))
           (erc-server-parameters '(("CHANTYPES" . "&#")))
           (erc--target (erc--target-from-string "&chan")))

      (ert-info ("Announced prioritized")

        (ert-info ("Announced wins")
          (let* ((erc-session-server "irc.gnu.org")
                 (erc-server-announced-name "my.gnu.org")
                 (erc-session-port 6697)
                 (erc-network 'GNU.chat)
                 (erc-server-current-nick "tester")
                 (erc-networks--id (erc-networks--id-create nil)))
            (should (string= (erc--auth-source-search :user "#chan")
                             "baz"))))

        (ert-info ("Peer next")
          (let* ((erc-server-announced-name "irc.gnu.org")
                 (erc-session-port 6697)
                 (erc-network 'GNU.chat)
                 (erc-server-current-nick "tester")
                 (erc-networks--id (erc-networks--id-create nil)))
            (should (string= (erc--auth-source-search :user "#chan")
                             "bar"))))

        (ert-info ("Network used as fallback")
          (let* ((erc-session-port 6697)
                 (erc-network 'GNU.chat)
                 (erc-server-current-nick "tester")
                 (erc-networks--id (erc-networks--id-create nil)))
            (should (string= (erc--auth-source-search :user "#chan")
                             "foo"))))))))

(ert-deftest erc--auth-source-search--overrides ()
  (ert-with-temp-file netrc-file
    :prefix "erc--auth-source-search--overrides"
    :text (erc-services-tests--auth-source-shuffle
           "machine GNU.chat port 6697 user \"#chan\" password spam"
           "machine my.gnu.org port irc user \"#fsf\" password 42"
           "machine irc.gnu.org port 6667 password sesame"
           "machine MyHost port irc password 456"
           "machine MyHost port 6667 password 123")

    (let* ((auth-sources (list netrc-file))
           (auth-source-do-cache nil)
           (erc-session-server "irc.gnu.org")
           (erc-server-announced-name "my.gnu.org")
           (erc-network 'GNU.chat)
           (erc-server-current-nick "tester")
           (erc-networks--id (erc-networks--id-create nil))
           (erc-session-port 6667))

      (ert-info ("Specificity and overrides")

        (ert-info ("More specific port")
          (let ((erc-session-port 6697))
            (should (string= (erc--auth-source-search :user "#chan")
                             "spam"))))

        (ert-info ("More specific user (network loses)")
          (should (string= (erc--auth-source-search :user '("#fsf"))
                           "42")))

        (ert-info ("Actual override")
          (should (string= (erc--auth-source-search :port "6667")
                           "sesame")))

        (ert-info ("Overrides don't interfere with post-processing")
          (should (string= (erc--auth-source-search :host "MyHost")
                           "123")))))))

;; auth-source-pass backend

(require 'auth-source-pass)

;; `auth-source-pass--find-match-unambiguous' returns something like:
;;
;;   (list :host "irc.gnu.org"
;;         :port "6697"
;;         :user "rms"
;;         :secret
;;         #[0 "\301\302\300\"\207"
;;             [((secret . "freedom")) auth-source-pass--get-attr secret] 3])
;;
;; This function gives ^ (faked here to avoid gpg and file IO).  See
;; `auth-source-pass--with-store' in ../auth-source-pass-tests.el
(defun erc-services-tests--asp-parse-entry (store entry)
  (when-let ((found (cl-find entry store :key #'car :test #'string=)))
    (list (assoc 'secret (cdr found)))))

(defvar erc-join-tests--auth-source-pass-entries
  '(("irc.gnu.org:irc/#chan"
     ("port" . "irc") ("user" . "#chan") (secret . "bar"))
    ("my.gnu.org:irc/#chan"
     ("port" . "irc") ("user" . "#chan") (secret . "baz"))
    ("GNU.chat:irc/#chan"
     ("port" . "irc") ("user" . "#chan") (secret . "foo"))))

(ert-deftest erc-services-tests--auth-source-pass--standard ()
  (let ((store erc-join-tests--auth-source-pass-entries)
        (auth-sources '(password-store))
        (auth-source-do-cache nil))

    (cl-letf (((symbol-function 'auth-source-pass-parse-entry)
               (apply-partially #'erc-services-tests--asp-parse-entry store))
              ((symbol-function 'auth-source-pass-entries)
               (lambda () (mapcar #'car store))))

      (ert-info ("Normal ordering")

        (ert-info ("Session wins")
          (let ((erc-session-server "irc.gnu.org")
                (erc-server-announced-name "my.gnu.org")
                (erc-session-port 6697)
                (erc-network 'fake)
                (erc-server-current-nick "tester")
                (erc-networks--id (erc-networks--id-create 'GNU.chat)))
            (should (string= (erc--auth-source-search :user "#chan")
                             "foo"))))

        (ert-info ("Network wins")
          (let* ((erc-session-server "irc.gnu.org")
                 (erc-server-announced-name "my.gnu.org")
                 (erc-session-port 6697)
                 (erc-network 'GNU.chat)
                 (erc-server-current-nick "tester")
                 (erc-networks--id (erc-networks--id-create nil)))
            (should (string= (erc--auth-source-search :user "#chan")
                             "foo"))))

        (ert-info ("Announced wins")
          (let ((erc-session-server "irc.gnu.org")
                (erc-server-announced-name "my.gnu.org")
                (erc-session-port 6697)
                erc-network
                (erc-networks--id (erc-networks--id-create nil)))
            (should (string= (erc--auth-source-search :user "#chan")
                             "baz"))))))))

(ert-deftest erc-services-tests--auth-source-pass--announced ()
  (let ((store erc-join-tests--auth-source-pass-entries)
        (auth-sources '(password-store))
        (auth-source-do-cache nil))

    (cl-letf (((symbol-function 'auth-source-pass-parse-entry)
               (apply-partially #'erc-services-tests--asp-parse-entry store))
              ((symbol-function 'auth-source-pass-entries)
               (lambda () (mapcar #'car store))))

      (let* ((erc--isupport-params (make-hash-table))
             (erc-server-parameters '(("CHANTYPES" . "&#")))
             (erc--target (erc--target-from-string "&chan")))

        (ert-info ("Announced prioritized")

          (ert-info ("Announced wins")
            (let* ((erc-session-server "irc.gnu.org")
                   (erc-server-announced-name "my.gnu.org")
                   (erc-session-port 6697)
                   (erc-network 'GNU.chat)
                   (erc-server-current-nick "tester")
                   (erc-networks--id (erc-networks--id-create nil)))
              (should (string= (erc--auth-source-search :user "#chan")
                               "baz"))))

          (ert-info ("Peer next")
            (let* ((erc-server-announced-name "irc.gnu.org")
                   (erc-session-port 6697)
                   (erc-network 'GNU.chat)
                   (erc-server-current-nick "tester")
                   (erc-networks--id (erc-networks--id-create nil)))
              (should (string= (erc--auth-source-search :user "#chan")
                               "bar"))))

          (ert-info ("Network used as fallback")
            (let* ((erc-session-port 6697)
                   (erc-network 'GNU.chat)
                   (erc-server-current-nick "tester")
                   (erc-networks--id (erc-networks--id-create nil)))
              (should (string= (erc--auth-source-search :user "#chan")
                               "foo")))))))))

(ert-deftest erc-services-tests--auth-source-pass--overrides ()
  (let* ((store
          `(,@erc-join-tests--auth-source-pass-entries
            ("GNU.chat:6697/#chan"
             ("port" . "6697") ("user" . "#chan") (secret . "spam"))
            ("my.gnu.org:irc/#fsf"
             ("port" . "irc") ("user" . "#fsf") (secret . "42"))
            ("irc.gnu.org:6667"
             ("port" . "6667") (secret . "sesame"))
            ("MyHost:irc"
             ("port" . "irc") (secret . "456"))
            ("MyHost:6667"
             ("port" . "6667") (secret . "123"))))
         (auth-sources '(password-store))
         (auth-source-do-cache nil)
         (erc-session-server "irc.gnu.org")
         (erc-server-announced-name "my.gnu.org")
         (erc-network 'GNU.chat)
         (erc-server-current-nick "tester")
         (erc-networks--id (erc-networks--id-create nil))
         (erc-session-port 6667))

    (cl-letf (((symbol-function 'auth-source-pass-parse-entry)
               (apply-partially #'erc-services-tests--asp-parse-entry store))
              ((symbol-function 'auth-source-pass-entries)
               (lambda () (mapcar #'car store))))

      (ert-info ("More specific port")
        (let ((erc-session-port 6697))
          (should (string= (erc--auth-source-search :user "#chan") "spam"))))

      (ert-info ("Network wins")
        (should (string= (erc--auth-source-search :user '("#fsf")) "42")))

      (ert-info ("Actual override")
        (should (string= (erc--auth-source-search :port "6667") "sesame")))

      (ert-info ("Overrides don't interfere with post-processing")
        (should (string= (erc--auth-source-search :host "MyHost")
                         "123"))))))

;;;; The services module

(ert-deftest erc-nickserv-get-password ()
  (should erc-prompt-for-nickserv-password)
  (ert-with-temp-file netrc-file
    :prefix "erc-nickserv-get-password"
    :text (mapconcat 'identity
                     '("machine GNU/chat port 6697 user bob password spam"
                       "machine FSF.chat port 6697 user bob password sesame"
                       "machine MyHost port irc password 123")
                     "\n")

    (let* ((auth-sources (list netrc-file))
           (auth-source-do-cache nil)
           (erc-nickserv-passwords '((FSF.chat (("alice" . "foo")
                                                ("joe" . "bar")))))
           (erc-use-auth-source-for-nickserv-password t)
           (erc-session-server "irc.gnu.org")
           (erc-server-announced-name "my.gnu.org")
           (erc-network 'FSF.chat)
           (erc-server-current-nick "tester")
           (erc-networks--id (erc-networks--id-create nil))
           (erc-session-port 6697))

      (ert-info ("Lookup custom option")
        (should (string= (erc-nickserv-get-password "alice") "foo")))

      (ert-info ("Auth source")
        (ert-info ("Network")
          (should (string= (erc-nickserv-get-password "bob") "sesame")))

        (ert-info ("Network ID")
          (let ((erc-networks--id (erc-networks--id-create 'GNU/chat)))
            (should (string= (erc-nickserv-get-password "bob") "spam")))))

      (ert-info ("Read input")
        (should (string=
                 (ert-simulate-keys "baz\r" (erc-nickserv-get-password "mike"))
                 "baz")))

      (ert-info ("Failed")
        (should-not (ert-simulate-keys "\r"
                      (erc-nickserv-get-password "fake")))))))


;;; erc-services-tests.el ends here
