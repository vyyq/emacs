;;; erc-scenarios.el --- user test cases for ERC -*- lexical-binding: t -*-

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
;;
;; These are e2e-ish test cases primarily intended to assert core,
;; fundamental behavior expected of any modern IRC client.  Tests may
;; also simulate specific scenarios drawn from bug reports.  Incoming
;; messages are provided by playback scripts resembling I/O logs.  In
;; place of time stamps, they have time deltas, which are used to
;; govern the test server in a fashion reminiscent of music rolls (or
;; the script(1) UNIX program).  These scripts can be found in the
;; accompanying erc-scenarios-resources directory.
;;
;; Isolation:
;;
;; The set of enabled modules is shared among all tests.  The function
;; `erc-update-modules' activates them (as minor modes), but it never
;; deactivates them.  So there's no going back, and let-binding
;; `erc-modules' is useless.  The safest route is therefore to (1)
;; assume the set of default modules is already activated or will be
;; over the course of the test session and (2) let-bind relevant user
;; options as needed.  For example, to limit the damage of
;; `erc-autojoin-channels-alist' to a given test, assume the
;; `erc-join' library has already been loaded or will be on the next
;; call to `erc-open'.  And then simply let-bind
;; `erc-autojoin-channels-alist' for the duration of the test.
;;
;; Playing nice:
;;
;; Right now, these tests all rely on an ugly fixture macro named
;; `erc-scenarios-common-with-cleanup', which is defined in the
;; companion file erc-scenarios-common.el.  It helps restore (but not
;; really prepare) the environment by destroying any stray processes
;; or buffers named in the first argument, a `let*'-style VAR-LIST.
;; Relying on such a macro is unfortunate because in many ways it
;; actually hampers readability by favoring magic over verbosity.  But
;; without it (or something similar), any failing test would cause all
;; subsequent tests in this file to fail in a cascading manner (making
;; all but the first backtrace useless).
;;
;; Misc:
;;
;; Note that in the following examples, nicknames Alice and Bob are
;; always associated with the fake network FooNet, while nicks Joe and
;; Mike are always on BarNet.
;;

;;; Code:
(require 'ert-x) ; cl-lib

(eval-and-compile
  (let ((dir (getenv "EMACS_TEST_DIRECTORY")))
    (when dir (load (concat dir "/lisp/erc/erc-scenarios-common") nil t))))

(require 'erc-d)
(require 'erc-scenarios-common)
(require 'erc-backend)

(declare-function erc-network-name "erc-networks")
(declare-function erc-network "erc-networks")
(defvar erc-autojoin-channels-alist)
(defvar erc-network)

;; Two networks, same channel name, no confusion (no bouncer).  Some
;; of this draws from bug#47522 "foil-in-server-buf".  It shows that
;; disambiguation-related changes added for bug#48598 are not specific
;; to bouncers.

(defun erc-scenarios-common--base-association-multi-net (second-join)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/association/multi-net")
       (erc-server-flood-penalty 0.1)
       (erc-d-linger-secs 1)
       (dumb-server-foonet-buffer (get-buffer-create "*server-foonet*"))
       (dumb-server-barnet-buffer (get-buffer-create "*server-barnet*"))
       (dumb-server-foonet (erc-d-run "localhost" t "server-foonet" 'foonet))
       (dumb-server-barnet (erc-d-run "localhost" t "server-barnet" 'barnet))
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect to foonet, join #chan")
      (with-current-buffer
          (erc :server "127.0.0.1"
               :port (process-contact dumb-server-foonet :service)
               :nick "tester"
               :password "changeme"
               :full-name "tester")
        (funcall expect 3 "debug mode")
        (erc-cmd-JOIN "#chan")))

    (erc-d-t-wait-for 2 (get-buffer "#chan"))

    (ert-info ("Connect to barnet, join #chan")
      (with-current-buffer
          (erc :server "127.0.0.1"
               :port (process-contact dumb-server-barnet :service)
               :nick "tester"
               :password "changeme"
               :full-name "tester")
        (funcall expect 1 "debug mode")))

    (funcall second-join)

    (erc-d-t-wait-for 3 (get-buffer "#chan@barnet"))

    (erc-d-t-wait-for 2 "Buf #chan now #chan@foonet"
      (and (get-buffer "#chan@foonet") (not (get-buffer "#chan"))))

    (ert-info ("All #chan@foonet output consumed")
      (with-current-buffer "#chan@foonet"
        (funcall expect 3 "bob")
        (funcall expect 3 "was created on")
        (funcall expect 3 "prosperous")))

    (ert-info ("All #chan@barnet output consumed")
      (with-current-buffer "#chan@barnet"
        (funcall expect 3 "mike")
        (funcall expect 3 "was created on")
        (funcall expect 3 "ingenuous")))))

(ert-deftest erc-scenarios-base-association-multi-net--baseline ()
  (erc-scenarios-common--base-association-multi-net
   (lambda () (with-current-buffer "barnet" (erc-cmd-JOIN "#chan")))))

;; The /join command only targets the current buffer's process.  This
;; recasts scenario bug#48598 "ambiguous-join" (which was based on
;; bug#47522) to show that issuing superfluous /join commands
;; (apparently fairly common) is benign.

(ert-deftest erc-scenarios-base-association-multi-net--ambiguous-join ()
  (erc-scenarios-common--base-association-multi-net
   (lambda ()
     (ert-info ("Nonsensical JOIN attempts silently dropped.")
       (with-current-buffer "foonet" (erc-cmd-JOIN "#chan"))
       (sit-for 0.1)
       (with-current-buffer "#chan" (erc-cmd-JOIN "#chan"))
       (sit-for 0.1)
       (erc-d-t-wait-for 2 (get-buffer "#chan"))
       (erc-d-t-wait-for 1 "Only one #chan buffer exists"
         (should (equal (erc-scenarios-common-buflist "#chan")
                        (list (get-buffer "#chan")))))
       (with-current-buffer "*server-barnet*"
         (erc-d-t-absent-for 0.1 "JOIN"))
       (with-current-buffer "barnet" (erc-cmd-JOIN "#chan"))))))

;; One network, two simultaneous connections, no IDs.
;; Reassociates on reconnect with and without server buffer.

(defun erc-scenarios-common--base-association-same-network (after)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/association/same-network")
       (dumb-server (erc-d-run "localhost" t 'tester 'chester 'tester-again))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter))
       (erc-server-flood-penalty 0.5)
       (erc-server-flood-margin 30))

    (ert-info ("Connect to foonet with nick tester")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :password "changeme"
                                :full-name "tester")
        (erc-scenarios-common-assert-initial-buf-name nil port)
        (erc-d-t-wait-for 5 (eq erc-network 'foonet))))

    (ert-info ("Connect to foonet with nick chester")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "chester"
                                :password "changeme"
                                :full-name "chester")
        (erc-scenarios-common-assert-initial-buf-name nil port)))

    (erc-d-t-wait-for 3 "Dialed Buflist is Empty"
      (not (erc-scenarios-common-buflist "127.0.0.1")))

    (with-current-buffer "foonet/tester"
      (funcall expect 3 "debug mode")
      (erc-cmd-JOIN "#chan"))

    (erc-d-t-wait-for 10 (get-buffer "#chan@foonet/tester"))
    (with-current-buffer "foonet/chester" (funcall expect 3 "debug mode"))
    (erc-d-t-wait-for 10 (get-buffer "#chan@foonet/chester"))

    (ert-info ("Nick tester sees other nick chester in channel")
      (with-current-buffer "#chan@foonet/tester"
        (funcall expect 5 "chester")
        (funcall expect 5 "find the forester")
        (erc-cmd-QUIT "")))

    (ert-info ("Nick chester sees other nick tester in same channel")
      (with-current-buffer  "#chan@foonet/chester"
        (funcall expect 5 "tester")
        (funcall expect 5 "find the forester")))

    (funcall after expect)))

(ert-deftest erc-scenarios-base-association-same-network--reconnect-one ()
  (erc-scenarios-common--base-association-same-network
   (lambda (expect)

     (ert-info ("Connection tester reconnects")
       (with-current-buffer "foonet/tester"
         (erc-d-t-wait-for 10 (not (erc-server-process-alive)))
         (funcall expect 10 "*** ERC finished")
         (erc-cmd-RECONNECT)
         (funcall expect 5 "debug mode")))

     (ert-info ("Reassociated to same channel")
       (with-current-buffer "#chan@foonet/tester"
         (funcall expect 5 "chester")
         (funcall expect 5 "welcome again")
         (erc-cmd-QUIT "")))

     (with-current-buffer "#chan@foonet/chester"
       (funcall expect 5 "tester")
       (funcall expect 5 "welcome again")
       (funcall expect 5 "welcome again")
       (erc-cmd-QUIT "")))))

(ert-deftest erc-scenarios-base-association-same-network--new-buffer ()
  (erc-scenarios-common--base-association-same-network
   (lambda (expect)

     (ert-info ("Tester kills buffer and connects from scratch")

       (let (port)
         (with-current-buffer "foonet/tester"
           (erc-d-t-wait-for 10 (not (erc-server-process-alive)))
           (funcall expect 10 "*** ERC finished")
           (setq port erc-session-port)
           (kill-buffer))

         (with-current-buffer (erc :server "127.0.0.1"
                                   :port port
                                   :nick "tester"
                                   :password "changeme"
                                   :full-name "tester")

           (erc-d-t-wait-for 5 (eq erc-network 'foonet)))))

     (with-current-buffer "foonet/tester" (funcall expect 3 "debug mode"))

     (ert-info ("Reassociated to same channel")
       (with-current-buffer "#chan@foonet/tester"
         (funcall expect 5 "chester")
         (funcall expect 5 "welcome again")
         (erc-cmd-QUIT "")))

     (with-current-buffer "#chan@foonet/chester"
       (funcall expect 5 "tester")
       (funcall expect 5 "welcome again")
       (funcall expect 5 "welcome again")
       (erc-cmd-QUIT "")))))

;; Playback for same channel on two networks routed correctly.
;; Originally from Bug#48598: 28.0.50; buffer-naming collisions
;; involving bouncers in ERC.

(ert-deftest erc-scenarios-base-association-bouncer-history ()
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/association/bouncer-history")
       (erc-d-t-cleanup-sleep-secs 1)
       (erc-d-linger-secs 1)
       (dumb-server (erc-d-run "localhost" t 'foonet 'barnet))
       (port (process-contact dumb-server :service))
       (erc-server-flood-penalty 0.5)
       (expect (erc-d-t-make-expecter))
       erc-autojoin-channels-alist
       erc-server-buffer-foo erc-server-process-foo
       erc-server-buffer-bar erc-server-process-bar)

    (ert-info ("Connect to foonet")
      (with-current-buffer
          (setq erc-server-buffer-foo (erc :server "127.0.0.1"
                                           :port port
                                           :nick "tester"
                                           :password "foonet:changeme"
                                           :full-name "tester"))
        (setq erc-server-process-foo erc-server-process)
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
        (funcall expect 5 "foonet")))

    (erc-d-t-wait-for 5 (get-buffer "#chan"))

    (ert-info ("Connect to barnet")
      (with-current-buffer
          (setq erc-server-buffer-bar (erc :server "127.0.0.1"
                                           :port port
                                           :nick "tester"
                                           :password "barnet:changeme"
                                           :full-name "tester"))
        (setq erc-server-process-bar erc-server-process)
        (erc-d-t-wait-for 5 "Temporary name assigned"
          (string= (buffer-name) (format "127.0.0.1:%d" port)))
        (funcall expect 5 "barnet")))

    (ert-info ("Server buffers are unique")
      (should-not (eq erc-server-buffer-foo erc-server-buffer-bar)))

    (ert-info ("Networks correctly determined and adopted as buffer names")
      (with-current-buffer erc-server-buffer-foo
        (erc-d-t-wait-for 3 "network name foonet becomes buffer name"
          (and (eq (erc-network) 'foonet) (string= (buffer-name) "foonet"))))
      (with-current-buffer erc-server-buffer-bar
        (erc-d-t-wait-for 3 "network name barnet becomes buffer name"
          (and (eq (erc-network) 'barnet) (string= (buffer-name) "barnet")))))

    (erc-d-t-wait-for 5 (get-buffer "#chan@barnet"))

    (ert-info ("Two channel buffers created, original #chan renamed")
      (should (= 4 (length (erc-buffer-list))))
      (should (equal (list (get-buffer "#chan@barnet")
                           (get-buffer "#chan@foonet"))
                     (erc-scenarios-common-buflist "#chan"))))

    (ert-info ("#chan@foonet is exclusive, no cross-contamination")
      (with-current-buffer "#chan@foonet"
        (erc-d-t-search-for 1 "<bob>")
        (erc-d-t-absent-for 0.1 "<joe>")
        (should (eq erc-server-process erc-server-process-foo))))

    (ert-info ("#chan@barnet is exclusive, no cross-contamination")
      (with-current-buffer "#chan@barnet"
        (erc-d-t-search-for 1 "<joe>")
        (erc-d-t-absent-for 0.1 "<bob>")
        (should (eq erc-server-process erc-server-process-bar))))

    (ert-info ("All output sent")
      (with-current-buffer "#chan@foonet"
        (while (accept-process-output erc-server-process-foo))
        (erc-d-t-search-for 3 "please your lordship"))
      (with-current-buffer "#chan@barnet"
        (while (accept-process-output erc-server-process-bar))
        (erc-d-t-search-for 3 "I'll bid adieu")))))

(cl-defun erc-scenarios-common--base-network-id-bouncer
    ((&key autop foo-id bar-id after
           &aux
           (foo-id (and foo-id 'oofnet))
           (bar-id (and bar-id 'rabnet))
           (serv-buf-foo (if foo-id "oofnet" "foonet"))
           (serv-buf-bar (if bar-id "rabnet" "barnet"))
           (chan-buf-foo (if foo-id "#chan@oofnet" "#chan@foonet"))
           (chan-buf-bar (if bar-id "#chan@rabnet" "#chan@barnet")))
     &rest dialogs)
  "Ensure retired option `erc-rename-buffers' is now the default behavior.
The option `erc-rename-buffers' is now deprecated and on by default, so
this now just asserts baseline behavior.  Originally from scenario
clash-of-chans/rename-buffers as explained in Bug#48598: 28.0.50;
buffer-naming collisions involving bouncers in ERC."
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/network-id/bouncer")
       (erc-d-t-cleanup-sleep-secs 1)
       (erc-server-flood-penalty 0.1)
       (dumb-server (apply #'erc-d-run "localhost" t dialogs))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter))
       (erc-server-auto-reconnect autop)
       erc-server-buffer-foo erc-server-process-foo
       erc-server-buffer-bar erc-server-process-bar)

    (ert-info ("Connect to foonet")
      (with-current-buffer
          (setq erc-server-buffer-foo (erc :server "127.0.0.1"
                                           :port port
                                           :nick "tester"
                                           :password "foonet:changeme"
                                           :full-name "tester"
                                           :id foo-id))
        (setq erc-server-process-foo erc-server-process)
        (erc-scenarios-common-assert-initial-buf-name foo-id port)
        (erc-d-t-wait-for 3 (eq (erc-network) 'foonet))
        (erc-d-t-wait-for 3 (string= (buffer-name) serv-buf-foo))
        (funcall expect 5 "foonet")))

    (ert-info ("Join #chan@foonet")
      (with-current-buffer erc-server-buffer-foo (erc-cmd-JOIN "#chan"))
      (with-current-buffer (erc-d-t-wait-for 5 (get-buffer "#chan"))
        (funcall expect 5 "<alice>")))

    (ert-info ("Connect to barnet")
      (with-current-buffer
          (setq erc-server-buffer-bar (erc :server "127.0.0.1"
                                           :port port
                                           :nick "tester"
                                           :password "barnet:changeme"
                                           :full-name "tester"
                                           :id bar-id))
        (setq erc-server-process-bar erc-server-process)
        (erc-scenarios-common-assert-initial-buf-name bar-id port)
        (erc-d-t-wait-for 3 (eq (erc-network) 'barnet))
        (erc-d-t-wait-for 3 (string= (buffer-name) serv-buf-bar))
        (funcall expect 5 "barnet")))

    (ert-info ("Server buffers are unique, no names based on IPs")
      (should-not (eq erc-server-buffer-foo erc-server-buffer-bar))
      (should-not (erc-scenarios-common-buflist "127.0.0.1")))

    (ert-info ("Join #chan@barnet")
      (with-current-buffer erc-server-buffer-bar (erc-cmd-JOIN "#chan")))

    (erc-d-t-wait-for 5 "Exactly 2 #chan-prefixed buffers exist"
      (equal (list (get-buffer chan-buf-bar)
                   (get-buffer chan-buf-foo))
             (erc-scenarios-common-buflist "#chan")))

    (ert-info ("#chan@<esid> is exclusive to foonet")
      (with-current-buffer chan-buf-foo
        (erc-d-t-search-for 1 "<bob>")
        (erc-d-t-absent-for 0.1 "<joe>")
        (should (eq erc-server-process erc-server-process-foo))
        (while (accept-process-output erc-server-process-foo))
        (erc-d-t-search-for 1 "ape is dead")
        (should-not (erc-server-process-alive))))

    (ert-info ("#chan@<esid> is exclusive to barnet")
      (with-current-buffer chan-buf-bar
        (erc-d-t-search-for 1 "<joe>")
        (erc-d-t-absent-for 0.1 "<bob>")
        (should (eq erc-server-process erc-server-process-bar))
        (while (accept-process-output erc-server-process-bar))
        (erc-d-t-search-for 1 "keeps you from dishonour")
        (should-not (erc-server-process-alive))))

    (when after (funcall after))))

(ert-deftest erc-scenarios-base-network-id-bouncer--base ()
  (erc-scenarios-common--base-network-id-bouncer () 'foonet 'barnet))

(ert-deftest erc-scenarios-base-network-id-bouncer--id-foo ()
  (erc-scenarios-common--base-network-id-bouncer '(:foo-id t) 'foonet 'barnet))

(ert-deftest erc-scenarios-base-network-id-bouncer--id-bar ()
  (erc-scenarios-common--base-network-id-bouncer '(:bar-id t) 'foonet 'barnet))

(ert-deftest erc-scenarios-base-network-id-bouncer--both ()
  (erc-scenarios-common--base-network-id-bouncer '(:foo-id t :bar-id t)
                                                 'foonet 'barnet))

(defun erc-scenarios--clash-rename-pass-handler (dialog exchange)
  (when (eq (erc-d-dialog-name dialog) 'stub-again)
    (let* ((match (erc-d-exchange-match exchange 1))
           (sym (if (string= match "foonet") 'foonet-again 'barnet-again)))
      (should (member match (list "foonet" "barnet")))
      (erc-d-load-replacement-dialog dialog sym 1))))

(defun erc-scenarios-common--base-network-id-bouncer--reconnect (foo-id bar-id)
  (let ((erc-d-spec-vars '((token . (group (| "barnet" "foonet")))))
        (erc-d-match-handlers
         ;; Auto reconnect is nondeterministic, so let computer decide
         (list :pass #'erc-scenarios--clash-rename-pass-handler))
        (after
         (lambda ()
           ;; Simulate disconnection and `erc-server-auto-reconnect'
           (ert-info ("Reconnect to foonet and barnet back-to-back")
             (with-current-buffer (if foo-id "oofnet" "foonet")
               (erc-d-t-wait-for 5 (erc-server-process-alive)))
             (with-current-buffer (if bar-id "rabnet" "barnet")
               (erc-d-t-wait-for 5 (erc-server-process-alive))))

           (ert-info ("#chan@foonet is exclusive to foonet")
             (with-current-buffer (if foo-id "#chan@oofnet" "#chan@foonet")
               (erc-d-t-search-for 1 "<alice>")
               (erc-d-t-absent-for 0.1 "<joe>")
               (while (accept-process-output erc-server-process))
               (erc-d-t-search-for 3 "please your lordship")))

           (ert-info ("#chan@barnet is exclusive to barnet")
             (with-current-buffer (if bar-id "#chan@rabnet" "#chan@barnet")
               (erc-d-t-search-for 1 "<joe>")
               (erc-d-t-absent-for 0.1 "<bob>")
               (while (accept-process-output erc-server-process))
               (erc-d-t-search-for 1 "much in private")))

           ;; XXX this is important (reconnects overlapped, so we'd get
           ;; chan@127.0.0.1:6667)
           (should-not (erc-scenarios-common-buflist "127.0.0.1"))
           ;; Reconnection order doesn't matter here because session objects
           ;; are persisted, meaning original timestamps preserved.
           (should (equal (list (get-buffer (if bar-id "#chan@rabnet"
                                              "#chan@barnet"))
                                (get-buffer (if foo-id "#chan@oofnet"
                                              "#chan@foonet")))
                          (erc-scenarios-common-buflist "#chan"))))))
    (erc-scenarios-common--base-network-id-bouncer
     (list :autop t :foo-id foo-id :bar-id bar-id :after after)
     'foonet-drop 'barnet-drop
     'stub-again 'stub-again
     'foonet-again 'barnet-again)))

(ert-deftest erc-scenarios-base-network-id-bouncer--reconnect-base ()
  (erc-scenarios-common--base-network-id-bouncer--reconnect nil nil))

(ert-deftest erc-scenarios-base-network-id-bouncer--reconnect-id-foo ()
  (erc-scenarios-common--base-network-id-bouncer--reconnect 'foo-id nil))

(ert-deftest erc-scenarios-base-network-id-bouncer--reconnect-id-bar ()
  (erc-scenarios-common--base-network-id-bouncer--reconnect nil 'bar-id))

(ert-deftest erc-scenarios-base-network-id-bouncer--reconnect-both ()
  (erc-scenarios-common--base-network-id-bouncer--reconnect 'foo-id 'bar-id))

;; Ensure deprecated option still respected when old default value
;; explicitly set ("respected" in the sense of having names reflect
;; dialed TCP endpoints with possible uniquifiers but without any of
;; the old issues, pre-bug#48598).

(defun erc-scenarios-common--base-compat-no-rename-bouncer (dialogs auto more)
  (erc-scenarios-common-with-cleanup
      ;; These actually *are* (assigned-)network-id related because
      ;; our kludge assigns one after the fact.
      ((erc-scenarios-common-dialog "base/network-id/bouncer")
       (erc-d-t-cleanup-sleep-secs 1)
       (erc-server-flood-penalty 0.1)
       (dumb-server (apply #'erc-d-run "localhost" t dialogs))
       (port (process-contact dumb-server :service))
       (chan-buf-foo (format "#chan@127.0.0.1:%d" port))
       (chan-buf-bar (format "#chan@127.0.0.1:%d<2>" port))
       (expect (erc-d-t-make-expecter))
       (erc-server-auto-reconnect auto)
       erc-server-buffer-foo erc-server-process-foo
       erc-server-buffer-bar erc-server-process-bar)

    (ert-info ("Connect to foonet")
      (with-current-buffer
          (setq erc-server-buffer-foo (erc :server "127.0.0.1"
                                           :port port
                                           :nick "tester"
                                           :password "foonet:changeme"
                                           :full-name "tester"
                                           :id nil))
        (setq erc-server-process-foo erc-server-process)
        (erc-d-t-wait-for 3 (eq (erc-network) 'foonet))
        (erc-d-t-wait-for 3 "Final buffer name determined"
          (string= (buffer-name) (format "127.0.0.1:%d" port)))
        (funcall expect 5 "foonet")))

    (ert-info ("Join #chan@foonet")
      (with-current-buffer erc-server-buffer-foo (erc-cmd-JOIN "#chan"))
      (with-current-buffer (erc-d-t-wait-for 5 (get-buffer "#chan"))
        (funcall expect 5 "<alice>")))

    (ert-info ("Connect to barnet")
      (with-current-buffer
          (setq erc-server-buffer-bar (erc :server "127.0.0.1"
                                           :port port
                                           :nick "tester"
                                           :password "barnet:changeme"
                                           :full-name "tester"
                                           :id nil))
        (setq erc-server-process-bar erc-server-process)
        (erc-d-t-wait-for 3 (eq (erc-network) 'barnet))
        (erc-d-t-wait-for 3 "Final buffer name determined"
          (string= (buffer-name) (format "127.0.0.1:%d<2>" port)))
        (funcall expect 5 "barnet")))

    (ert-info ("Server buffers are unique, no names based on IPs")
      (should-not (eq erc-server-buffer-foo erc-server-buffer-bar))
      (should (equal (erc-scenarios-common-buflist "127.0.0.1")
                     (list (get-buffer (format "127.0.0.1:%d<2>" port))
                           (get-buffer (format "127.0.0.1:%d" port))))))

    (ert-info ("Join #chan@barnet")
      (with-current-buffer erc-server-buffer-bar (erc-cmd-JOIN "#chan")))

    (erc-d-t-wait-for 5 "Exactly 2 #chan-prefixed buffers exist"
      (equal (list (get-buffer chan-buf-bar)
                   (get-buffer chan-buf-foo))
             (erc-scenarios-common-buflist "#chan")))

    (ert-info ("#chan@127.0.0.1:$port is exclusive to foonet")
      (with-current-buffer chan-buf-foo
        (erc-d-t-search-for 1 "<bob>")
        (erc-d-t-absent-for 0.1 "<joe>")
        (should (eq erc-server-process erc-server-process-foo))
        (while (accept-process-output erc-server-process-foo))
        (erc-d-t-search-for 1 "ape is dead")
        (should-not (erc-server-process-alive))))

    (ert-info ("#chan@127.0.0.1:$port<2> is exclusive to barnet")
      (with-current-buffer chan-buf-bar
        (erc-d-t-search-for 1 "<joe>")
        (erc-d-t-absent-for 0.1 "<bob>")
        (should (eq erc-server-process erc-server-process-bar))
        (while (accept-process-output erc-server-process-bar))
        (erc-d-t-search-for 1 "keeps you from dishonour")
        (should-not (erc-server-process-alive))))

    (when more (funcall more))))

(ert-deftest erc-scenarios-base-compat-no-rename-bouncer--basic ()
  (with-suppressed-warnings ((obsolete erc-rename-buffers))
    (let (erc-rename-buffers)
      (erc-scenarios-common--base-compat-no-rename-bouncer
       '(foonet barnet) nil nil))))

(ert-deftest erc-scenarios-base-compat-no-rename-bouncer--reconnect ()
  (let ((erc-d-spec-vars '((token . (group (| "barnet" "foonet")))))
        (erc-d-match-handlers
         (list :pass #'erc-scenarios--clash-rename-pass-handler))
        (dialogs '(foonet-drop barnet-drop stub-again stub-again
                               foonet-again barnet-again))
        (after
         (lambda ()
           (pcase-let* ((`(,barnet ,foonet)
                         (erc-scenarios-common-buflist "127.0.0.1"))
                        (port (process-contact (with-current-buffer foonet
                                                 erc-server-process)
                                               :service)))

             (ert-info ("Sanity check: barnet retains uniquifying suffix")
               (should (string-suffix-p "<2>" (buffer-name barnet))))

             ;; Simulate disconnection and `erc-server-auto-reconnect'
             (ert-info ("Reconnect to foonet and barnet back-to-back")
               (with-current-buffer foonet
                 (erc-d-t-wait-for 5 (erc-server-process-alive)))
               (with-current-buffer barnet
                 (erc-d-t-wait-for 5 (erc-server-process-alive))))

             (ert-info ("#chan@127.0.0.1:<port> is exclusive to foonet")
               (with-current-buffer  (format "#chan@127.0.0.1:%d" port)
                 (erc-d-t-search-for 1 "<alice>")
                 (erc-d-t-absent-for 0.1 "<joe>")
                 (while (accept-process-output erc-server-process))
                 (erc-d-t-search-for 3 "please your lordship")))

             (ert-info ("#chan@barnet is exclusive to barnet")
               (with-current-buffer  (format "#chan@127.0.0.1:%d<2>" port)
                 (erc-d-t-search-for 1 "<joe>")
                 (erc-d-t-absent-for 0.1 "<bob>")
                 (while (accept-process-output erc-server-process))
                 (erc-d-t-search-for 1 "much in private")))

             ;; Ordering deterministic here even though not so for reconnect
             (should (equal (list barnet foonet)
                            (erc-scenarios-common-buflist "127.0.0.1")))
             (should (equal (list
                             (get-buffer (format "#chan@127.0.0.1:%d<2>" port))
                             (get-buffer (format "#chan@127.0.0.1:%d" port)))
                            (erc-scenarios-common-buflist "#chan")))))))

    (with-suppressed-warnings ((obsolete erc-rename-buffers))
      (let (erc-rename-buffers)
        (erc-scenarios-common--base-compat-no-rename-bouncer dialogs
                                                             'auto after)))))

;; The added complexity of a request handler definitely stinks. But on
;; some machines, the ordering from the selector is nondeterministic,
;; whereas normally, the filter for the last process created (in the
;; code) gets all the initial attention. FIXME delete obsolete comment

(defun erc-scenarios--rebuffed-gapless-pass-handler (dialog exchange)
  (when (eq (erc-d-dialog-name dialog) 'pass-stub)
    (let* ((match (erc-d-exchange-match exchange 1))
           (sym (if (string= match "foonet") 'foonet 'barnet)))
      (should (member match (list "foonet" "barnet")))
      (erc-d-load-replacement-dialog dialog sym 1))))

(ert-deftest erc-scenarios-base-gapless-connect ()
  "Back-to-back entry-point invocations happen successfully.
Originally from scenario rebuffed/gapless as explained in Bug#48598:
28.0.50; buffer-naming collisions involving bouncers in ERC."
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/gapless-connect")
       (erc-server-flood-penalty 0.1)
       (erc-d-linger-secs 4)
       (erc-server-flood-penalty erc-server-flood-penalty)
       (erc-d-spec-vars '((token . (group (| "barnet" "foonet")))))
       (erc-d-match-handlers
        (list :pass #'erc-scenarios--rebuffed-gapless-pass-handler))
       (dumb-server (erc-d-run "localhost" t
                               'pass-stub 'pass-stub 'barnet 'foonet))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter))
       erc-autojoin-channels-alist
       erc-server-buffer-foo
       erc-server-buffer-bar)

    (ert-info ("Connect twice to same endpoint without pausing")
      (setq erc-server-buffer-foo (erc :server "127.0.0.1"
                                       :port port
                                       :nick "tester"
                                       :password "foonet:changeme"
                                       :full-name "tester")
            erc-server-buffer-bar (erc :server "127.0.0.1"
                                       :port port
                                       :nick "tester"
                                       :password "barnet:changeme"
                                       :full-name "tester")))

    (ert-info ("Returned server buffers are unique")
      (should-not (eq erc-server-buffer-foo erc-server-buffer-bar)))

    (ert-info ("Both connections still alive")
      (should (get-process (format "erc-127.0.0.1-%d" port)))
      (should (get-process (format "erc-127.0.0.1-%d<1>" port))))

    (with-current-buffer erc-server-buffer-bar
      (funcall expect 2 "marked as being away"))

    (with-current-buffer (erc-d-t-wait-for 20 (get-buffer "#bar"))
      (while (accept-process-output erc-server-process))
      (funcall expect 2 "was created on")
      (funcall expect 2 "his second fit"))

    (with-current-buffer (erc-d-t-wait-for 20 (get-buffer "#foo"))
      (while (accept-process-output erc-server-process))
      (funcall expect 2 "was created on")
      (funcall expect 2 "no use of him"))))

(defun erc-scenarios-common--base-reuse-buffers-server-buffers (&optional more)
  "Show that `erc-reuse-buffers' doesn't affect server buffers.
Overlaps some with `clash-of-chans/uniquify'.  Adapted from
rebuffed/reuseless, described in Bug#48598: 28.0.50; buffer-naming
collisions involving bouncers in ERC.  Run EXTRA."
  (erc-scenarios-common-with-cleanup
      ((erc-d-linger-secs 1)
       (dumb-server (erc-d-run "localhost" t 'foonet 'barnet))
       (port (process-contact dumb-server :service))
       erc-autojoin-channels-alist)

    (ert-info ("Connect to foonet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :password "foonet:changeme"
                                :full-name "tester")
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
        (erc-d-t-search-for 2 "marked as being away")))

    (ert-info ("Connect to barnet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :password "barnet:changeme"
                                :full-name "tester")
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
        (erc-d-t-search-for 2 "marked as being away")))

    (erc-d-t-wait-for 2 (get-buffer "foonet"))
    (erc-d-t-wait-for 2 (get-buffer "barnet"))

    (ert-info ("Server buffers are unique, no IP-based names")
      (should-not (eq (get-buffer "foonet") (get-buffer "barnet")))
      (should-not (erc-scenarios-common-buflist "127.0.0.1")))

    (when more (funcall more))))

(ert-deftest erc-scenarios-base-reuse-buffers-server-buffers--enabled ()
  (should erc-reuse-buffers)
  (let ((erc-scenarios-common-dialog "base/reuse-buffers/server-buffers"))
    (erc-scenarios-common--base-reuse-buffers-server-buffers)))

(ert-deftest erc-scenarios-base-reuse-buffers-server-buffers--disabled ()
  (should erc-reuse-buffers)
  (let ((erc-scenarios-common-dialog "base/reuse-buffers/server-buffers")
        erc-reuse-buffers)
    (erc-scenarios-common--base-reuse-buffers-server-buffers)))

;; The server changes your nick just after registration.

(ert-deftest erc-scenarios-base-renick-self-auto ()
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/renick/self")
       (erc-d-linger-secs 0.1)
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'auto))
       (port (process-contact dumb-server :service))
       erc-autojoin-channels-alist
       erc-server-buffer-foo)

    (ert-info ("Connect to foonet")
      (setq erc-server-buffer-foo (erc :server "127.0.0.1"
                                       :port port
                                       :nick "tester"
                                       :password "foonet:changeme"
                                       :full-name "tester"))
      (with-current-buffer erc-server-buffer-foo
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))))

    (with-current-buffer (erc-d-t-wait-for 3 (get-buffer "foonet"))
      (erc-d-t-search-for 10 "Your new nickname is dummy"))

    (ert-info ("Joined by bouncer to #foo, own nick present")
      (with-current-buffer (erc-d-t-wait-for 1 (get-buffer "#foo"))
        (erc-d-t-search-for 10 "dummy")
        (erc-d-t-search-for 10 "On Thursday")))))

;; You change your nickname manually in a server buffer; a message is
;; printed in channel buffers.

(ert-deftest erc-scenarios-base-renick-self-manual ()
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/renick/self")
       (erc-d-linger-secs 0.1)
       (erc-server-flood-penalty 0.1)
       (dumb-server (erc-d-run "localhost" t 'manual))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter))
       erc-autojoin-channels-alist
       erc-server-buffer-foo)

    (ert-info ("Connect to foonet")
      (setq erc-server-buffer-foo (erc :server "127.0.0.1"
                                       :port port
                                       :nick "tester"
                                       :password "foonet:changeme"
                                       :full-name "tester"))
      (with-current-buffer erc-server-buffer-foo
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))))

    (erc-d-t-wait-for 3 (get-buffer "foonet"))

    (ert-info ("Joined by bouncer to #foo, own nick present")
      (with-current-buffer (erc-d-t-wait-for 1 (get-buffer "#foo"))
        (funcall expect 5 "tester")
        (funcall expect 5 "On Thursday")
        (erc-with-server-buffer (erc-cmd-NICK "dummy"))
        (funcall expect 5 "Your new nickname is dummy")
        (funcall expect 5 "<bob> dummy: Hi")
        ;; Regression in which changing a nick would trigger #foo@foonet
        (erc-d-t-ensure-for 0.4 (equal (buffer-name) "#foo"))))))

;; You connect to the same network with two different nicks.  You
;; manually change the first nick at some point, and buffer names are
;; updated correctly.

(ert-deftest erc-scenarios-base-renick-self-qualified ()
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/renick/self")
       (dumb-server (erc-d-run "localhost" t 'qual-tester 'qual-chester))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter))
       (erc-server-flood-penalty 0.1)
       (erc-server-flood-margin 30)
       erc-serv-buf-a erc-serv-buf-b)

    (ert-info ("Connect to foonet with nick tester")
      (with-current-buffer
          (setq erc-serv-buf-a (erc :server "127.0.0.1"
                                    :port port
                                    :nick "tester"
                                    :password "changeme"
                                    :full-name "tester"))
        (erc-d-t-wait-for 5 (eq erc-network 'foonet))))

    (ert-info ("Connect to foonet with nick chester")
      (with-current-buffer
          (setq erc-serv-buf-b (erc :server "127.0.0.1"
                                    :port port
                                    :nick "chester"
                                    :password "changeme"
                                    :full-name "chester"))))

    (erc-d-t-wait-for 3 "Dialed Buflist is Empty"
      (not (erc-scenarios-common-buflist "127.0.0.1")))

    (with-current-buffer  "foonet/tester"
      (funcall expect 3 "debug mode")
      (erc-cmd-JOIN "#chan"))

    (with-current-buffer  "foonet/chester"
      (funcall expect 3 "debug mode")
      (erc-cmd-JOIN "#chan"))

    (erc-d-t-wait-for 10 (get-buffer "#chan@foonet/tester"))
    (erc-d-t-wait-for 10 (get-buffer "#chan@foonet/chester"))

    (ert-info ("Greets other nick in same channel")
      (with-current-buffer "#chan@foonet/tester"
        (funcall expect 5 "<bob> chester, welcome!")
        (erc-cmd-NICK "dummy")
        (funcall expect 5 "Your new nickname is dummy")
        (funcall expect 5 "find the forester")
        (erc-d-t-wait-for 5 (string= (buffer-name) "#chan@foonet/dummy"))))

    (ert-info ("Renick propagated throughout all buffers of process")
      (should-not (get-buffer "#chan@foonet/tester"))
      (should-not (get-buffer "foonet/tester"))
      (should (get-buffer "foonet/dummy")))))

;; When a channel user changes their nick, any query buffers for them
;; are updated.

(ert-deftest erc-scenarios-base-renick-queries-solo ()

  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/renick/queries")
       (erc-d-linger-secs 0.1)
       (erc-server-flood-penalty 0.1)
       (erc-server-flood-margin 20)
       (dumb-server (erc-d-run "localhost" t 'solo))
       (port (process-contact dumb-server :service))
       erc-autojoin-channels-alist
       erc-server-buffer-foo)

    (ert-info ("Connect to foonet")
      (setq erc-server-buffer-foo (erc :server "127.0.0.1"
                                       :port port
                                       :nick "tester"
                                       :password "foonet:changeme"
                                       :full-name "tester"))
      (with-current-buffer erc-server-buffer-foo
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))))

    (erc-d-t-wait-for 1 (get-buffer "foonet"))

    (ert-info ("Joined by bouncer to #foo, pal persent")
      (with-current-buffer (erc-d-t-wait-for 1 (get-buffer "#foo"))
        (erc-d-t-search-for 1 "On Thursday")
        (goto-char erc-input-marker)
        (insert "hi")
        (erc-send-current-line)))

    (erc-d-t-wait-for 10 "Query buffer appears with message from pal"
      (get-buffer "Lal"))

    (ert-info ("Chat with pal, who changes name")
      (with-current-buffer "Lal"
        (erc-d-t-search-for 3 "hello")
        (goto-char erc-input-marker)
        (insert "hi")
        (erc-send-current-line)
        (erc-d-t-search-for 10 "is now known as Linguo")
        (should-not (search-forward "is now known as Linguo" nil t))))

    (erc-d-t-wait-for 1 (get-buffer "Linguo"))
    (should-not (get-buffer "Lal"))

    (with-current-buffer "Linguo"
      (goto-char erc-input-marker)
      (insert "howdy Linguo")
      (erc-send-current-line))

    (with-current-buffer "#foo"
      (erc-d-t-search-for 10 "is now known as Linguo")
      (should-not (search-forward "is now known as Linguo" nil t))
      (erc-cmd-PART ""))

    (with-current-buffer "Linguo"
      (erc-d-t-search-for 10 "get along"))))

;; You share a channel and a query buffer with a user on two different
;; networks (through a proxy).  The user changes their nick on both
;; networks at the same time.  Query buffers are updated accordingly.

(ert-deftest erc-scenarios-base-renick-queries-bouncer ()

  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/renick/queries")
       (erc-d-linger-secs 0.5)
       (erc-server-flood-penalty 0.1)
       (erc-server-flood-margin 30)
       (dumb-server (erc-d-run "localhost" t 'bouncer-foonet 'bouncer-barnet))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter))
       erc-accidental-paste-threshold-seconds
       erc-autojoin-channels-alist
       erc-server-buffer-foo
       erc-server-buffer-bar)

    (ert-info ("Connect to foonet")
      (setq erc-server-buffer-foo (erc :server "127.0.0.1"
                                       :port port
                                       :nick "tester"
                                       :password "foonet:changeme"
                                       :full-name "tester"))
      (with-current-buffer erc-server-buffer-foo
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))))

    (erc-d-t-wait-for 1 (get-buffer "foonet"))

    (ert-info ("Connect to barnet")
      (setq erc-server-buffer-bar (erc :server "127.0.0.1"
                                       :port port
                                       :nick "tester"
                                       :password "barnet:changeme"
                                       :full-name "tester"))
      (with-current-buffer erc-server-buffer-bar
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))))

    (erc-d-t-wait-for 1 (get-buffer "barnet"))
    (should-not (erc-scenarios-common-buflist "127.0.0.1"))

    (ert-info ("Joined by bouncer to #chan@foonet, pal persent")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan@foonet"))
        (funcall expect 1 "rando")
        (funcall expect 1 "simply misused")))

    (ert-info ("Joined by bouncer to #chan@barnet, pal persent")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan@barnet"))
        (funcall expect 1 "rando")
        (funcall expect 1 "come, sir, I am")))

    (ert-info ("Query buffer exists for rando@foonet")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "rando@foonet"))
        (funcall expect 1 "guess not")
        (goto-char erc-input-marker)
        (insert "I here")
        (erc-send-current-line)))

    (ert-info ("Query buffer exists for rando@barnet")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "rando@barnet"))
        (funcall expect 2 "rentacop")
        (goto-char erc-input-marker)
        (insert "Linda said you were gonna kill me.")
        (erc-send-current-line)))

    (ert-info ("Sync convo for rando@foonet")
      (with-current-buffer "rando@foonet"
        (funcall expect 1 "u are dumb")
        (goto-char erc-input-marker)
        (insert "not so")
        (erc-send-current-line)))

    (ert-info ("Sync convo for rando@barnet")
      (with-current-buffer "rando@barnet"
        (funcall expect 3 "I never saw her before")
        (goto-char erc-input-marker)
        (insert "You aren't with Wage?")
        (erc-send-current-line)))

    (erc-d-t-wait-for 1 (get-buffer "frenemy@foonet"))
    (erc-d-t-wait-for 1 (get-buffer "frenemy@barnet"))
    (should-not (get-buffer "rando@foonet"))
    (should-not (get-buffer "rando@barnet"))

    (with-current-buffer "frenemy@foonet"
      (funcall expect 1 "now known as")
      (funcall expect 1 "doubly so"))

    (with-current-buffer "frenemy@barnet"
      (funcall expect 1 "now known as")
      (funcall expect 1 "reality picture"))

    (when noninteractive
      (with-current-buffer "frenemy@barnet" (kill-buffer))
      (erc-d-t-wait-for 2 (get-buffer "frenemy"))
      (should-not (get-buffer "frenemy@foonet")))

    (with-current-buffer "#chan@foonet"
      (funcall expect 10 "is now known as frenemy")
      (should-not (search-forward "now known as frenemy" nil t)) ; regression
      (funcall expect 10 "words are razors"))

    (with-current-buffer "#chan@barnet"
      (funcall expect 10 "is now known as frenemy")
      (should-not (search-forward "now known as frenemy" nil t))
      (while (accept-process-output erc-server-process))
      (funcall expect 10 "I have lost"))))

(ert-deftest erc-scenarios-aux-unix-socket ()
  (skip-unless (featurep 'make-network-process '(:family local)))
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/renick/self")
       (erc-d-linger-secs 0.1)
       (erc-server-flood-penalty 0.1)
       (sock (expand-file-name "erc-d.sock" temporary-file-directory))
       (erc-scenarios-common-extra-teardown (lambda ()
                                              (delete-file sock)))
       (erc-server-connect-function
        (lambda (n b _ p &rest r)
          (apply #'make-network-process
                 `(:name ,n :buffer ,b :service ,p :family local ,@r))))
       (dumb-server (erc-d-run nil sock 'auto))
       erc-autojoin-channels-alist
       erc-server-buffer-foo)

    (ert-info ("Connect to foonet")
      (setq erc-server-buffer-foo (erc :server "fake"
                                       :port sock
                                       :nick "tester"
                                       :password "foonet:changeme"
                                       :full-name "tester"))
      (with-current-buffer erc-server-buffer-foo
        (should (string= (buffer-name) (format "fake:%s" sock)))))

    (with-current-buffer (erc-d-t-wait-for 3 (get-buffer "foonet"))
      (erc-d-t-search-for 10 "Your new nickname is dummy"))

    (ert-info ("Joined by bouncer to #foo, own nick present")
      (with-current-buffer (erc-d-t-wait-for 1 (get-buffer "#foo"))
        (erc-d-t-search-for 10 "dummy")
        (erc-d-t-search-for 10 "On Thursday")))))

;; See `erc-update-server-buffer-name'.  A perceived loss in
;; network connectivity turns out to be a false alarm, but the
;; bouncer has already accepted the second connection

(defun erc-scenarios--base-aborted-reconnect ()
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/reconnect")
       (erc-d-t-cleanup-sleep-secs 1)
       (erc-d-linger-secs 0.5)
       (dumb-server (erc-d-run "localhost" t 'aborted 'aborted-dupe))
       (port (process-contact dumb-server :service))
       erc-autojoin-channels-alist
       erc-server-buffer-foo)

    (ert-info ("Connect to foonet")
      (setq erc-server-buffer-foo (erc :server "127.0.0.1"
                                       :port port
                                       :nick "tester"
                                       :password "changeme"
                                       :full-name "tester"))
      (with-current-buffer erc-server-buffer-foo
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))))

    (ert-info ("Server buffer is unique and temp name is absent")
      (erc-d-t-wait-for 1 (get-buffer "FooNet"))
      (should-not (erc-scenarios-common-buflist "127.0.0.1"))
      (with-current-buffer erc-server-buffer-foo
        (erc-cmd-JOIN "#chan")))

    (ert-info ("Channel buffer #chan alive and well")
      (with-current-buffer (erc-d-t-wait-for 4 (get-buffer "#chan"))
        (erc-d-t-search-for 10 "welcome")))

    (ert-info ("Connect to foonet again")
      (setq erc-server-buffer-foo (erc :server "127.0.0.1"
                                       :port port
                                       :nick "tester"
                                       :password "changeme"
                                       :full-name "tester"))
      (let ((inhibit-message noninteractive))
        (with-current-buffer erc-server-buffer-foo
          (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
          (erc-d-t-wait-for 5 (not (erc-server-process-alive)))
          (erc-d-t-search-for 10 "FooNet still connected"))))

    (ert-info ("Server buffer is unique and temp name is absent")
      (should (equal (list (get-buffer "FooNet"))
                     (erc-scenarios-common-buflist "FooNet")))
      (should (equal (list (get-buffer (format "127.0.0.1:%d" port)))
                     (erc-scenarios-common-buflist "127.0.0.1"))))

    (ert-info ("Channel buffer #chan still going")
      (with-current-buffer "#chan"
        (erc-d-t-search-for 10 "and be prosperous")))))

(ert-deftest erc-scenarios-base-aborted-reconnect ()
  :tags '(:unstable)
  (let ((tries 3)
        (timeout 1)
        failed)
    (while (condition-case _err
               (progn
                 (erc-scenarios--base-aborted-reconnect)
                 nil)
             (ert-test-failed
              (message "Test %S failed; %s attempt(s) remaining."
                       (ert-test-name (ert-running-test))
                       tries)
              (sleep-for (cl-incf timeout))
              (not (setq failed (zerop (cl-decf tries)))))))
    (should-not failed)))

;; This defends against a regression in `erc-server-PRIVMSG' caused by
;; the removal of `erc-auto-query'.  When an active channel buffer is
;; killed off and PRIVMSGs arrive targeting it, the buffer should be
;; recreated.  See elsewhere for NOTICE logic, which is more complex.

(ert-deftest erc-scenarios-base-channel-buffer-revival ()

  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/channel-buffer-revival")
       (erc-d-linger-secs 0.5)
       (dumb-server (erc-d-run "localhost" t 'foonet))
       (port (process-contact dumb-server :service))
       erc-autojoin-channels-alist
       erc-server-buffer-foo)

    (ert-info ("Connect to foonet")
      (setq erc-server-buffer-foo (erc :server "127.0.0.1"
                                       :port port
                                       :nick "tester"
                                       :password "changeme"
                                       :full-name "tester"))
      (with-current-buffer erc-server-buffer-foo
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))))

    (ert-info ("Server buffer is unique and temp name is absent")
      (erc-d-t-wait-for 1 (get-buffer "FooNet"))
      (should-not (erc-scenarios-common-buflist "127.0.0.1"))
      (with-current-buffer erc-server-buffer-foo
        (erc-cmd-JOIN "#chan")))

    (ert-info ("Channel buffer #chan alive and well")
      (with-current-buffer (erc-d-t-wait-for 8 (get-buffer "#chan"))
        (erc-d-t-search-for 10 "Our queen and all her elves")
        (kill-buffer)))

    (should-not (get-buffer "#chan"))

    (ert-info ("Channel buffer #chan revived")
      (with-current-buffer (erc-d-t-wait-for 5 (get-buffer "#chan"))
        (erc-d-t-search-for 10 "and be prosperous")))))

;; This ensures we only reconnect `erc-server-reconnect-attempts'
;; (rather than infinitely many) times, which can easily happen when
;; tweaking code related to process sentinels in erc-backend.el.

(ert-deftest erc-scenarios-base-reconnect-timer ()
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/reconnect")
       (dumb-server (erc-d-run "localhost" t 'timer 'timer 'timer-last))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter))
       (erc-server-auto-reconnect t)
       erc-autojoin-channels-alist
       erc-server-buffer)

    (ert-info ("Connect to foonet")
      (setq erc-server-buffer (erc :server "127.0.0.1"
                                   :port port
                                   :nick "tester"
                                   :password "changeme"
                                   :full-name "tester"))
      (with-current-buffer erc-server-buffer
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))))

    (ert-info ("Server tries to connect thrice (including initial attempt)")
      (with-current-buffer erc-server-buffer
        (dotimes (n 3)
          (ert-info ((format "Attempt %d" n))
            (funcall expect 3 "Opening connection")
            (funcall expect 2 "Password incorrect")
            (funcall expect 2 "Connection failed!")
            (funcall expect 2 "Re-establishing connection")))
        (ert-info ("Prev attempt was final")
          (erc-d-t-absent-for 1 "Opening connection" (point)))))

    (ert-info ("Server buffer is unique and temp name is absent")
      (should (equal (list (get-buffer (format "127.0.0.1:%d" port)))
                     (erc-scenarios-common-buflist "127.0.0.1"))))))

(defun erc-scenarios-common--base-reconnect-options (test)
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/reconnect")
       (dumb-server (erc-d-run "localhost" t 'options 'options-again))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter))
       (erc-server-flood-penalty 0.1)
       (erc-server-auto-reconnect t)
       erc-autojoin-channels-alist
       erc-server-buffer)

    (should (memq 'autojoin erc-modules))

    (ert-info ("Connect to foonet")
      (setq erc-server-buffer (erc :server "127.0.0.1"
                                   :port port
                                   :nick "tester"
                                   :password "changeme"
                                   :full-name "tester"))
      (with-current-buffer erc-server-buffer
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
        (funcall expect 1 "debug mode")))

    (ert-info ("Wait for some output in channels")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
        (funcall expect 10 "welcome")))

    (ert-info ("Server buffer shows connection failed")
      (with-current-buffer erc-server-buffer
        (funcall expect 10 "Connection failed!  Re-establishing")))

    (should (equal erc-autojoin-channels-alist '(("foonet.org" "#chan"))))

    (funcall test)

    (with-current-buffer "FooNet" (erc-cmd-JOIN "#spam"))

    (erc-d-t-wait-for 5 "Channel #spam shown when autojoined"
      (eq (window-buffer) (get-buffer "#spam")))

    (ert-info ("Wait for auto reconnect")
      (with-current-buffer erc-server-buffer
        (funcall expect 10 "still in debug mode")))

    (ert-info ("Wait for activity to recommence in channels")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#chan"))
        (funcall expect 10 "forest of Arden"))
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "#spam"))
        (funcall expect 10 "her elves come here anon")))))

(ert-deftest erc-scenarios-base-reconnect-options--default ()
  (should (eq erc-join-buffer 'buffer))
  (should-not erc-reconnect-display)

  ;; FooNet (the server buffer) is not switched to because it's
  ;; already current (but not shown) when `erc-open' is called.  See
  ;; related conditional guard towards the end of that function.

  (erc-scenarios-common--base-reconnect-options
   (lambda ()
     (pop-to-buffer-same-window "*Messages*")

     (erc-d-t-ensure-for 1 "Server buffer not shown"
       (not (eq (window-buffer) (get-buffer "FooNet"))))

     (erc-d-t-wait-for 5 "Channel #chan shown when autojoined"
       (eq (window-buffer) (get-buffer "#chan"))))))

(ert-deftest erc-scenarios-base-reconnect-options--bury ()
  (should (eq erc-join-buffer 'buffer))
  (should-not erc-reconnect-display)

  (let ((erc-reconnect-display 'bury))
    (erc-scenarios-common--base-reconnect-options

     (lambda ()
       (pop-to-buffer-same-window "*Messages*")

       (erc-d-t-ensure-for 1 "Server buffer not shown"
         (not (eq (window-buffer) (get-buffer "FooNet"))))

       (erc-d-t-ensure-for 3 "Channel #chan not shown"
         (not (eq (window-buffer) (get-buffer "#chan"))))

       (eq (window-buffer) (messages-buffer))))))

(cl-defun erc-scenarios-common--base-network-id-same-network
    ((&key nick id server chan
           &aux (nick-a nick) (id-a id) (serv-buf-a server) (chan-buf-a chan))
     (&key nick id server chan
           &aux (nick-b nick) (id-b id) (serv-buf-b server) (chan-buf-b chan)))
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/network-id/same-network")
       (dumb-server (erc-d-run "localhost" t 'tester 'chester))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter))
       (erc-server-flood-penalty 0.1)
       (erc-server-flood-margin 30)
       erc-serv-buf-a erc-serv-buf-b)

    (ert-info ("Connect to foonet with nick tester")
      (with-current-buffer
          (setq erc-serv-buf-a (erc :server "127.0.0.1"
                                    :port port
                                    :nick nick-a
                                    :password "changeme"
                                    :full-name nick-a
                                    :id id-a))
        (erc-scenarios-common-assert-initial-buf-name id-a port)
        (erc-d-t-wait-for 5 (eq erc-network 'foonet))))

    (ert-info ("Connect to foonet with nick chester")
      (with-current-buffer
          (setq erc-serv-buf-b (erc :server "127.0.0.1"
                                    :port port
                                    :nick nick-b
                                    :password "changeme"
                                    :full-name nick-b
                                    :id id-b))
        (erc-scenarios-common-assert-initial-buf-name id-b port)))

    (erc-d-t-wait-for 3 (not (erc-scenarios-common-buflist "127.0.0.1")))

    (with-current-buffer erc-serv-buf-a
      (should (string= (buffer-name) serv-buf-a))
      (funcall expect 3 "debug mode")
      (erc-cmd-JOIN "#chan"))

    (with-current-buffer erc-serv-buf-b
      (should (string= (buffer-name) serv-buf-b))
      (funcall expect 3 "debug mode")
      (erc-cmd-JOIN "#chan"))

    (erc-d-t-wait-for 10 (get-buffer chan-buf-a))
    (erc-d-t-wait-for 10 (get-buffer chan-buf-b))

    (ert-info ("Greets other nick in same channel")
      (with-current-buffer chan-buf-a
        (funcall expect 5 "chester")
        (funcall expect 5 "find the forester")
        (erc-cmd-MSG "#chan chester: hi")))

    (ert-info ("Sees other nick in same channel")
      (with-current-buffer chan-buf-b
        (funcall expect 5 "tester")
        (funcall expect 10 "<tester> chester: hi")
        (funcall expect 5 "This was lofty")
        (erc-cmd-MSG "#chan hi tester")))

    (with-current-buffer chan-buf-a
      (funcall expect 5 "To employ you towards")
      (erc-cmd-QUIT ""))

    (with-current-buffer chan-buf-b
      (funcall expect 5 "To employ you towards")
      (erc-cmd-QUIT ""))))

(ert-deftest erc-scenarios-base-network-id-same-network--two-ids ()
  (erc-scenarios-common--base-network-id-same-network
   (list :nick "tester"
         :id 'tester/foonet
         :server "tester/foonet"
         :chan "#chan@tester/foonet")
   (list :nick "chester"
         :id 'chester/foonet
         :server "chester/foonet"
         :chan "#chan@chester/foonet")))

(ert-deftest erc-scenarios-base-network-id-same-network--one-id-tester ()
  (erc-scenarios-common--base-network-id-same-network
   (list :nick "tester"
         :id 'tester/foonet
         :server "tester/foonet"
         :chan "#chan@tester/foonet")
   (list :nick "chester"
         :id nil
         :server "foonet"
         :chan "#chan@foonet")))

(ert-deftest erc-scenarios-base-network-id-same-network--one-id-chester ()
  (erc-scenarios-common--base-network-id-same-network
   (list :nick "tester"
         :id nil
         :server "foonet"
         :chan "#chan@foonet")
   (list :nick "chester"
         :id 'chester/foonet
         :server "chester/foonet"
         :chan "#chan@chester/foonet")))

(ert-deftest erc-scenarios-base-network-id-same-network--no-ids ()
  (erc-scenarios-common--base-network-id-same-network
   (list :nick "tester"
         :id nil
         :server "foonet/tester"
         :chan "#chan@foonet/tester") ; <- note net before nick
   (list :nick "chester"
         :id nil
         :server "foonet/chester"
         :chan "#chan@foonet/chester")))

;; Upon reconnecting, playback for channel and target buffers is
;; routed correctly.  Autojoin is irrelevant here, but for the
;; skeptical, see `erc-scenarios-common--join-network-id', which
;; overlaps with this and includes spurious JOINs ignored by the
;; server.

(ert-deftest erc-scenarios-base-association-reconnect-playback ()
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/association/reconnect-playback")
       (erc-d-linger-secs 0.5)
       (erc-server-flood-penalty 0.1)
       (erc-server-flood-margin 30)
       (dumb-server (erc-d-run "localhost" t 'foonet 'foonet-again))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter))
       erc-autojoin-channels-alist
       erc-server-buffer-foo)

    (ert-info ("Connect to foonet")
      (setq erc-server-buffer-foo (erc :server "127.0.0.1"
                                       :port port
                                       :nick "tester"
                                       :password "changeme"
                                       :full-name "tester"))
      (with-current-buffer erc-server-buffer-foo
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))))

    (ert-info ("Setup")

      (ert-info ("Server buffer is unique and temp name is absent")
        (erc-d-t-wait-for 1 (get-buffer "foonet"))
        (should-not (erc-scenarios-common-buflist "127.0.0.1")))

      (ert-info ("Channel buffer #chan playback received")
        (with-current-buffer (erc-d-t-wait-for 3 (get-buffer "#chan"))
          (funcall expect 10 "But purgatory")))

      (ert-info ("Ask for help from services or bouncer bot")
        (with-current-buffer erc-server-buffer-foo
          (erc-cmd-MSG "*status help")))

      (ert-info ("Help received")
        (with-current-buffer (erc-d-t-wait-for 5 (get-buffer "*status"))
          (funcall expect 10 "Rehash")))

      (ert-info ("#chan convo done")
        (with-current-buffer "#chan"
          (funcall expect 10 "most egregious indignity"))))

    ;; KLUDGE (see note above test)
    (should erc-autojoin-channels-alist)
    (setq erc-autojoin-channels-alist nil)

    (with-current-buffer erc-server-buffer-foo
      (erc-cmd-QUIT "")
      (erc-d-t-wait-for 4 (not (erc-server-process-alive)))
      (erc-cmd-RECONNECT))

    (ert-info ("Channel buffer found and associated")
      (with-current-buffer "#chan"
        (funcall expect 10 "Wilt thou rest damned")))

    (ert-info ("Help buffer found and associated")
      (with-current-buffer "*status"
        (goto-char erc-input-marker)
        (insert "help")
        (erc-send-current-line)
        (funcall expect 10 "Restart ZNC")))

    (ert-info ("#chan convo done")
      (with-current-buffer "#chan"
        (funcall expect 10 "here comes the lady")))))

;; You register a new nick, disconnect, and log back in, but your nick
;; is not granted, so ERC obtains a backtick'd version.  You open a
;; query buffer for NickServ, and ERC names it using the session ID
;; (which includes the backtick'd nick) as a suffix.  The original
;; (disconnected) NickServ buffer gets renamed with *its* session ID
;; as well.  You then identify to NickServ, and the dead session is no
;; longer considered distinct.

(ert-deftest erc-scenarios-base-association-nick-bumped ()
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/association/nick-bump")
       (dumb-server (erc-d-run "localhost" t 'renicked 'renicked-again))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter))
       (erc-server-flood-penalty 0.5)
       (erc-server-flood-margin 30))

    (ert-info ("Connect to foonet with nick tester")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :full-name "tester")
        (erc-scenarios-common-assert-initial-buf-name nil port)
        (erc-d-t-wait-for 5 (eq erc-network 'foonet))))

    (ert-info ("Create an account for tester and quit")
      (with-current-buffer "foonet"
        (funcall expect 3 "debug mode")

        (erc-cmd-QUERY "NickServ")
        (with-current-buffer "NickServ"
          (erc-send-input-line "NickServ" "REGISTER changeme")
          (funcall expect 5 "Account created")
          (funcall expect 1 "You're now logged in as tester"))

        (with-current-buffer "foonet"
          (erc-cmd-QUIT "")
          (erc-d-t-wait-for 4 (not (erc-server-process-alive)))
          (funcall expect 5 "ERC finished"))))

    (with-current-buffer "foonet"
      (erc-cmd-RECONNECT))

    (erc-d-t-wait-for 10 "Nick request rejection prevents reassociation (good)"
      (get-buffer "foonet/tester`"))

    (ert-info ("Ask NickServ to change nick")
      (with-current-buffer "foonet/tester`"
        (funcall expect 3 "already in use")
        (funcall expect 3 "debug mode")
        (erc-cmd-QUERY "NickServ"))

      (erc-d-t-wait-for 1 "Dead NickServ query buffer renamed, now qualified"
        (get-buffer "NickServ@foonet/tester"))

      (with-current-buffer "NickServ@foonet/tester`" ; new one
        (erc-send-input-line "NickServ" "IDENTIFY tester changeme")
        (funcall expect 5 "You're now logged in as tester")
        (ert-info ("Original buffer found, reused")
          (erc-d-t-wait-for 2 (equal (buffer-name) "NickServ")))))

    (ert-info ("Ours is the only NickServ buffer that remains")
      (should-not (cdr (erc-scenarios-common-buflist "NickServ"))))

    (ert-info ("Visible network ID truncated to one component")
      (should (not (get-buffer "foonet/tester`")))
      (should (not (get-buffer "foonet/tester")))
      (should (get-buffer "foonet")))))

;; A less common variant is when your bouncer switches to an alternate
;; nick while you're disconnected, and upon reconnecting, you get
;; a new nick.

(ert-deftest erc-scenarios-base-association-nick-bumped-mandated-renick ()
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/association/nick-bump")
       (dumb-server (erc-d-run "localhost" t
                               'renicked-foisted 'renicked-foisted-again))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter))
       (erc-server-flood-penalty 0.5)
       (erc-server-flood-margin 30))

    (ert-info ("Connect to foonet with nick tester")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :full-name "tester")
        (erc-scenarios-common-assert-initial-buf-name nil port)
        (erc-d-t-wait-for 5 (eq erc-network 'foonet))))

    (ert-info ("Greet bob and quit")
      (with-current-buffer "foonet"
        (funcall expect 3 "debug mode")

        (erc-cmd-QUERY "bob")
        (with-current-buffer "bob"
          (erc-send-input-line "bob" "hi")
          (funcall expect 5 "hola")
          (funcall expect 1 "how r u?"))

        (with-current-buffer "foonet"
          (erc-cmd-QUIT "")
          (erc-d-t-wait-for 4 (not (erc-server-process-alive)))
          (funcall expect 5 "ERC finished"))))

    ;; Since we use reconnect, a new buffer won't be created
    ;; TODO add variant with clean `erc' invocation
    (with-current-buffer "foonet"
      (erc-cmd-RECONNECT))

    (ert-info ("Server-initiated renick")
      (with-current-buffer (erc-d-t-wait-for 10 (get-buffer "foonet/dummy"))
        (should-not (get-buffer "foonet/tester"))
        (funcall expect 5 "debug mode"))

      (erc-d-t-wait-for 1 "Old query renamed, now qualified"
        (get-buffer "bob@foonet/tester"))

      (with-current-buffer (erc-d-t-wait-for 5 (get-buffer "bob@foonet/dummy"))
        (erc-cmd-NICK "tester")
        (ert-info ("Buffers combined")
          (erc-d-t-wait-for 2 (equal (buffer-name) "bob")))))

    (with-current-buffer "foonet"
      (funcall expect 5 "You're now logged in as tester"))

    (ert-info ("Ours is the only bob buffer that remains")
      (should-not (cdr (erc-scenarios-common-buflist "bob"))))

    (ert-info ("Visible network ID truncated to one component")
      (should (not (get-buffer "foonet/dummy")))
      (should (get-buffer "foonet")))))

(ert-deftest erc-scenarios-services-password ()

  (defvar erc-nickserv-passwords) ; <- FIXME what is this?

  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "services/password")
       (erc-server-flood-penalty 0.1)
       (erc-modules (cons 'services erc-modules))
       (erc-nickserv-passwords '((Libera.Chat (("joe" . "bar")
                                               ("tester" . "changeme")))))
       (expect (erc-d-t-make-expecter))
       (dumb-server (erc-d-run "localhost" t 'libera))
       (port (process-contact dumb-server :service)))

    (ert-info ("Connect without password")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :full-name "tester")
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
        (erc-d-t-wait-for 2 (eq erc-network 'Libera.Chat))
        (funcall expect 1 "This nickname is registered.")
        (funcall expect 1 "You are now identified")
        (funcall expect 1 "Last login from")
        (erc-cmd-QUIT "")))

    (erc-services-mode -1)

    (should-not (memq 'services erc-modules))))

(ert-deftest erc-scenarios-services-prompt ()
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "services/password")
       (erc-server-flood-penalty 0.1)
       (erc-modules (cons 'services erc-modules))
       (expect (erc-d-t-make-expecter))
       (dumb-server (erc-d-run "localhost" t 'libera))
       (port (process-contact dumb-server :service)))

    (ert-info ("Connect without password")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :full-name "tester")
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
        (ert-simulate-keys "changeme\r"
          (erc-d-t-wait-for 2 (eq erc-network 'Libera.Chat))
          (funcall expect 3 "This nickname is registered.")
          (funcall expect 3 "You are now identified")
          (funcall expect 3 "Last login from"))
        (erc-cmd-QUIT "")))

    (erc-services-mode -1)

    (should-not (memq 'services erc-modules))))

(ert-deftest erc-scenarios-base-flood ()
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/flood")
       (erc-d-linger-secs 0.5)
       (dumb-server (erc-d-run "localhost" t 'soju))
       (port (process-contact dumb-server :service))
       (erc-server-flood-penalty 0.5) ; this ratio MUST match
       (erc-server-flood-margin 1.5) ;  the default of 3:10
       (expect (erc-d-t-make-expecter))
       erc-autojoin-channels-alist)

    (ert-info ("Connect to bouncer")
      (with-current-buffer
          (erc :server "127.0.0.1"
               :port port
               :nick "tester"
               :password "changeme"
               :full-name "tester")
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
        (funcall expect 5 "Soju")))

    (ert-info ("#chan@foonet exists")
      (with-current-buffer (erc-d-t-wait-for 5 (get-buffer "#chan/foonet"))
        (erc-d-t-search-for 2 "<bob/foonet>")
        (erc-d-t-absent-for 0.1 "<joe")
        (funcall expect 3 "was created on")))

    (ert-info ("#chan@barnet exists")
      (with-current-buffer (erc-d-t-wait-for 5 (get-buffer "#chan/barnet"))
        (erc-d-t-search-for 2 "<joe/barnet>")
        (erc-d-t-absent-for 0.1 "<bob")
        (funcall expect 3 "was created on")
        (funcall expect 5 "To get good guard")))

    (ert-info ("Message not held in queue limbo")
      (with-current-buffer "#chan/foonet"
        ;; Without 'no-penalty param in `erc-server-send', should fail
        ;; after ~10 secs with:
        ;;
        ;;   (erc-d-timeout "Timed out awaiting request: (:name ~privmsg
        ;;    :pattern \\`PRIVMSG #chan/foonet :alice: hi :timeout 2
        ;;    :dialog soju)")
        ;;
        ;; Try reversing commit and spying on queue interactively
        (erc-cmd-MSG "#chan/foonet alice: hi")
        (funcall expect 5 "tester: Good, very good")))

    (ert-info ("All output sent")
      (with-current-buffer "#chan/foonet"
        (funcall expect 5 "Some man or other"))
      (with-current-buffer "#chan/barnet"
        (while (accept-process-output erc-server-process))
        (funcall expect 5 "That's he that was Othello")))))

;; Corner case demoing fallback behavior for an absent 004 RPL but a
;; present 422 or 375.  If this is unlikely enough, remove or guard
;; with `ert-skip' plus some condition so it only runs when explicitly
;; named via ERT specifier

(ert-deftest erc-scenarios-networks-announced-missing ()
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "networks/announced-missing")
       (erc-d-linger-secs 0.5)
       (expect (erc-d-t-make-expecter))
       (dumb-server (erc-d-run "localhost" t 'foonet))
       (port (process-contact dumb-server :service)))

    (ert-info ("Connect without password")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :full-name "tester")
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))
        (let ((err (should-error (sleep-for 1))))
          (should (string-match-p "Failed to determine" (cadr err))))
        (funcall expect 1 "Failed to determine")
        (funcall expect 1 "Failed to determine")
        (should-not erc-network)
        (should (string= erc-server-announced-name "irc.foonet.org"))))))

;; Targets that are host/server masks like $*, $$*, and #* are routed
;; to the server buffer: https://github.com/ircdocs/wooooms/issues/5

(ert-deftest erc-scenarios-base-mask-target-routing ()
  (erc-scenarios-common-with-cleanup
      ((erc-scenarios-common-dialog "base/mask-target-routing")
       (erc-d-linger-secs 0.5)
       (dumb-server (erc-d-run "localhost" t 'foonet))
       (port (process-contact dumb-server :service))
       (expect (erc-d-t-make-expecter)))

    (ert-info ("Connect to foonet")
      (with-current-buffer (erc :server "127.0.0.1"
                                :port port
                                :nick "tester"
                                :password "changeme"
                                :full-name "tester")
        (should (string= (buffer-name) (format "127.0.0.1:%d" port)))))

    (erc-d-t-wait-for 1 (get-buffer "foonet"))

    (ert-info ("Channel buffer #foo playback received")
      (with-current-buffer (erc-d-t-wait-for 3 (get-buffer "#foo"))
        (funcall expect 10 "Excellent workman")))

    (ert-info ("Global notices routed to server buffer")
      (with-current-buffer "foonet"
        (funcall expect 10 "going down soon")
        (funcall expect 10 "this is a warning")
        (funcall expect 10 "second warning")
        (funcall expect 10 "final warning")))

    (should-not (get-buffer "$*"))))

;;; erc-scenarios.el ends here
