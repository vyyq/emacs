;;; proxy-subprocess.el --- Example setup file for erc-d
;;; Commentary:
;;; Code:

(defvar erc-d-spec-vars)

(setq erc-d-spec-vars

      (list
       (cons 'fqdn (lambda (helper)
                     (let ((name (funcall helper :dialog-name)))
                       (funcall helper :set
                                (if (eq name 'proxy-foonet)
                                    "irc.foo.net"
                                  "irc.bar.net")))))

       (cons 'net (lambda (helper)
                    (let ((name (funcall helper :dialog-name)))
                      (funcall helper :set
                               (if (eq name 'proxy-foonet)
                                   "FooNet"
                                 "BarNet")))))

       (cons 'network '(group (+ alpha)))))

;;; proxy-subprocess.el ends here
