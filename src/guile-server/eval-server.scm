;; -*-scheme-*-
;;
;; eval-server.scm - example server for evaluating Scheme expressions
;;
;; Copyright (C) 2001, 2002 Stefan Jahn <stefan@lkcc.org>,
;; Copyright (C) 2001 Martin Grabmueller <mgrabmue@cs.tu-berlin.de>
;;
;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this package.  If not, see <http://www.gnu.org/licenses/>.

;; Some awkward compatibility kluges for making this run with Guile
;; 1.4 and 1.6/later.
;;
(serveez-load "serveez.scm")

(define (eval-global-init servertype)
  (println "Running eval global init " servertype ".")
  0)

(define (eval-init server)
  (println "Running eval init " server ".")
  0)

(define (eval-global-finalize servertype)
  (println "Running eval global finalizer " servertype ".")
  0)

(define (eval-finalize server)
  (println "Running eval finalizer " server ".")
  0)

(define (eval-detect-proto server sock)
  (println "Detecting eval protocol ...")
  1)

(define (eval-info-server server)
  (println "Running eval server info " server ".")
  " This is the eval server.")

(define (eval-handle-request sock request len)
  (let ((idx (bytevector-contains request 
                                  (string->utf8 (svz:server:config-ref sock "quit")))))
    (if (and idx (= idx 0))
        ;; If REQUEST begins with 'quit' string that was defined in
        ;; eval-server's instantiation, return now
        -1
        ;; Otherwise, try to evaluate REQUEST by converting it to a
        ;; string an trying to evaluate it in the Guile evaluator
	(let ((safe-module (interaction-environment)))
	  (catch #t
		 (lambda ()
		   (let ((expr (call-with-input-string
				(utf8->string request) read)))
		     (let ((res (eval expr safe-module)))
		       (svz:sock:print sock
		         (string-append "=> "
			   (object->string res)
			   "\r\n"
			   (svz:server:config-ref sock "prompt"))))))
		 (lambda args
		   (svz:sock:print sock
		     (string-append "Exception: "
		       (apply simple-format #f (caddr args) (cadddr args))
		       "\r\n"
		       (svz:server:config-ref sock "prompt")))))
	  0))))

(define (eval-connect-socket server sock)
  (println "Running connect socket.")
  (svz:sock:boundary sock "\n")
  (svz:sock:handle-request sock eval-handle-request)
  (svz:sock:print sock (string-append
                        (svz:server:config-ref server "greeting")
                        "\r\n"
                        (svz:server:config-ref server "prompt")))
  0)

;; Port configuration.
(define-port! 'eval-port '((proto . tcp)
                           (port  . 2001)))

;; Servertype definitions.
(define-servertype! '(
  (prefix      . "eval")
  (description . "guile eval server")
  (detect-proto    . eval-detect-proto)
  (global-init     . eval-global-init)
  (init            . eval-init)
  (finalize        . eval-finalize)
  (global-finalize . eval-global-finalize)
  (connect-socket  . eval-connect-socket)
  (info-server     . eval-info-server)
  (configuration   . (
    (prompt   . ( string #t "eval: " ))
    (quit     . ( string #t "quit" ))
    (greeting . ( string #t "Hello, type `quit' to end the connection.\r
Type Scheme expression to see them evaluated (but only one-liners)." ))
    ))))

;; Server instantiation.
(define-server! 'eval-server '(
                               (prompt . "guile> ")
                               (quit   . "quit")
                               ))

;; Bind server to port.
(bind-server! 'eval-port 'eval-server)
