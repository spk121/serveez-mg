;; -*-scheme-*-
;;
;; echo-server.scm - example server completely written in Guile
;;
;; Copyright (C) 2001, 2002 Stefan Jahn <stefan@lkcc.org>
;;
;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this package; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;; $Id: echo-server.scm,v 1.2 2002/07/30 22:39:08 ela Exp $
;;

(serveez-load "serveez.scm")

(define (echo-global-init servertype)
  (println "Running echo global init " servertype ".")
  0)

(define (echo-init server)
  (println "Running echo init " server ".")
  0)

(define (echo-global-finalize servertype)
  (println "Running echo global finalizer " servertype ".")
  0)

(define (echo-finalize server)
  (println "Running echo finalizer " server ".")
  0)

(define (echo-detect-proto server sock)
  (println "Detecting echo protocol ...")
  1)

(define (echo-info-server server)
  (define ret '())
  (println "Running echo server info " server ".")
  (set! ret " This is the echo server.")
  (println " echo-integer: " 
	   (svz:server:config-ref server "echo-integer"))
  (println " echo-integer-array: " 
	   (svz:server:config-ref server "echo-integer-array"))
  (println " echo-string: " 
	   (svz:server:config-ref server "echo-string"))
  (println " echo-string-array: " 
	   (svz:server:config-ref server "echo-string-array"))
  (println " echo-hash: " 
	   (svz:server:config-ref server "echo-hash"))
  (println " echo-port: " 
	   (svz:server:config-ref server "echo-port"))
  (println " echo-boolean: " 
	   (svz:server:config-ref server "echo-boolean"))
  ret)

(define (echo-handle-request sock request len)
  (define ret '())
  (let ((idx (binary-search request "quit")))
    (if (and idx (= idx 0))
	(set! ret -1)
    (begin
      (svz:sock:print sock (binary-concat! (string->binary "Echo: ") request))
      (set! ret 0)))
    ret))

(define (echo-connect-socket server sock)
  (define hello "Hello, type `quit' to end the connection.\r\n")
  (println "Running connect socket.")
  (svz:sock:boundary sock "\n")
  (svz:sock:handle-request sock echo-handle-request)
  (svz:sock:print sock hello)
  0)

;; Port configuration.
(define-port! 'echo-port '((proto . tcp)
			   (port  . 2000)))

;; Servertype definitions.
(define-servertype! '(
  (prefix      . "echo")
  (description . "guile echo server")
  (detect-proto    . echo-detect-proto)
  (global-init     . echo-global-init)
  (init            . echo-init)
  (finalize        . echo-finalize)
  (global-finalize . echo-global-finalize)
  (connect-socket  . echo-connect-socket)
  (info-server     . echo-info-server)
  (configuration   . (
    ;; (key . (type defaultable default))
    (echo-integer       . (integer #t 0))
    (echo-integer-array . (intarray #t (1 2 3 4 5)))
    (echo-string        . (string #t "default-echo-string"))
    (echo-string-array  . (strarray #t ("guile" "echo" "server")))
    (echo-hash          . (hash #t (("echo" . "loud") ("guile" . "tricky"))))
    (echo-port          . (portcfg #t echo-port))
    (echo-boolean       . (boolean #t #t))
  ))))

;; Server instantiation.
(define-server! 'echo-server '(
			       (echo-integer       . 42)
			       (echo-integer-array . (5 4 3 2 1))
			       ))

;; Bind server to port.
(bind-server! 'echo-port 'echo-server)

;; Control protocol server for remote control.
(define-port! 'control-port `(
			      (proto . tcp)
			      (port . 42420)
			      (ipaddr . *)
			      ))

(define-server! 'control-server)

(bind-server! 'control-port 'control-server)
