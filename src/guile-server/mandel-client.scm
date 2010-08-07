;; -*-scheme-*-
;;
;; mandel-client.scm - mandelbrot fractal client
;;
;; Copyright (C) 2001 Raimund Jacob <raimi@lkcc.org>
;; Copyright (C) 2001, 2002 Stefan Jahn <stefan@lkcc.org>,
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
;; $Id: mandel-client.scm,v 1.5 2002/07/30 22:39:08 ela Exp $
;;

;; load shared functionality
(serveez-load "mandel-shared.scm")

;; epsilon environment
(define epsilon 1e-6)

;; return #t if the complex number z1 is near (within a certain epsilon 
;; environment) z2, otherwise #f
(define (near? z1 z2)
  (and (< (abs (real-part (- z2 z1))) epsilon)
       (< (abs (imag-part (- z2 z1))) epsilon)))

;; compute the number of iterations for the mandelbrot algorithm for the
;; complex number z, abort calculation at max-iteration limit
(define (iterate-mandel z max-iteration)
  (let ((z-first z))
    (let loop ((i 0) (z 0) (old-z z))
      (cond ((>= i (1- max-iteration)) i)
	    ((= z old-z) (1- max-iteration)) ;; use (near?)
	    ((> (magnitude z) 2) i)
	    (else (loop (1+ i) (+ z-first (* z z)) z))))))

;; handle one line sent by the server
(define (mandel-handle-request sock request len)
  (let* ((tokens (mandel-split (binary->string request)))
	 (command (list-ref tokens 1))
	 (todo (hash-ref (svz:sock:data sock) "todo")))

    (cond 
     ;; server accepted us
     ((equal? command "welcome")
      (svz:sock:print sock "(dnc:request)\r\n")
      0)
     ;; server told us to quit
     ((equal? command "bye")
      -1)
     ;; server gave us something to compute
     ((equal? command "value")
      (let* ((max-iteration (string->number (list-ref tokens 4)))
	     (result (iterate-mandel (string->number (list-ref tokens 2))
				     max-iteration)))

	(display (string-append "*** Calculating point "
				(list-ref tokens 3)
				" -> "
				(number->string result)
				"\n"))
	(svz:sock:print sock
			(string-append "(dnc:value:"
				       (list-ref tokens 2)
				       ":"
				       (list-ref tokens 3)
				       ":"
				       (number->string result)
				       ")\r\n"))
	(if (= 0 (1- todo))
	    (begin
	      (svz:sock:final-print sock)
	      (svz:sock:print sock "(dnc:bye)\r\n")
	      0)
	    (begin 
	      (hash-set! (svz:sock:data sock) "todo" (1- todo))
	      (svz:sock:print sock "(dnc:request)\r\n")
	      0))
	))
     ;; invalid server request
     (else -1))
    ))

;; disconnected callback
(define (mandel-disconnected sock)
  (serveez-nuke)
  0)

;; main program entry point
(define (mandel-main todo)
  (let* ((sock (svz:sock:connect "127.0.0.1" PROTO_TCP 1025))
	 (state (make-hash-table 3)))

    (mandel-prepare-sock sock)
    (hash-set! state "todo" todo)
    (svz:sock:data sock state)
    (svz:sock:handle-request sock mandel-handle-request)
    (svz:sock:disconnected sock mandel-disconnected)
    (svz:sock:print sock mandel-magic)
    ))

(mandel-main (* 320 240))
