;; -*-scheme-*-
;;
;; serveez.scm - convenience functions
;;
;; Copyright (C) 2001 Martin Grabmueller <mgrabmue@cs.tu-berlin.de>
;; Copyright (C) 2001 Stefan Jahn <stefan@lkcc.org>
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
;; $Id: serveez.scm,v 1.7 2002/07/29 18:32:09 ela Exp $
;;

;;
;; === Miscellaneous functions - Scheme for beginners, thanks to 'mgrabmue.
;;
(define (println . args)
  (for-each display args) (newline))
(define (printsln spacer . args)
  (for-each (lambda (x) (display x) (display spacer)) args) (newline))

;;
;; === Network interfaces
;;
(define (interface-add! . ifc)
  (serveez-interfaces (append! (serveez-interfaces) ifc)))

;;
;; === Additional search paths for server modules
;;
(define (loadpath-add! . path)
  (serveez-loadpath (append! (serveez-loadpath) path)))

;;
;; === Enhanced server bindings
;;
(define (bind-servers! . args)
  (let ((server-list '())  ;; Initialize lists.
        (port-list '()))

    ;; Iterate over argument list, separating ports from servers.
    (for-each
     (lambda (elem)
       (cond ((serveez-port? elem)
	      (set! port-list (cons elem port-list)))
	     ((serveez-server? elem)
	      (set! server-list (cons elem server-list)))))
     args)

    ;; Iterate over server list and ..
    (for-each
     (lambda (server)
       ;; ... for each server, iterate over port list and ...
       (for-each
	(lambda (port)
	  ;; ... bind each port to each server.
	  (bind-server! port server))
	port-list))
     server-list)))

;;
;; === Create a simple tcp port
;;
(define (create-tcp-port! basename port)
  (let ((portname (string-append basename (number->string port))))
    (if (not (serveez-port? portname))
	(define-port! portname 
	  `((proto . tcp) 
	    (port . ,port))))
    portname))

;;
;; === Bind some servers to a range of tcp network ports
;;
(define (bind-tcp-port-range! from to . args)
  (do ((no from (+ no 1)))
      ((> no to))
    (for-each
     (lambda (server)
       (bind-server! (create-tcp-port! "guile-tcp-port-" no) server))
     args)))

;;
;; === Create a simple udp port
;;
(define (create-udp-port! basename port)
  (let ((portname (string-append basename (number->string port))))
    (if (not (serveez-port? portname))
	(define-port! portname 
	  `((proto . udp) 
	    (port . ,port))))
    portname))

;;
;; === Bind some servers to a range of udp network ports
;;
(define (bind-udp-port-range! from to . args)
  (do ((no from (+ no 1)))
      ((> no to))
    (for-each
     (lambda (server)
       (bind-server! (create-udp-port! "guile-udp-port-" no) server))
     args)))

;;
;; === Additional Guile networking API
;;
(define (getrpcent) (getrpc))
(define (getrpcbyname name) (getrpc name))
(define (getrpcbynumber number) (getrpc number))
(define (setrpcent . stayopen)
  (if (pair? stayopen)
      (setrpc (car stayopen))
      (setrpc #f)))
(define (endrpcent) (setrpc))

;;
;; === Include documentation file into Guile help system
;;
(define (serveez-doc-add!)
  (catch #t
	 (lambda ()
	   (use-modules (ice-9 session))
	   (use-modules (ice-9 documentation))
	   (for-each 
	    (lambda (path)
	      (let ((file (string-append path "/serveez-procedures.txt"))
		    (found #f))
		(for-each 
		 (lambda (f) (if (equal? f file) (set! found #t)))
		 documentation-files)
		(if (not found)
		    (set! documentation-files 
			  (cons file documentation-files)))))
	    (serveez-loadpath))
	   (display "Serveez documentation file successfully added.\n")
	   #t)
	 (lambda args
	   (display "Failed to add Serveez documentation file.\n")
	   #f)))
