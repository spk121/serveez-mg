;; -*-scheme-*-
;;
;; (serveez lib) - convenience functions
;;
;; Copyright 2010, 2011 Michael Gran <spk121@yahoo.com>
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
;;

;;
;; === Miscellaneous functions - Scheme for beginners, thanks to 'mgrabmue.
;;
(define-module (serveez-mg lib)
  #:use-module (guile-user)
  #:use-module (rnrs bytevectors)
  #:export (println
            printsln
            interface-add!
            loadpath-add!
            bind-servers!
            create-tcp-port!
            bind-tcp-port-range!
            create-udp-port!
            bind-udp-port-range!
            getrpcent
            getrpcbyname
            getrpcbynumber
            setrpcent
            endrpcent
            subbytevector
            bytevector-contains
            bytevector-search
            serveez-doc-add!
            ))

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

;; Create a new bytevector that is a copy of BV from START (inclusive)
;; to END (exclusive)
(define (subbytevector bv start end)
  (let ((end (or end (bytevector-length bv))))
    (if (or (> 0 start)
            (> start end)
            (> end (bytevector-length bv)))
        (error "bad start/end values" start end))
    (let* ((len (- end start))
           (bv2 (make-bytevector len)))
      (bytevector-copy! bv start bv2 0 len)
      bv2)))

;; Search for BV1 in BV2.  If it is found, return the index in BV2 where
;; BV1 is located.  Otherwise, return #f.
(define (bytevector-contains bv2 bv1)
  (let ((len1 (bytevector-length bv1))
        (len2 (bytevector-length bv2)))
    (if (<= len1 len2)
        ;; If BV1 fits in BV2, begin searching
        (let loop ((i 0))
          ;; (format #t "~a  ~a ~%" bv1 (subbytevector bv2 i (+ i len1))) 
          (cond
           ;; If it is found at index I, return I
           ((bytevector=? bv1 (subbytevector bv2 i (+ i len1)))
            i)
           ;; If we've reached the end of BV2, return #f
           ((>= (+ i len1) len2)
            #f)
           ;; Else keep looping
           (else
            (loop (+ i 1)))))
        ;; If BV1 is longer than BV2, return #f
        #f)))

(define (bytevector-search haystack needle)
  (if (not (bytevector? haystack))
      (error "not a bytevector " haystack))
  (if (not (or (string? needle)
               (char? needle)
               (exact? needle)
               (bytevector? needle)))
      (error "not a string, character, integer, or bytevector" needle))
  (let ((bv1 haystack)
        (bv2 (cond 
              ((string? needle)
               (string->utf8 needle))
              ((char? needle)
               (make-bytevector 1 (char->integer needle)))
              ((exact? needle)
               (make-bytevector 1 needle))
              (else
               needle))))
    (bytevector-contains bv1 bv2)))


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
