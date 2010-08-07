;; -*-scheme-*-
;;
;; mandel-server.scm - mandelbrot fractal server
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
;; $Id: mandel-server.scm,v 1.6 2002/07/30 22:39:08 ela Exp $
;;

;; load shared functionality
(serveez-load "mandel-shared.scm")

;; initialize the server state by calculating values from the configuration
(define (mandel-init server)
  (let* ((x-res (svz:server:config-ref server "x-res"))
	 (y-res (svz:server:config-ref server "y-res"))
	 (start (string->number (svz:server:config-ref server "start")))
	 (end   (string->number (svz:server:config-ref server "end")))
	 (x-diff (real-part (- end start))) 
	 (y-diff (imag-part (- end start)))
	 (x-ratio (/ x-diff x-res))
	 (y-ratio (/ y-diff y-res)))

    (svz:server:state-set! server "data" (make-vector (* x-res y-res) -1))
    (svz:server:state-set! server "missing" (* x-res y-res))
    (svz:server:state-set! server "index" 0)
    (svz:server:state-set! server "x-ratio" x-ratio)
    (svz:server:state-set! server "y-ratio" y-ratio)
    0
    ))

;; pure integer division for k by n
(define (divide k n)
  (inexact->exact (floor (/ k n))))

;; fill left side string with zeros
(define (left-zeros text n)
  (let loop ((i (string-length text)))
    (if (< i n) (begin
		  (set! text (string-append "0" text))
		  (loop (1+ i)))))
  text)

;; create an ascii representation for the given index i, each character is
;; limited to a certain range, creates a string with bpp (bytes per pixel)
;; characters
(define (generate-ascii range bpp i)
  (let* ((ascii (make-string bpp #\space)))
    (let loop ((n 0) (i i))
      (if (< n bpp)
	  (let ((c (integer->char (+ (modulo i range)
				     (char->integer #\space)))))
	    (if (equal? c #\")
		(set! c (integer->char (+ range (char->integer #\space)))))
	    (string-set! ascii n c)
	    (loop (1+ n) (divide i range)))))
    ascii))

;; generate a colourful palette
(define (mandel-palette server)
  (let* ((colors (svz:server:config-ref server "colors"))
	 (rgb (cons (make-vector colors) (make-vector colors)))
	 (bpp 1))
    (let loop ((i (1- colors)) (r 0) (g 0) (b 0))
      (if (>= i 0)
	  (let* ((val 0) 
		 (char-range (- (char->integer #\~) (char->integer #\space))))

	    (set! bpp (let loop ((n 1) (cols char-range))
			(if (>= cols colors) n
			    (loop (1+ n) (* cols char-range)))))
	    (set! val (+ (inexact->exact (* 127 (- 1 (cos b))))
			 (* 256 (inexact->exact (* 127 (- 1 (cos g)))))
			 (* 256 256 (inexact->exact (* 127 (- 1 (cos r)))))))
	    (vector-set! (car rgb) i (generate-ascii char-range bpp i))
	    (vector-set! (cdr rgb) i (left-zeros (number->string val 16) 6))
	    (loop (1- i) (+ r 0.2) (+ g 0.02) (+ b 0.33)))))
    (svz:server:state-set! server "palette" rgb)
    (svz:server:state-set! server "bpp" (number->string bpp))))

;; save the generated palette to a file
(define (mandel-save-palette server outfile)
  (let* ((rgb (svz:server:state-ref server "palette"))
	 (size (vector-length (car rgb))))
    (let loop ((i 0))
      (if (< i size)
	  (begin
	    (write-line (string-append "\""
				       (vector-ref (car rgb) i)
				       "\tc #"
				       (vector-ref (cdr rgb) i)
				       "\",")
			outfile)
	    (loop (1+ i)))))))

;; write out a finished xpm picture
(define (mandel-write server)
  (let* ((data (svz:server:state-ref server "data"))
	 (x-res (svz:server:config-ref server "x-res"))
	 (y-res (svz:server:config-ref server "y-res"))
	 (colors (svz:server:config-ref server "colors"))
	 (outfile (open-output-file (svz:server:config-ref server "outfile")))
	 (cols '()))

    (display (string-append "*** Generating \""
			    (svz:server:config-ref server "outfile")
			    "\" ... "))

    ;; create color palette
    (mandel-palette server)
    (set! cols (car (svz:server:state-ref server "palette")))

    ;; create header
    (write-line "/* XPM */" outfile)
    (write-line "static char * mandel_xpm[] = {" outfile)
    (write-line (string-append "\""
			       (number->string x-res)
			       " "
			       (number->string y-res)
			       " "
			       (number->string colors)
			       " "
			       (svz:server:state-ref server "bpp")
			       "\",")
		outfile)

    ;; write palette information
    (mandel-save-palette server outfile)

    ;; output picture data
    (do ((y 0 (1+ y))) ((>= y y-res))
      (let ((line "\""))
	(do ((x 0 (1+ x))) ((>= x x-res))
	  (set! line
		(string-append line (vector-ref
				     cols
				     (vector-ref data (+ x (* y x-res)))))))
	(set! line (string-append line "\""))
	(if (not (= y (1- y-res)))
	    (set! line (string-append line ","))
	    (set! line (string-append line "};")))
	(write-line line outfile)))
    
    (close-output-port outfile)
    (display "done.\n")))

;; check if all points were calculated
(define (finished? server)
  (<= (svz:server:state-ref server "missing") 0))

;; store a calculated point
(define (save-point! server index value)
  (if (< (vector-ref (svz:server:state-ref server "data") index) 0)
      (begin
	(vector-set! (svz:server:state-ref server "data") index value)
	(svz:server:state-set! server "missing"
			       (1- (svz:server:state-ref server "missing"))))))

;; calculate the complex number at a given array index
(define (index->z server index)
  (let* ((x (modulo index (svz:server:config-ref server "x-res")))
	 (y (divide index (svz:server:config-ref server "x-res")))
	 (offset (make-rectangular 
		  (* (+ x 0.5) (svz:server:state-ref server "x-ratio"))
		  (* (+ y 0.5) (svz:server:state-ref server "y-ratio"))))
	 (z (+ (string->number (svz:server:config-ref server "start")) 
	       offset)))
    z))

;; determine the next index to be calculated
(define (next-index! server)
  (let* ((index (svz:server:state-ref server "index"))
	 (data (svz:server:state-ref server "data"))
	 (size (vector-length data)))
    (set! index (let loop ((i index))
		  (if (>= i size)
		      (set! i 0)) 
		  (if (< (vector-ref data i) 0) i (loop (1+ i)))))
    (svz:server:state-set! server "index" (1+ index))
    index))

;; detect our client with a magic string
(define (mandel-detect-proto server sock)
  (let ((idx (binary-search (svz:sock:receive-buffer sock) mandel-magic)))
    (if (and idx (= idx 0))
	(begin
	  (svz:sock:receive-buffer-reduce sock (string-length mandel-magic))
	  -1)
	0)))

;; server information callback for the control protocol
(define (mandel-info server)
  (string-append
   " Mandelbrot calculation server.\r\n"
   " Points given for calculation: "
   (number->string (svz:server:state-ref server "index")) "\r\n"
   " Missing points: "
   (number->string (svz:server:state-ref server "missing"))))

;; connect a new client
(define (mandel-connect server sock)
  (mandel-prepare-sock sock)
  (svz:sock:handle-request sock mandel-handle-request)
  (if (finished? server)
      (begin 
	(svz:sock:final-print sock)
	(svz:sock:print sock "(dnc:bye)\r\n") 0)
      (begin
	(svz:sock:print sock "(dnc:welcome)\r\n") 0)))

;; server instance finalizer callback
(define (mandel-finalize server)
  (if (and (finished? server) (system))
      (begin
	(system (string-append (svz:server:config-ref server "viewer") " "
			       (svz:server:config-ref server "outfile")))
	)) 0)

;; handle one request by a client
(define (mandel-handle-request sock request len)
  (let* ((server (svz:sock:server sock))
	 (colors (svz:server:config-ref server "colors"))
	 (tokens (mandel-split (binary->string request)))
	 (command (list-ref tokens 1)))

    (cond 
     ;; client wants to disconnect
     ((equal? command "bye")
      -1
      )
     ;; client awaits new input data     
     ((equal? command "request")
      (if (finished? server)
	  -1
	  (let* ((index (next-index! server))
		 (z (index->z server index))
		 (answer (string-append "(dnc:value:"
					(number->string z)
					":"
					(number->string index)
					":"
					(number->string colors)
					")\r\n")))
	    (svz:sock:print sock answer)
	    0)
	  ))
     ;; client has some result
     ((equal? command "value")
      (save-point! server
		   (string->number (list-ref tokens 3))
		   (string->number (list-ref tokens 4)))
      (if (finished? server)
	  (begin
	    (mandel-write server)
	    (serveez-nuke)
	    -1)
	  0))
     ;; invalid protocol
     (else -1))
    ))

;; now make Serveez recognize this as a new server
(define-servertype! '(
		      (prefix         . "mandel")
		      (description    . "Distributed Mandelbrot Fractal")
		      (detect-proto   . mandel-detect-proto)
		      (init           . mandel-init)
		      (finalize       . mandel-finalize)
		      (connect-socket . mandel-connect)
		      (info-server    . mandel-info)
		      (configuration  . (
					 (start   . (string #t "-2.0-1.5i"))
					 (end     . (string #t "+1.1+1.5i"))
					 (x-res   . (integer #t 320))
					 (y-res   . (integer #t 240))
					 (colors  . (integer #t 256))
					 (outfile . (string #t "mandel.xpm"))
					 (viewer  . (string #t "xv"))
					 ))))

;; throw the pile together
(define-port! 'mandel-port '(
			     (proto . tcp)
			     (port . 1025)
			     (ipaddr . *)))

(define-server! 'mandel-server '(
				 (x-res . 100)
				 (y-res . 100)
				 ))

(define-server! 'control-server '())

(bind-server! 'mandel-port 'mandel-server)
(bind-server! 'mandel-port 'control-server)
