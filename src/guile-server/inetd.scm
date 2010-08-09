;; -*-scheme-*-
;;
;; inetd.scm - replacement for the Internet ``super-server''
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
;; $Id: inetd.scm,v 1.14 2002/02/06 06:54:56 ela Exp $
;;

;; the inetd configuration file
(define config-file "/etc/inetd.conf")

;; working directory
(define directory "/tmp")

;; should inetd use fork() and exec() ?
(define do-fork #t)

;; print some messages if #t
(define verbose #f)

;; bind servers to this network address or device
;; (define ip-address "*")
(define ip-address "any")

;; opens a configuration file, reads each line and uncomments them,
;; comments are marked with a leading '#', returns a list of non-empty
;; lines
(define (read-config file)
  (catch #t
	 (lambda ()
	   (let* ((f (open-input-file file))
		  (lines '()))
	     (let loop ((line (read-line f)))
	       (if (not (eof-object? line))
		   (let ((n (string-index line #\#)))
		     (if n (set! line (substring line 0 n)))
		     (if (> (string-length line) 0)
			 (set! lines (cons line lines)))
		     (loop (read-line f)))))
	     (close-input-port f)
	     (reverse lines)))
	 (lambda args
	   (display (string-append "inetd: unable to parse `"
				   file "'\n"))
	   '())))

;; splits a string in the format "string1.string2" into a pair.  if 
;; ".string2" is omitted the (cdr) defaults to #f.  the default delimiter
;; for a tuple is #\. and can be set via the last argument.
(define (split-tuple string . c)
  (let* ((c (if (pair? c) (car c) #\.))
	 (i (string-index string c)))
    (if i
	(cons (substring string 0 i) (substring string (1+ i)))
	(cons string #f))))

;; removes leading white spaces from the given string and returns it
(define (crop-spaces string)
  (let ((ret string))
    (if (> (string-length string) 0)
	(let loop ((c (string-ref string 0)))
	  (if (<= (char->integer c) (char->integer #\space))
	      (begin
		(set! string (substring string 1))
		(if (> (string-length string) 0)
		    (loop (string-ref string 0)))))))
    string))

;; returns the next position of a white space character in the given 
;; string or #f if there is no further space or the string was empty
(define (next-space-position string)
  (let ((i 0))
    (let loop ((i 0))
      (if (< i (string-length string))
	  (if (<= (char->integer (string-ref string i)) 
		  (char->integer #\space))
	      i
	      (loop (1+ i)))
	  #f))))

;; tokenizes a given string into a vector, if TOKENS is a number the 
;; procedure parses this number of tokens and stores the remaining tokens as
;; a variable length vector in the last element of the returned vector, 
;; otherwise it returns a variable length vector
(define (string-split string . tokens)
  (let* ((vector (if (pair? tokens) (make-vector (1+ (car tokens))) '())))
    (define (next-token string i)
      (if i (substring string 0 i) string))
    (let loop ((n 0))
      (set! string (crop-spaces string))
      (let* ((i (next-space-position string)))
	(if (pair? tokens)
	    (vector-set! vector n (next-token string i))
	    (set! vector (append vector `(,(next-token string i)))))
	(if i
	    (set! string (substring string i))
	    (set! string ""))
	(if (and i (or (not (pair? tokens)) (< n (1- (car tokens)))))
	    (loop (1+ n)))))
    (if (pair? tokens)
	(vector-set! vector (car tokens) (string-split string))
	(set! vector (if (null? vector) #f (list->vector vector))))
    vector))

;; returns the full rpc entry for a given service name or #f if there is
;; no such service found in the file `/etc/rpc'
(define (lookup-rpc-service name)
  (catch #t
	 (lambda ()
	   (getrpc name))
	 (lambda key
	   (display (string-append "inetd: no such rpc service `" name "'\n"))
	   #f)))

;; this procedure translates a given service line into a pair containing
;; the full rpc entry as a vector and their versions (also as a pair of 
;; numbers)
(define (get-rpc-service service-line)
  (let* ((entry (split-tuple (vector-ref service-line 0) #\/))
	 (name (car entry))
	 (versions (if (cdr entry) 
		       (split-tuple (cdr entry) #\-) 
		       (cons "1" "1")))
	 (version-begin (car versions))
	 (version-end (if (cdr versions) (cdr versions) version-begin)))
    (if (not (cdr entry))
	(display (string-append "inetd: " name ": no rpc version\n")))
    (cons (lookup-rpc-service name) 
	  (cons (string->number version-begin) 
		(string->number version-end)))))

;; creates a unique name suffix for rpc servers and port configurations
(define (protocol-rpc-string rpc proto)
  (string-append proto "-" rpc))

;; creates and defines a rpc port configuration.  the network port is set
;; to zero in order to let the system choose one
(define (create-rpc-portcfg rpc proto)
  (let* ((port '()) 
	 (name "undefined"))
    (set! port (cons (cons "proto" proto) port))
    (set! port (cons (cons "ipaddr" ip-address) port))
    (set! port (cons (cons "port" 0) port))
    (set! name (string-append "inetd-port-"
			      (protocol-rpc-string rpc proto)))
    (define-port! name port)
    name))

;; creates and defines a rpc server instance from the given splitted service
;; line.   
(define (create-rpc-server line rpc proto)
  (let* ((server '()) 
	 (name "undefined")
	 (threads (split-tuple (vector-ref line 3))))
    (set! server (cons (cons "binary" (service-binary line)) server))
    (set! server (cons (cons "directory" directory) server))
    (set! server (cons (cons "user" (vector-ref line 4)) server))
    (set! server (cons (cons "do-fork" do-fork) server))
    (set! server (cons (cons "single-threaded"
			     (equal? (car threads) "wait")) 
		       server))
    (if (cdr threads)
	(set! server (cons (cons "thread-frequency"
				 (string->number (cdr threads)))
			   server)))
    (let ((argv (vector-ref line 6)))
      (if argv
	  (set! server (cons (cons "argv" (vector->list argv))
			     server))))
    (set! name (string-append "prog-server-"
			      (protocol-rpc-string rpc proto)))
    (define-server! name server)
    name))

;; returns either "tcp" or "udp" needed to determine the kind of port 
;; configuration the identd has to define
(define (rpc-protocol service-line)
  (cdr (split-tuple (vector-ref service-line 2) #\/)))

;; converts a given network protocol name into a valid ip protocol
;; identifier
(define (rpc-ip-proto service-line)
  (if (equal? (rpc-protocol service-line) "tcp")
      IPPROTO_TCP
      IPPROTO_UDP))

;; checks whether the rpc service identified by [number,version] is already
;; registered at the portmapper and returns #t if so.  otherwise the 
;; procedure returns #f.
(define (check-rpc-portmapper number version)
  (let ((mappings (portmap-list)))
    (let loop ((mapping mappings))
      (if (null? mapping)
	  #f
	  (or (and (equal? (vector-ref (car mapping) 0) number)
		   (equal? (vector-ref (car mapping) 1) version))
	      (loop (cdr mapping)))))))

;; this procedure registers the rpc service identified by the triplet
;; [number,version,protocol] at a network port system wide.  this 
;; information can be obtained issuing the `rpcinfo -p' command.
(define (run-rpc-portmapper name number version protocol port)
  (let ((result #f))
    ;; should the previous setting really be disabled ?
    (catch #t
	   (lambda ()
	     (if (check-rpc-portmapper number version)
		 (begin
		   (if verbose
		       (display
			(string-append "inetd: unregistering rpc service `" 
				       name "'\n")))
		   (portmap number version)))
	     (set! result #t))
	   (lambda key
	     (display (string-append "inetd: portmapping for rpc service `" 
				     name "' cannot be unregistered\n"))
	     (set! result #f)))

    (catch #t
	   (lambda ()
	     (portmap number version protocol port)
	     #t)
	   (lambda key
	     (display (string-append "inetd: portmapper for rpc service `" 
				     name "' failed\n"))
	     #f))))

;; when the inetd determines a valid rpc line in its configuration file
;; this procedure is called.
(define (bind-rpc-service service-line)
  (let* ((service (get-rpc-service service-line))
	 (rpc (car service))
	 (versions (cdr service)))
    (if rpc
	;; create port configuration and server
	(let* ((name (vector-ref rpc 0))
	       (proto (rpc-protocol service-line))
	       (port (create-rpc-portcfg name proto))
	       (server (create-rpc-server service-line name proto)))
	  ;; bind the server to its port
	  (if verbose
	      (display (string-append "inetd: binding `"
				      server
				      "' to `"
				      port
				      "'\n")))
	  (bind-server! port server)

	  ;; now go through each listening socket structure the server got
	  ;; finally bound to
	  (for-each (lambda (sock)
		      ;; obtain the local network port
		      (let ((port (cdr (svz:sock:local-address sock))))
			;; for each version specified in the original
			;; service line
			(do ((version (car versions) (+ version 1)))
			    ((> version (cdr versions)))
			  ;; create a port-mapping
			  (run-rpc-portmapper (vector-ref rpc 0)
					      (vector-ref rpc 2) version
					      (rpc-ip-proto service-line)
					      (ntohs port)))
			))
		    ;; get the listening socket sructures
		    (svz:server:listeners server))
	  ))))

;; this checks if the given service line specifies a rpc service or not
(define (rpc-service? service-line)
  (cdr (split-tuple (vector-ref service-line 2) #\/)))

;; returns a service with fully qualified port, protocol, service
;; name and its aliases if there is such a service, otherwise the
;; procedure returns #f
(define (lookup-service service-line)
  (catch #t
	 (lambda ()
	   (getservbyname (vector-ref service-line 0)
			  (vector-ref service-line 2)))
	 (lambda key
	   (display (string-append "inetd: no such service `"
				   (vector-ref service-line 0)
				   "', protocol `"
				   (vector-ref service-line 2)
				   "'\n"))
	   #f)))

;; returns the name of the program which is meant to be started for a given
;; service line
(define (service-binary service)
  (vector-ref service 5))

;; this procedure returns #t if the service specified by the given service
;; line is explicitely disabled by the keywork `disable'
(define (service-disabled? service)
  (equal? (service-binary service) "disable"))

;; creates a "protocol-port" text representation from a service vector 
;; returned by (lookup-service)
(define (protocol-port-string service)
  (string-append (vector-ref service 3)
		 "-"
		 (number->string (vector-ref service 2))))

;; creates a port configuration for Serveez, returns the name of the new
;; port or #f on failure
(define (create-portcfg line)
  (let* ((service (lookup-service line))
	 (port '()) (name "undefined"))
    (if service
	(begin
	  (set! port (cons (cons "proto" (vector-ref service 3)) port))
	  (set! port (cons (cons "ipaddr" ip-address) port))
	  (set! port (cons (cons "port" (vector-ref service 2)) port))
	  (set! name (string-append "inetd-port-"
				    (protocol-port-string service)))
	  (define-port! name port)
	  name)
	#f)))

;; creates a program passthrough server for Serveez, returns the name of
;; the new server or #f on failure
(define (create-server line)
  (let* ((service (lookup-service line))
	 (server '()) 
	 (name "undefined")
	 (threads (split-tuple (vector-ref line 3))))
    (if service
	(begin
	  (if (equal? (service-binary line) "internal")
	      (set! name (translate-internal-server line service))
	      (begin
		(set! server (cons (cons "binary" (service-binary line)) 
				   server))
		(set! server (cons (cons "directory" directory) server))
		(set! server (cons (cons "user" (vector-ref line 4)) server))
		(set! server (cons (cons "do-fork" do-fork) server))
		(set! server (cons (cons "single-threaded"
					 (equal? (car threads) "wait")) 
				   server))
		(if (cdr threads)
		    (set! server (cons (cons "thread-frequency"
					     (string->number (cdr threads)))
				       server)))
		(let ((argv (vector-ref line 6)))
		  (if argv
		      (set! server (cons (cons "argv" (vector->list argv))
					 server))))
		(set! name (string-append "prog-server-"
					  (protocol-port-string service)))
		(define-server! name server)))
	  name)
	#f)))

;; translates the inetd servers marked with `internal' into Serveez 
;; servers if possible
(define (translate-internal-server line service)
  (let* ((server '()) 
	 (name (vector-ref service 0))
	 (threads (split-tuple (vector-ref line 3))))
    (if verbose
	(display (string-append "inetd: translating internal `"
				name "' server\n")))
    (cond
     ;; timeserver
     ((equal? name "time")
      (if (serveez-servertype? "sntp")
	  (begin
	    (set! name (string-append "sntp-server-"
				      (protocol-port-string service)))
	    (define-server! name server)
	    name)))
     ;; echo
     ((equal? name "echo")
      #f)
     ;; sink
     ((equal? name "discard")
      #f)
     ;; daytime
     ((equal? name "daytime")
      #f)
     ;; ttytst source
     ((equal? name "chargen")
      #f)
     (else #f))))

;; glues the above port configurations and servers together:
;;   this is achieved by splitting the lines of the configuration file
;;   into tokens.  each inetd line looks like
;;     (service name) (socket type) (protocol) (wait/nowait[.max]) \
;;     (user[.group]) (server program) (server program arguments)
;;   that is why we split this line into 6 tokens with an additional rest 
;;   token, pass them to the port configuration and server builders and
;;   finally bind the server to the port.
(define (create-bindings lines)
  (for-each 
   (lambda (line)
     (let ((service (string-split line 6)))
       (if (not (service-disabled? service))
	   (if (rpc-service? service)
	       (bind-rpc-service service)
	       (begin
		 (let* ((port (create-portcfg service))
			(server (create-server service)))
		   (if (and port server)
		       (begin
			 (if verbose
			     (display (string-append "inetd: binding `"
						     server
						     "' to `"
						     port
						     "'\n")))
			 (bind-server! port server)))))))))
   lines))

;; main program entry point
(define (inetd-main)
  (let ((lines (read-config config-file)))
    (create-bindings lines)))

;; run
(inetd-main)
