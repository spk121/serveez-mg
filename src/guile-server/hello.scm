;; The Hello World server

(use-modules (rnrs bytevectors)
	     (serveez-mg lib))

;; The protocol detection function: this function is called the 
;; first time a client connects.  The client is expected to
;; identify itself by sending the string "HELLO".
(define (hello-detect-proto server socket)
  (let ((input-bytes (svz:sock:receive-buffer socket))
	(remote-address (svz:sock:remote-address socket)))

    (if (bytevector-contains (svz:sock:receive-buffer socket) 
			     (string->utf8 "HELLO"))
	;; If true, allow connection
	(begin 
	  (format #t "Valid connection from ~a:~a~%" 
		  (inet-ntop AF_INET (htonl (car remote-address)))
		  (cdr remote-address))
	  1)
	;; Otherwise, ignore
	0)))

;; The connect socket function: after a client has been detected by
;; detect-proto, this function handles each request
(define (hello-connect-socket server socket)
  ;; Each packet is going to be delimited by CR/LF
  (svz:sock:boundary socket "\r\n")
  ;; Connect to the request handler
  (svz:sock:handle-request socket hello-handle-request)
  ;; Zero indicates success
  0)

(define (strip-boundary str)
  (string-trim-right str (string->char-set "\r\n")))

;; The request handler.
;; If it receives "HELLO", return "HELLO, WORLD!".
;; If it receives "WHAT TIME IT IS?", return the time.
;; Otherwise, return ERROR.
(define (hello-handle-request socket request len)
  (let ((command (strip-boundary (utf8->string request))))
    (cond
     ((string=? command "HELLO")
      (svz:sock:print socket "HELLO, WORLD!\r\n"))
     ((string=? command "WHAT TIME IS IT?")
      (svz:sock:print socket (strftime "%c" (localtime (current-time))))
      (svz:sock:print socket "\r\n"))
     (else
      (svz:sock:print socket "ERROR\r\n")))
    ;; Return zero to indicate success
    0))

(define (hello-init server)
  (display "HELLO ")
  (display server)
  (newline)
  0)

;; Port configuration
(define-port! 'hello-port '((proto . tcp)
			    (port . 3003)))

;; Servertype definitions
(define-servertype! 
  '((prefix  . "hello")
    (description . "guile hello world server")
    (detect-proto . hello-detect-proto)
    (init . hello-init)
    (connect-socket . hello-connect-socket)
    (configuration . ())
    ))

;; Server instantiation
(define-server! 'hello-server '())

(bind-server! 'hello-port 'hello-server)