;; -*-scheme-*-
;;
;; server-test.scm - server and port definition test suite
;;
;; Copyright (C) 2003 Stefan Jahn <stefan@lkcc.org>
;;
;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this package; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;; $Id: server-test.scm,v 1.1 2003/06/01 12:57:07 ela Exp $
;;

;; Load the test suite module.
(use-modules (test-suite))

;; This code exports the public symbols of the guile-user module.
(if (defined? 'micro-version)
    (begin
      (export serveez-verbosity)
      (export serveez-exceptions)
      (export serveez-nuke)))

;; Run the test suite for the server and port definitions used in serveez.
;; Right now it basically tests hopefully all of the error conditions.
(test-suite "server and port definition test suite"

  ;; Testing (define-server!) ...
  (pass-if "Multiple definitions"
	   (not (define-server! 'foo-server-1 '(
						(bar . 100)
						(bar . 100)
						))))
  (pass-if "Unused definition"
	   (not (define-server! 'foo-server-2 '(
						(bar . 100)
						(foo . 100)
						))))
  (pass-if "Invalid key type"
	   (not (define-server! 'foo-server-3 '(
						(1 . 2)
						))))
  (pass-if "Not a pair"
	   (not (define-server! 'foo-server-4 '(
						bar
						))))
  (pass-if "Invalid pairlist"
	   (not (define-server! 'foo-server-5 '(
						(bar . 100) . 0
						))))
  (pass-if "Not a valid hash"
	   (not (define-server! 'foo-server-6 '(
						(bar . 100)
						(assoc . 0)
						))))
  (pass-if "Not a pair in hash"
	   (not (define-server! 'foo-server-7 '(
						(bar . 100)
						(assoc . (0))
						))))
  (pass-if "Not a valid key in hash"
	   (not (define-server! 'foo-server-8 '(
						(bar . 100)
						(assoc . ((0 . "great")))
						))))
  (pass-if "Not a valid value in hash"
	   (not (define-server! 'foo-server-9 '(
						(bar . 100)
						(assoc . (("GNU" . 0)))
						))))
  (pass-if "Not a valid list for string array"
	   (not (define-server! 'foo-server-10 '(
						 (bar . 100)
						 (messages . 0)
						 ))))
  (pass-if "Not a valid value in string array"
	   (not (define-server! 'foo-server-11 '(
						 (bar . 100)
						 (messages . (0))
						 ))))
  (pass-if "Not a valid list for integer array"
	   (not (define-server! 'foo-server-12 '(
						 (bar . 100)
						 (ports . 0)
						 ))))
  (pass-if "Not a valid value in integer array"
	   (not (define-server! 'foo-server-13 '(
						 (bar . 100)
						 (ports . ("Hello"))
						 ))))
  (pass-if "No default integer"
	   (not (define-server! 'foo-server-14 '(
						 ))))
  (pass-if "Invalid integer"
	   (not (define-server! 'foo-server-15 '(
						 (bar . "Hello")
						 ))))
  (pass-if "Invalid string"
	   (not (define-server! 'foo-server-16 '(
						 (bar . 100)
						 (reply . 0)
						 ))))
  (pass-if "No such port configuration"
	   (not (define-server! 'foo-server-17 '(
						 (bar . 100)
						 (port . "Hello")
						 ))))
  (pass-if "Invalid port configuration"
	   (not (define-server! 'foo-server-18 '(
						 (bar . 100)
						 (port . 0)
						 ))))
  (pass-if "Invalid boolean"
	   (not (define-server! 'foo-server-19 '(
						 (bar . 100)
						 (truth . "Hello")
						 ))))
  (pass-if "No server name"
	   (not (define-server! 0 '(
				    ))))
  (pass-if "Invalid server name"
	   (not (define-server! 'foo- '(
					))))

  (pass-if "A simple server"
	   (define-server! 'foo-server-20 '(
					 (bar . 0)
					 )))
  (pass-if "Duplicate server"
	   (not (define-server! 'foo-server-20 '(
						 (bar . 1)
						 ))))

  ;; Testing (define-port!) ...
  (pass-if "No port configuration name"
	   (not (define-port! 0 '(
				  ))))
  (pass-if "Field required"
	   (not (define-port! 'foo-port-1 '(
					    ))))
  (pass-if "Invalid proto field"
	   (not (define-port! 'foo-port-2 '(
					    (proto . "foo")
					    ))))
  (pass-if "No port given - TCP"
	   (not (define-port! 'foo-tcp-port-1 '(
						(proto . tcp)
						))))
  (pass-if "Integer required for port - TCP"
	   (not (define-port! 'foo-tcp-port-2 '(
						(proto . tcp)
						(port . "foo")
						))))
  (pass-if "Valid integer required for port - TCP"
	   (not (define-port! 'foo-tcp-port-3 '(
						(proto . tcp)
						(port . -1)
						))))
  (pass-if "Backlog integer required - TCP"
	   (not (define-port! 'foo-tcp-port-4 '(
						(proto . tcp)
						(port . 0)
						(backlog . "foo")
						))))
  (pass-if "IP address as string required - TCP"
	   (not (define-port! 'foo-tcp-port-5 '(
						(proto . tcp)
						(port . 0)
						(ipaddr . 0)
						))))
  (pass-if "Invalid device setting - TCP"
	   (not (define-port! 'foo-tcp-port-6 '(
						(proto . tcp)
						(port . 0)
						(device . 0)
						))))
  (pass-if "No port given - UDP"
	   (not (define-port! 'foo-udp-port-1 '(
						(proto . udp)
						))))
  (pass-if "Integer required for port - UDP"
	   (not (define-port! 'foo-udp-port-2 '(
						(proto . udp)
						(port . "foo")
						))))
  (pass-if "Valid integer required for port - UDP"
	   (not (define-port! 'foo-udp-port-3 '(
						(proto . udp)
						(port . -1)
						))))
  (pass-if "IP address as string required - UDP"
	   (not (define-port! 'foo-udp-port-4 '(
						(proto . udp)
						(port . 0)
						(ipaddr . 0)
						))))
  (pass-if "Invalid device setting - UDP"
	   (not (define-port! 'foo-udp-port-5 '(
						(proto . udp)
						(port . 0)
						(device . 0)
						))))
  (pass-if "Integer required for type - ICMP"
	   (not (define-port! 'foo-icmp-port-1 '(
						 (proto . icmp)
						 (type . "foo")
						 ))))
  (pass-if "Valid integer required for type - ICMP"
	   (not (define-port! 'foo-icmp-port-2 '(
						 (proto . icmp)
						 (type . -1)
						 ))))
  (pass-if "IP address as string required - ICMP"
	   (not (define-port! 'foo-icmp-port-3 '(
						 (proto . icmp)
						 (ipaddr . 0)
						 ))))
  (pass-if "Invalid device setting - ICMP"
	   (not (define-port! 'foo-icmp-port-4 '(
						 (proto . icmp)
						 (device . 0)
						 ))))
  (pass-if "IP address as string required - RAW"
	   (not (define-port! 'foo-raw-port-1 '(
						(proto . raw)
						(ipaddr . 0)
						))))
  (pass-if "Invalid device setting - RAW"
	   (not (define-port! 'foo-raw-port-2 '(
						(proto . raw)
						(device . 0)
						))))
  (pass-if "Missing recv and send pipe"
	   (not (define-port! 'foo-pipe-port-1 '(
						 (proto . pipe)
						 ))))
  (pass-if "Invalid recv value"
	   (not (define-port! 'foo-pipe-port-2 '(
						 (proto . pipe)
						 (recv . 0)
						 (send . "bar")
						 ))))
  (pass-if "Invalid send value"
	   (not (define-port! 'foo-pipe-port-3 '(
						 (proto . pipe)
						 (recv . "foo")
						 (send . 0)
						 ))))
  (pass-if "Invalid recv name value"
	   (not (define-port! 'foo-pipe-port-4 '(
						 (proto . pipe)
						 (recv . ((name . 0)))
						 (send . "bar")
						 ))))
  (pass-if "Invalid recv user value"
	   (not (define-port! 'foo-pipe-port-5 '(
						 (proto . pipe)
						 (recv . ((name . foo) 
							  (user . 0)))
						 (send . "bar")
						 ))))
  (pass-if "Invalid recv group value"
	   (not (define-port! 'foo-pipe-port-6 '(
						 (proto . pipe)
						 (recv . ((name . foo) 
							  (group . 0)))
						 (send . "bar")
						 ))))
  (pass-if "Invalid recv uid value"
	   (not (define-port! 'foo-pipe-port-7 '(
						 (proto . pipe)
						 (recv . ((name . foo) 
							  (uid . "foo")))
						 (send . "bar")
						 ))))
  (pass-if "Invalid recv gid value"
	   (not (define-port! 'foo-pipe-port-8 '(
						 (proto . pipe)
						 (recv . ((name . foo)
							  (gid . "foo")))
						 (send . "bar")
						 ))))
  (pass-if "Invalid recv permissions value"
	   (not (define-port! 'foo-pipe-port-9 '(
				        (proto . pipe)
					(recv . ((name . foo)
						 (permissions . "foo")))
					(send . "bar")
					))))
  (pass-if "Invalid send name value"
	   (not (define-port! 'foo-pipe-port-10 '(
						  (proto . pipe)
						  (recv . "foo")
						  (send . ((name . 0)))
						  ))))
  (pass-if "Invalid send user value"
	   (not (define-port! 'foo-pipe-port-11 '(
						  (proto . pipe)
						  (recv . "foo")
						  (send . ((name . bar) 
							   (user . 0)))
						  ))))
  (pass-if "Invalid send group value"
	   (not (define-port! 'foo-pipe-port-12 '(
						  (proto . pipe)
						  (recv . "foo")
						  (send . ((name . bar)
							   (group . 0)))
						  ))))
  (pass-if "Invalid send uid value"
	   (not (define-port! 'foo-pipe-port-13 '(
						  (proto . pipe)
						  (recv . "foo")
						  (send . ((name . bar)
							   (uid . "bar")))
						  ))))
  (pass-if "Invalid send gid value"
	   (not (define-port! 'foo-pipe-port-14 '(
						  (proto . pipe)
						  (recv . "foo")
						  (send . ((name . bar)
							   (gid . "bar")))
						  ))))
  (pass-if "Invalid send permissions value"
	   (not (define-port! 'foo-pipe-port-15 '(
					(proto . pipe)
					(recv . "foo")
					(send . ((name . bar)
						 (permissions . "bar")))
					))))
  (pass-if "No default recv name value"
	   (not (define-port! 'foo-pipe-port-16 '(
						  (proto . pipe)
						  (recv . ())
						  (send . "bar")
						  ))))
  (pass-if "No default send name value"
	   (not (define-port! 'foo-pipe-port-17 '(
						  (proto . pipe)
						  (recv . "foo")
						  (send . ())
						  ))))
  (pass-if "Invalid send-buffer-size value"
	   (not (define-port! 'foo-port-3 '(
					    (proto . icmp)
					    (send-buffer-size . "foo")
					    ))))
  (pass-if "Invalid recv-buffer-size value"
	   (not (define-port! 'foo-port-4 '(
					    (proto . icmp)
					    (recv-buffer-size . "foo")
					    ))))
  (pass-if "Invalid connect-frequency value"
	   (not (define-port! 'foo-port-5 '(
					    (proto . tcp)
					    (port . 0)
					    (connect-frequency . "foo")
					    ))))
  (pass-if "Invalid allow list"
	   (not (define-port! 'foo-port-6 '(
					    (proto . tcp)
					    (port . 0)
					    (allow . "foo")
					    ))))
  (pass-if "Invalid deny list"
	   (not (define-port! 'foo-port-7 '(
					    (proto . tcp)
					    (port . 0)
					    (deny . "foo")
					    ))))
  (pass-if "A simple tcp port"
	   (define-port! 'foo-port-8 '(
				       (proto . tcp)
				       (port . 0)
				       )))
  (pass-if "Duplicate tcp port"
	   (not (define-port! 'foo-port-8 '(
					    (proto . tcp)
					    (port . 1)
					    ))))


  ;; Testing (bind-server!) ...
  (pass-if "Invalid port name"
	   (not (bind-server! 0 "bar")))
  (pass-if "Invalid server name"
	   (not (bind-server! "foo" 0)))
  (pass-if "No such server and port"
	   (not (bind-server! "foo" "bar")))

)
