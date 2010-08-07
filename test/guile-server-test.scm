;; -*-scheme-*-
;;
;; guile-server-test.scm - Guile server definition test suite
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
;; $Id: guile-server-test.scm,v 1.1 2003/06/29 09:21:28 ela Exp $
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
(test-suite "guile server definition test suite"

  ;; Testing (define-servertype!) ...
  (pass-if "No such procedure"
	   (not (define-servertype! '(
		  (prefix        . "test")
		  (description   . "guile test server")
		  (init          . test-init)
		  (configuration . ())
		  ))))

  (pass-if "Invalid procedure"
	   (not (define-servertype! '(
		  (prefix        . "test")
		  (description   . "guile test server")
		  (init          . #t)
		  (configuration . ())
		  ))))

  (pass-if "Invalid integer value"
	   (not (define-servertype! '(
		  (prefix        . "test")
		  (description   . "guile test server")
		  (configuration . ((test-integer . (integer #t 'foo))))
		  ))))

  (pass-if "Invalid string value 1"
	   (not (define-servertype! '(
		  (prefix        . "test")
		  (description   . "guile test server")
		  (configuration . ((test-string . (string #t 0))))
		  ))))

  (pass-if "Invalid string value 2"
	   (not (define-servertype! '(
		  (prefix        . "test")
		  (description   . "guile test server")
		  (configuration . ((test-port . (portcfg #t 0))))
		  ))))

  (pass-if "No such port configuration"
	   (not (define-servertype! '(
		  (prefix        . "test")
		  (description   . "guile test server")
		  (configuration . ((test-port . (portcfg #t test-port))))
		  ))))

  (pass-if "Invalid boolean value"
	   (not (define-servertype! '(
		  (prefix        . "test")
		  (description   . "guile test server")
		  (configuration . ((test-boolean . (boolean #t foo))))
		  ))))

  (pass-if "Missing configuration"
	   (not (define-servertype! '(
		  (prefix        . "test")
		  (description   . "guile test server")
		  ))))

  (pass-if "Invalid definition"
	   (not (define-servertype! '(
		  (prefix        . "test")
		  (description   . "guile test server")
		  (configuration . ((foo)))
		  ))))

  (pass-if "Invalid type definition"
	   (not (define-servertype! '(
		  (prefix        . "test")
		  (description   . "guile test server")
		  (configuration . ((foo . (0 #t 0))))
		  ))))

  (pass-if "Invalid type"
	   (not (define-servertype! '(
		  (prefix        . "test")
		  (description   . "guile test server")
		  (configuration . ((foo . (bar #t 0))))
		  ))))

  (pass-if "Invalid defaultable value"
	   (not (define-servertype! '(
		  (prefix        . "test")
		  (description   . "guile test server")
		  (configuration . ((foo . (boolean bar 0))))
		  ))))


  (pass-if "Servertype definition"
	   (define-servertype! '(
		  (prefix        . "test")
		  (description   . "guile test server")
		  (configuration . ())
		  )))

  (pass-if "Duplicate servertype definition"
	   (not (define-servertype! '(
		  (prefix        . "test")
		  (description   . "guile test server")
		  (configuration . ())
		  ))))

  (pass-if "Missing prefix"
	   (not (define-servertype! '(
		  (description   . "guile test server")
		  (configuration . ())
		  ))))

  (pass-if "Missing description"
	   (not (define-servertype! '(
		  (prefix        . "test-2")
		  (configuration . ())
		  ))))

  ;; Testing (serveez-exceptions) ...
  (pass-if "Invalid boolean"
	   (not (serveez-exceptions 'foo)))
)
