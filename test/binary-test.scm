;; -*-scheme-*-
;;
;; binary-test.scm - binary function test suite
;;
;; Copyright (C) 2002 Stefan Jahn <stefan@lkcc.org>
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
;; $Id: binary-test.scm,v 1.4 2003/06/01 12:57:07 ela Exp $
;;

;; Load the test suite module.
(use-modules (test-suite))

;; This code exports the public symbols of the guile-user module.
(if (defined? 'micro-version)
    (begin
      (export serveez-verbosity)
      (export serveez-exceptions)
      (export serveez-nuke)))

;; Run the test suite for the binary smob used in serveez.
(test-suite "binary function test suite"

  (pass-if "binary?"
	   (and (not (binary? "foo"))
		(binary? (string->binary "foo"))))

  (pass-if "binary->string and string->binary"
	   (and (equal? "foo" (binary->string (string->binary "foo")))
		(equal? "" (binary->string (string->binary "")))
		(equal? (string->binary "") (string->binary ""))))

  (pass-if-exception "string->binary exception 1"
		     'wrong-type-arg
		     (binary-search (string->binary "foo") 'foo))

  (pass-if-exception "string->binary exception 2"
		     'wrong-type-arg
		     (binary-search (string->binary "foo") 0.5))

  (pass-if "binary-search strings"
	   (let ((foobar (string->binary "foobar")))
	     (and (not (binary-search foobar "x"))
		  (= 0 (binary-search foobar "foo"))
		  (= 0 (binary-search foobar "foobar"))
		  (not (binary-search foobar "foobar_"))
		  (not (binary-search foobar ""))
		  (= 1 (binary-search foobar "oo"))
		  (= 3 (binary-search foobar "bar"))
		  (= 5 (binary-search foobar "r")))))

  (pass-if "binary-search binaries"
	   (let ((foobar (string->binary "foobar")))
	     (and (not (binary-search foobar (string->binary "x")))
		  (= 0 (binary-search foobar (string->binary "foo")))
		  (= 0 (binary-search foobar (string->binary "foobar")))
		  (not (binary-search foobar (string->binary "foobar_")))
		  (not (binary-search foobar (string->binary "")))
		  (= 1 (binary-search foobar (string->binary "oo")))
		  (= 3 (binary-search foobar (string->binary "bar")))
		  (= 5 (binary-search foobar (string->binary "r"))))))

  (pass-if "binary-search characters"
	   (let ((foobar (string->binary "foobar")))
	     (and (not (binary-search foobar #\x))
		  (= 0 (binary-search foobar #\f))
		  (= 1 (binary-search foobar #\o))
		  (= 3 (binary-search foobar #\b))
		  (= 5 (binary-search foobar #\r)))))

  (pass-if "binary-search numbers"
	   (let ((foobar (string->binary "foobar")))
	     (and (not (binary-search foobar (char->integer #\x)))
		  (= 0 (binary-search foobar (char->integer #\f)))
		  (not (binary-search foobar 0))
		  (= 1 (binary-search foobar (char->integer #\o)))
		  (= 3 (binary-search foobar (char->integer #\b)))
		  (= 5 (binary-search foobar (char->integer #\r))))))

  (pass-if "list->binary and binary->list"
	   (let ((foo (list->binary (list 1 2 3))))
	     (and (equal? (list 1 2 3) (binary->list foo))
		  (equal? (list 1 2) (binary->list (list->binary (list 1 2))))
		  (equal? (list) (binary->list (list->binary (list)))))))

  (pass-if-exception "list->binary exception 1"
		     'wrong-type-arg
		     (list->binary 1))

  (pass-if-exception "list->binary exception 2"
		     'out-of-range
		     (list->binary (list -129)))

  (pass-if-exception "list->binary exception 3"
		     'wrong-type-arg
		     (list->binary (list 0.5)))

  (pass-if "binary-set!"
	   (let ((foobar (string->binary "foobar")))
	     (and (binary-set! foobar 0 #\b)
		  (binary-set! foobar 3 #\f)
		  (equal? (binary->string foobar) "boofar")
		  (binary-set! foobar 3 #\r)
		  (binary-set! foobar 5 #\f)
		  (equal? (binary->string foobar) "booraf")
		  (binary-set! foobar 5 0)
		  (equal? (binary->string foobar) "boora\0"))))

  (pass-if-exception "binary-set! exception"
		     'out-of-range
		     (binary-set! (string->binary "foo") 3 0))
		  
  (pass-if "binary-ref"
	   (let ((foobar (string->binary "foobar")))
	     (and (equal? (binary-ref foobar 0) #\f)
		  (equal? (binary-ref foobar 3) #\b)
		  (equal? (binary-ref foobar 5) #\r))))

  (pass-if-exception "binary-ref exception"
		     'out-of-range
		     (binary-ref (string->binary "foo") 3))

  (pass-if "binary-length"
	   (and (= 0 (binary-length (string->binary "")))
		(= 3 (binary-length (string->binary "foo")))
		(= 6 (binary-length (string->binary "foobar")))))

  (pass-if-exception "binary-length exception"
		     'wrong-type-arg
		     (binary-length "foo"))

  (pass-if "binary-concat!"
	   (let ((foobar (string->binary "foobar"))
		 (bar (string->binary "bar"))
		 (foo (string->binary "foo"))
		 (none (string->binary "")))
	     (and (equal? (binary-concat! foo bar) foobar)
		  (equal? (binary-concat! none none) none)
		  (equal? (binary-concat! (string->binary "") bar) bar)
		  (equal? (binary-concat! bar "") bar)
		  (not (equal? (binary-concat! bar foo) foobar)))))

  (pass-if-exception "binary-concat! exception"
		     'wrong-type-arg
		     (binary-concat! (string->binary "foo") 'bar))
  
  (pass-if "binary-subset"
	   (let ((foobar (string->binary "foobar")))
	     (and (equal? (binary->string (binary-subset foobar 0 2)) "foo")
		  (equal? (binary->string (binary-subset foobar 3 5)) "bar")
		  (equal? (binary->string (binary-subset foobar 0 5)) "foobar")
		  (equal? (binary->string (binary-subset foobar 0)) "foobar")
		  (equal? (binary->string (binary-subset foobar 5)) "r")
		  (equal? (binary->string (binary-subset foobar 5 5)) "r")
		  (equal? (binary->string (binary-subset foobar 0 0)) "f"))))

  (pass-if-exception "binary-subset exception 1"
		     'wrong-type-arg
		     (binary-subset (string->binary "foo") 'foo))

  (pass-if-exception "binary-subset exception 2"
		     'out-of-range
		     (binary-subset (string->binary "foo") 3))

  (pass-if-exception "binary-subset exception 3"
		     'out-of-range
		     (binary-subset (string->binary "foo") 2 1))

  (pass-if "binary-reverse!"
	   (let ((foobar (string->binary "foobar"))
		 (none (string->binary "")))
	     (and (equal? (binary->string (binary-reverse! foobar)) "raboof")
		  (equal? (binary->string (binary-reverse! foobar)) "foobar")
		  (equal? (binary->string (binary-reverse! none)) ""))))

  (pass-if "binary-reverse"
	   (let ((foobar (string->binary "foobar"))
		 (none (string->binary "")))
	     (and (equal? (binary->string (binary-reverse foobar)) "raboof")
		  (equal? (binary->string foobar) "foobar")
		  (equal? (binary->string (binary-reverse none)) ""))))

  )
