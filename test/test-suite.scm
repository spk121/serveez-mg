;; -*-scheme-*-
;;
;; test-suite.scm - test suite library
;;
;; Copyright (C) 2002, 2003 Stefan Jahn <stefan@lkcc.org>
;;
;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this package.  If not, see <http://www.gnu.org/licenses/>.

;; Module definition.
(define-module (test-suite))

;; Import the public serveez symbols.
(if (defined? 'micro-version)
    (use-modules (guile-user)))

;; This is the test suite library's core.  It evaluates the 'test'
;; expression giving it the title 'description' and compares its result
;; with the 'expected' value.  If you pass a valid 'exception' symbol it
;; is possible to verify that 'test' throws this exception.
(define (run-test description expected exception test)
  (catch #t
         (lambda ()
           (let ((result (test)))
             (if (eq? result expected)
                 (throw 'pass)
                 (throw 'fail))))
         (lambda (key . args)
           (cond
            ((eq? key 'pass)
             (report description "ok"))
            ((eq? key 'fail)
             (report description "failed"))
            ((eq? key 'quit)
             (report description "fatal failure (exited)"))
            ((eq? key exception)
             (report description "ok"))
            (else
             (report description "unexpected exception")
             (apply throw key args))))))

;; Passes both arguments to 'run-test' and expects 'test' to return #t.
;; No exceptions allowed.  The test fails if 'test' returns other than #t
;; or throws an exception.
(defmacro pass-if (description test)
  `(run-test ,description #t 'invalid (lambda () ,test)))

;; Convenience macro similar to 'pass-if' but checks whether the expression
;; 'test' throws the 'exception' symbol.
(defmacro pass-if-exception (description exception test)
  `(run-test ,description #t ,exception (lambda () ,test)))

;; Displays a given test 'description' with the result 'result'.  The
;; displayed line is properly formatted.
(define (report description result)
  (let* ((text description) (n 40))
    (let loop ((i (string-length text)))
      (if (< i n) (begin
                    (set! text (string-append " " text))
                    (loop (+ i 1)))))
    (display text)
    (display ": ")
    (display result)
    (display "\r\n")))

;; Main entry point for a test suite using this library.  Display the test
;; suite description 'title' and runs any given 'test' expression which is
;; usually a number of 'pass-if' statements.
(define (run-test-suite title test)
  (serveez-verbosity 0)
  (serveez-exceptions #f)
  (display title)
  (display "\r\n")
  (test)
  (serveez-nuke))

;; Convenience macro for the above main entry point.  Use this from real
;; test suite files.
(defmacro test-suite (title . test)
  `(run-test-suite ,title (lambda () ,@test)))

;; Export these procedures and macros to be public.
(export test-suite
        run-test-suite
        run-test
        pass-if
        pass-if-exception)
