;; -*-scheme-*-
;;
;; mandel-shared.scm - mandelbrot fractal helper functionality
;;
;; Copyright (C) 2001 Raimund Jacob <raimi@lkcc.org>
;; Copyright (C) 2001 Stefan Jahn <stefan@lkcc.org>,
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

;;
;; The distributed Mandelbrot server is an Internet server completely written
;; in Guile with the help of the API provided by the underlying Serveez
;; application.  The reader will not see any occurrence of the networking API
;; of Guile.
;;
;; dnc - short for "Distributed Number Cruncher".  The Mandelbrot server
;; manages the computation of a graphic visualization of the Mandelbrot set
;; fractal.  Each client can connect to the server and ask for something
;; to calculate and is meant to send its result back to the server.
;;

;; splits mandel protocol text into tokens, returns list of tokens
(define (mandel-split text)
  (let* ((tokens '())
         (text (substring text 1 (- (string-length text) 3))))
    (let loop ((i 0))
      (if (< i (string-length text))
          (begin
            (let* ((idx (string-index text #\: i))
                   (end (if idx idx (string-length text))))
              (set! tokens (append tokens `(,(substring text i end))))
              (loop (1+ end))
              ))
          ))
    tokens))

;; prepare the given Serveez socket for our protocol
(define (mandel-prepare-sock sock)
  (svz:sock:no-delay sock #t)
  (svz:sock:boundary sock "\r\n")
  (svz:sock:floodprotect sock #f))

;; define magic detection string
(define mandel-magic "(dnc:mandel)\r\n")
