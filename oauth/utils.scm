;;; (oauth utils) --- Guile OAuth implementation.

;; Copyright (C) 2021 Aleix Conchillo Flaque <aconchillo@gmail.com>
;;
;; This file is part of guile-oauth.
;;
;; guile-oauth is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; guile-oauth is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with guile-oauth. If not, see https://www.gnu.org/licenses/.

;;; Commentary:

;; OAuth module for Guile

;;; Code:

(define-module (oauth utils)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs bytevectors)
  #:use-module (web http)
  #:use-module (web uri)
  #:use-module (gcrypt base64)
  #:export (oauth-http-basic-auth
            oauth-generate-token
            oauth-query-params
            oauth-parse-www-form-urlencoded))

(define ASCII_ALPHABET "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")

(define (oauth-http-basic-auth username password)
  "Create an HTTP basic authorization credentials header. If username or
password are false return nil."
  (cond
   ((and username password)
    (let* ((value (string-append username ":" password))
           (basic-auth (string-append "Basic " (base64-encode (string->utf8 value)))))
      `((authorization . ,(parse-header 'authorization basic-auth)))))
   (else '())))

(define* (oauth-parse-www-form-urlencoded str #:optional (charset "utf-8"))
  "Parse the string @var{str} of name/value pairs as defined by the
content type application/x-www-form-urlencoded and return and
association list. The keys and values in the association list are
strings."
  (map
   (lambda (piece)
     (let ((equals (string-index piece #\=)))
       (if equals
           (cons (uri-decode (substring piece 0 equals) #:encoding charset)
                 (uri-decode (substring piece (1+ equals)) #:encoding charset))
           (cons (uri-decode piece #:encoding charset) ""))))
   (string-split str #\&)))

(define* (oauth-generate-token #:optional (length 30))
  "Generate a random ASCII token of the given @var{length}."
  (define (random-char _)
    (string-ref ASCII_ALPHABET (random (string-length ASCII_ALPHABET))))
  (list->string (list-tabulate length random-char)))

(define (oauth-query-params params)
  "Returns a URL query (key1=value1&key2=value2...) string for the given
@var{params} association list. Both keys and values will be encoded."
  (string-join
   (map (lambda (p) (string-append (uri-encode (symbol->string (car p)))
                                   "="
                                   (uri-encode (cdr p))))
        params)
   "&"))

;;; (oauth utils) ends here
