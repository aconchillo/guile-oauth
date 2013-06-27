;;; (oauth oauth1 utils) --- Guile OAuth 1.0 implementation.

;; Copyright (C) 2013 Aleix Conchillo Flaque <aconchillo@gmail.com>
;;
;; This file is part of guile-oauth.
;;
;; guile-oauth is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; guile-oauth is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with guile-oauth; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Commentary:

;; OAuth 1.0 module for Guile

;;; Code:

(define-module (oauth oauth1 utils)
  #:use-module (srfi srfi-19)
  #:use-module (web uri)
  #:export (oauth1-timestamp
            oauth1-nonce
            oauth1-normalized-params
            oauth1-normalized-header-params
            oauth1-parse-www-form-urlencoded))

(define (oauth1-timestamp)
  "Return the number of seconds since the epoch, 00:00:00 UTC on 1
January 1970."
  (date->string (current-date) "~s"))

(define (oauth1-nonce)
  "A random string"
  (date->string (current-date) "~s"))

(define (encode-and-sort-params params)
  (stable-sort
   (map
      (lambda (pair) (cons (uri-encode (symbol->string (car pair)))
                           (uri-encode (cdr pair))))
      (assq-remove! params 'oauth_signature))
   (lambda (a b)
     (cond
      ;; If it's less, simply return true
      ((string< (car a) (car b)) #t)
      ;; If it's equal, compare value
      ((string= (car a) (car b)) (string< (cdr a) (cdr b)))
      (else #f)))))

(define (oauth1-normalized-params params)
  "Returns a normalized single string for the given @var{params}
association list, according to the Parameters Normalization section of
the RFC 5849."
  (string-join
   (map (lambda (p) (string-append (car p) "=" (cdr p)))
        (encode-and-sort-params params))
   "&"))

(define (oauth1-normalized-header-params params)
  "Returns a normalized single string for the given @var{params}
association list, according to the Authorization Header section of the
RFC 5849."
  (string-join
   (map (lambda (p) (string-append (symbol->string (car p))
                                   "=\"" (cdr p) "\""))
        params)
   ", "))

(define* (oauth1-parse-www-form-urlencoded str #:optional (charset "utf-8"))
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
