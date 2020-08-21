;;; (oauth oauth1 utils) --- Guile OAuth 1.0 implementation.

;; Copyright (C) 2013-2020 Aleix Conchillo Flaque <aconchillo@gmail.com>
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

;; OAuth 1.0 module for Guile

;;; Code:

(define-module (oauth oauth1 utils)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-19)
  #:use-module (web uri)
  #:export (oauth1-timestamp
            oauth1-nonce
            oauth1-param?
            oauth1-query-params
            oauth1-normalized-params
            oauth1-authorization-header-params
            oauth1-parse-www-form-urlencoded))

(define (oauth1-timestamp)
  "Return the number of seconds since the epoch, 00:00:00 UTC on 1
January 1970."
  (date->string (current-date) "~s"))

(define (oauth1-nonce)
  "A random string"
  (date->string (current-date) "~s"))

(define (oauth1-param? param)
  "Returns true if this parameter pair is used by OAuth, and false
otherwise.  Useful when filtering parameter lists."
  (match param
    ((k . v)
     (string-prefix-ci? "oauth_" (if (symbol? k) (symbol->string k) k)))))

(define (encode-and-sort-params params)
  (stable-sort
   (let lp ((params params))
     (match params
       (() '())
       ((('oauth_signature . _) . params) (lp params))
       (((key . value) . params)
        (acons (uri-encode (symbol->string key)) (uri-encode value) (lp params)))))
   (match-lambda*
    (((k1 . v1) (k2 . v2))
     (cond
      ;; If it's less, simply return true
      ((string< k1 k2) #t)
      ;; If it's equal, compare value
      ((string= k1 k2) (string< v1 v2))
      (else #f))))))

(define (oauth1-query-params params)
  "Returns a URL query string for the given @var{params} association list."
  (string-join
   (map (lambda (p) (string-append (uri-encode (symbol->string (car p)))
                                   "="
                                   (uri-encode (cdr p))))
        params)
   "&"))

(define (oauth1-normalized-params params)
  "Returns a normalized single string for the given @var{params}
association list, according to the Normalize Request Parameters section
of the RFC 5849."
  (string-join
   (map (lambda (p) (string-append (car p) "=" (cdr p)))
        (encode-and-sort-params params))
   "&"))

(define (oauth1-authorization-header-params params)
  "Returns a normalized single string for the given @var{params}
association list, according to the Authorization Header section of the
RFC 5849."
  (string-join
   (map (lambda (p) (string-append (uri-encode (symbol->string (car p)))
                                   "=\""
                                   (uri-encode (cdr p)) "\""))
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

;;; (oauth oauth1 utils) ends here
