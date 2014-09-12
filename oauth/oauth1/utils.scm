;;; (oauth oauth1 utils) --- Guile OAuth 1.0 implementation.

;; Copyright (C) 2013, 2014 Aleix Conchillo Flaque <aconchillo@gmail.com>
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
  #:use-module (gnutls)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-19)
  #:use-module (web client)
  #:use-module (web uri)
  #:export (oauth1-timestamp
            oauth1-nonce
            oauth1-normalized-params
            oauth1-normalized-header-params
            oauth1-parse-www-form-urlencoded
            oauth1-http-get
            oauth1-http-post))

(define (oauth1-timestamp)
  "Return the number of seconds since the epoch, 00:00:00 UTC on 1
January 1970."
  (date->string (current-date) "~s"))

(define (oauth1-nonce)
  "A random string"
  (date->string (current-date) "~s"))

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

;;
;; HTTP/HTTPS methods
;;

(define (https-call https-proc)
  (lambda* (uri #:key (headers '()))
    (let* ((socket (open-socket-for-uri uri))
           (session (make-session connection-end/client)))
      ;; (set-log-level! 9)
      ;; (set-log-procedure!
      ;;  (lambda (level msg) (format #t "|<~d>| ~a" level msg)))

      ;; Use the file descriptor that underlies SOCKET.
      (set-session-transport-fd! session (fileno socket))

      ;; Use the default settings.
      (set-session-priorities! session "NORMAL")

      ;; Create anonymous credentials.
      (set-session-credentials! session
                                (make-anonymous-client-credentials))
      (set-session-credentials! session
                                (make-certificate-credentials))

      ;; Perform the TLS handshake with the server.
      (handshake session)

      (receive (response body)
          (https-proc uri
                     #:port (session-record-port session)
                     #:keep-alive? #t
                     #:headers headers)
        (bye session close-request/rdwr)
        (values response body)))))

(define https-get (https-call http-get))
(define https-post (https-call http-post))

(define* (oauth1-http-get uri headers)
  "Perform an HTTP GET request with the given HTTP @var{headers} to the
@var{uri}."
  (if (eq? (uri-scheme uri) 'http)
      (http-get uri #:headers headers)
      (https-get uri #:headers headers)))

(define* (oauth1-http-post uri headers)
  "Perform an HTTP POST request with the given HTTP @var{headers} to the
@var{uri}."
  (if (eq? (uri-scheme uri) 'http)
      (http-post uri #:headers headers)
      (https-post uri #:headers headers)))
