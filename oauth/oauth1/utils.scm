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
  #:use-module (gnutls)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 receive)
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
            oauth1-http-post
            oauth1-uri-encode))

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
      (lambda (pair) (cons (oauth1-uri-encode (symbol->string (car pair)))
                           (oauth1-uri-encode (cdr pair))))
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

;; We need uri-encode to return uppercase. oauth1-uri-encode is just a
;; copy of uri-encode from Guile. This will be removed as soon as a new
;; version > 2.0.9 is released.

(define (call-with-output-string* proc)
  (let ((port (open-output-string)))
    (proc port)
    (let ((str (get-output-string port)))
      (close-port port)
      str)))

(define (call-with-output-bytevector* proc)
  (call-with-values
      (lambda ()
        (open-bytevector-output-port))
    (lambda (port get-bytevector)
      (proc port)
      (let ((bv (get-bytevector)))
        (close-port port)
        bv))))

(define (call-with-encoded-output-string encoding proc)
  (if (string-ci=? encoding "utf-8")
      (string->utf8 (call-with-output-string* proc))
      (call-with-output-bytevector*
       (lambda (port)
         (set-port-encoding! port encoding)
         (proc port)))))

(define (encode-string str encoding)
  (if (string-ci=? encoding "utf-8")
      (string->utf8 str)
      (call-with-encoded-output-string encoding
                                       (lambda (port)
                                         (display str port)))))

(define ascii-alnum-chars
  (string->char-set
   "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))

;; RFC 3986, #2.3
(define unreserved-chars
  (char-set-union ascii-alnum-chars
                  (string->char-set "-._~")))

;; Return a new string made from uri-encoding STR, unconditionally
;; transforming any characters not in UNESCAPED-CHARS.
;;
(define* (oauth1-uri-encode str #:key (encoding "utf-8")
                            (unescaped-chars unreserved-chars))
  "Percent-encode any character not in the character set,
UNESCAPED-CHARS.

The default character set includes alphanumerics from ASCII, as well as
the special characters ‘-’, ‘.’, ‘_’, and ‘~’.  Any other character will
be percent-encoded, by writing out the character to a bytevector within
the given ENCODING, then encoding each byte as ‘%HH’, where HH is the
uppercase hexadecimal representation of the byte."
  (define (needs-escaped? ch)
    (not (char-set-contains? unescaped-chars ch)))
  (if (string-index str needs-escaped?)
      (call-with-output-string*
       (lambda (port)
         (string-for-each
          (lambda (ch)
            (if (char-set-contains? unescaped-chars ch)
                (display ch port)
                (let* ((bv (encode-string (string ch) encoding))
                       (len (bytevector-length bv)))
                  (let lp ((i 0))
                    (if (< i len)
                        (let ((byte (bytevector-u8-ref bv i)))
                          (display #\% port)
                          (when (< byte 16)
                            (display #\0 port))
                          (display (string-upcase (number->string byte 16))
                                   port)
                          (lp (1+ i))))))))
          str)))
      str))
