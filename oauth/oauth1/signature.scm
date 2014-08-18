;;; (oauth oauth1 signature) --- Guile OAuth 1.0 implementation.

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

(define-module (oauth oauth1 signature)
  #:use-module (oauth oauth1 credentials)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (web uri)
  #:use-module (weinholt crypto sha-1)
  #:use-module (weinholt text base64)
  #:export (oauth1-signature
            oauth1-signature?
            oauth1-signature-method
            oauth1-signature-proc
            oauth1-signature-hmac-sha1
            oauth1-signature-plaintext))

(define-record-type <oauth1-signature>
  (oauth1-signature method proc)
  oauth1-signature?
  (method oauth1-signature-method)
  (proc oauth1-signature-proc))

;;
;; HMAC-SHA1
;;

(define (hmac-sha1-key credentials token)
  (string-append (uri-encode (oauth1-credentials-secret credentials))
                 "&"
                 (uri-encode (oauth1-credentials-secret token))))

(define (hmac-sha1-signature base-string credentials token)
  (let ((key (hmac-sha1-key credentials token)))
    (sha-1->bytevector
     (hmac-sha-1 (string->utf8 key) (string->utf8 base-string)))))

(define oauth1-signature-hmac-sha1
  (oauth1-signature
   "HMAC-SHA1"
   (lambda (base-string credentials token)
     (let ((s (hmac-sha1-signature base-string credentials token)))
       (uri-encode (base64-encode s))))))

;;
;; PLAINTEXT
;;

(define (plaintext-signature credentials token)
  (hmac-sha1-key credentials token))

(define oauth1-signature-plaintext
  (oauth1-signature
   "PLAINTEXT"
   (lambda (base-string credentials token)
     (uri-encode (plaintext-signature credentials token)))))
