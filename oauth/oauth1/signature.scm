;;; (oauth oauth1 signature) --- Guile OAuth 1.0 implementation.

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

(define-module (oauth oauth1 signature)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (web uri)
  #:use-module (gcrypt hmac)
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

(define (hmac-sha1-key client-secret token-secret)
  (string-append (uri-encode client-secret)
                 "&"
                 (uri-encode token-secret)))

(define (hmac-sha1-signature base-string client-secret token-secret)
  (let ((key (hmac-sha1-key client-secret token-secret)))
    (sign-data-base64 key base-string #:algorithm 'sha1)))

(define oauth1-signature-hmac-sha1
  (oauth1-signature
   "HMAC-SHA1"
   (lambda (base-string client-secret token-secret)
     ;; We don't (uri-encode) since this will be done once the Authorization
     ;; header is created.
     (hmac-sha1-signature base-string client-secret token-secret))))

;;
;; PLAINTEXT
;;

(define (plaintext-signature credentials token)
  (hmac-sha1-key credentials token))

(define oauth1-signature-plaintext
  (oauth1-signature
   "PLAINTEXT"
   (lambda (base-string credentials token)
     ;; We don't (uri-encode) since this will be done once the Authorization
     ;; header is created.
     (plaintext-signature credentials token))))

;;; (oauth oauth1 signature) ends here
