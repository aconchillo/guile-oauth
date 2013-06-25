;;; (oauth oauth1 signature) --- Guile OAuth 1.0 implementation.

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

(define-module (oauth oauth1 signature)
  #:use-module (oauth oauth1 request)
  #:use-module (oauth oauth1 token)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (web uri)
  #:use-module (weinholt crypto sha-1)
  #:use-module (weinholt text base64)
  #:export (oauth1-credentials
            oauth1-credentials?
            oauth1-credentials-key
            oauth1-credentials-secret
            oauth1-credentials-signature
            oauth1-signature
            oauth1-signature?
            oauth1-signature-method
            oauth1-signature-proc
            oauth1-signature-hmac-sha1
            oauth1-signature-plaintext
            oauth1-sign-request))

(define-record-type <oauth1-credentials>
  (make-oauth1-credentials key secret signature)
  oauth1-credentials?
  (key oauth1-credentials-key)
  (secret oauth1-credentials-secret)
  (signature oauth1-credentials-signature))

(define* (oauth1-credentials key secret
                             #:key (signature oauth1-signature-hmac-sha1))
  (make-oauth1-credentials key secret signature))

(define-record-type <oauth1-signature>
  (oauth1-signature method proc)
  oauth1-signature?
  (method oauth1-signature-method)
  (proc oauth1-signature-proc))

(define (hmac-sha1-key secret token)
  (string-append (uri-encode secret)
                 "&"
                 (uri-encode (oauth1-token-secret token))))

(define (hmac-sha1-signature request secret token)
  (let ((base-string (oauth1-request-signature-base-string request))
        (key (hmac-sha1-key secret token)))
    (sha-1->bytevector
     (hmac-sha-1 (string->utf8 key) (string->utf8 base-string)))))

(define oauth1-signature-hmac-sha1
  (oauth1-signature
   "HMAC-SHA1"
   (lambda (request secret token)
     (uri-encode (base64-encode (hmac-sha1-signature request secret token))))))

(define (plaintext-signature secret token)
  (hmac-sha1-key secret token))

(define oauth1-signature-plaintext
  (oauth1-signature
   "PLAINTEXT"
   (lambda (request secret token)
     (uri-encode (plaintext-signature secret token)))))

(define* (oauth1-sign-request request credentials token)
  (let* ((signature (oauth1-credentials-signature credentials))
         (secret (oauth1-credentials-secret credentials))
         (proc (oauth1-signature-proc signature)))
    (oauth1-request-add-param request
                              'oauth_signature_method
                              (oauth1-signature-method signature))
    (oauth1-request-add-param request
                              'oauth_signature
                              (proc request secret token))))
