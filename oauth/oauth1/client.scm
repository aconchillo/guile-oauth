;;; (oauth oauth1 client) --- Guile OAuth 1.0 implementation.

;; Copyright (C) 2013 Aleix Conchillo Flaque <aconchillo at gmail dot com>
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

(define-module (oauth oauth1 client)
  #:use-module (oauth oauth1 request)
  #:use-module (oauth oauth1 token)
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
            oauth1-signature-plaintext
            oauth1-client
            oauth1-client?
            oauth1-client-name
            oauth1-client-key
            oauth1-client-secret
            oauth1-client-signature
            oauth1-client-request-token-url
            oauth1-client-authorize-token-url
            oauth1-client-access-token-url
            oauth1-client-sign-request))

(define-record-type <oauth1-signature>
  (oauth1-signature method proc)
  oauth1-signature?
  (method oauth1-signature-method)
  (proc oauth1-signature-proc))

(define-record-type <oauth1-client>
  (make-oauth1-client name key secret signature request auth access)
  oauth1-client?
  (name oauth1-client-name)
  (key oauth1-client-key)
  (secret oauth1-client-secret)
  (signature oauth1-client-signature)
  (request oauth1-client-request-token-url)
  (auth oauth1-client-authorize-token-url)
  (access oauth1-client-access-token-url))

(define (hmac-sha1-key client token)
  (string-append (uri-encode (oauth1-client-secret client))
                 "&"
                 (uri-encode (oauth1-token-secret token))))

(define (hmac-sha1-signature client request token)
  (let ((base-string (oauth1-request-signature-base-string request))
        (key (hmac-sha1-key client token)))
    (sha-1->bytevector
     (hmac-sha-1 (string->utf8 key) (string->utf8 base-string)))))

(define oauth1-signature-hmac-sha1
  (oauth1-signature
   "HMAC-SHA1"
   (lambda (client request token)
     (uri-encode (base64-encode (hmac-sha1-signature client request token))))))

(define (plaintext-signature client token)
  (hmac-sha1-key client token))

(define oauth1-signature-plaintext
  (oauth1-signature
   "PLAINTEXT"
   (lambda (client request token)
     (uri-encode (plaintext-signature client token)))))

(define* (oauth1-client name key secret request auth access
                        #:key (signature oauth1-signature-hmac-sha1))
  (make-oauth1-client name key secret signature request auth access))

(define (oauth1-client-sign-request client request token)
  (let* ((signature (oauth1-client-signature client))
         (proc (oauth1-signature-proc signature)))
    (oauth1-request-add-param request
                              'oauth_signature_method
                              (oauth1-signature-method signature))
    (oauth1-request-add-param request
                              'oauth_signature
                              (proc client request token))))

(define (oauth1-server-verify-request request token))
