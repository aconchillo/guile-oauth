;;; (oauth oauth1 signature) --- Guile OAuth 1.0 implementation.

;; Copyright (C) 2013 Aleix Conchillo Flaque <aconchillo at gmail dot com>
;;
;; This file is part of guile-oauth.
;;
;; guile-oauth is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; guile-oauth is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;;; Commentary:

;; OAuth 1.0 module for Guile

;;; Code:

(define-module (oauth oauth1 signature)
  #:use-module (oauth oauth1 client)
  #:use-module (oauth oauth1 request)
  #:use-module (oauth oauth1 utils)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (web uri)
  #:use-module (weinholt crypto sha-1)
  #:use-module (weinholt text base64)
  #:export (oauth1-signature
            oauth1-signature-method
            oauth1-signature-proc
            oauth1-signature-hmac-sha1
            oauth1a-signature-hmac-sha1
            oauth1-signature-sign-request))

(define-record-type <oauth1-signature>
  (oauth1-signature method proc)
  oauth1-signature?
  (method oauth1-signature-method)
  (proc oauth1-signature-proc))

(define (standard-port? uri)
  (or (not (uri-port uri))
      (and (eq? 'http (uri-scheme uri))
           (= 80 (uri-port uri)))
      (and (eq? 'https (uri-scheme uri))
           (= 443 (uri-port uri)))))

(define (port->string uri)
  (if (standard-port? uri)
      ""
      (string-append ":" (number->string (uri-port uri)))))

(define (signature-request-url uri)
  (string-append (symbol->string (uri-scheme uri))
                 "://"
                 (uri-host uri)
                 (port->string uri)
                 (uri-path uri)))

(define (hmac-sha1-signature-base-string request)
  (let ((method (oauth1-request-method request))
        (uri (string->uri (oauth1-request-url request)))
        (params (oauth1-request-params request)))
    (string-join
     (map (lambda (p) (uri-encode p))
          (list (symbol->string method)
                (signature-request-url uri)
                (oauth1-normalized-params params)))
     "&")))

(define (hmac-sha1-key client token-secret)
  (string-append (uri-encode (oauth1-client-secret client))
                 "&"
                 (uri-encode token-secret)))

(define (hmac-sha1-signature client request token-secret)
  (let* ((base-string (hmac-sha1-signature-base-string request))
         (key (hmac-sha1-key client token-secret)))
    (base64-encode
     (sha-1->bytevector (hmac-sha-1 (string->utf8 key)
                                    (string->utf8 base-string))))))

(define oauth1-signature-hmac-sha1
  (oauth1-signature "HMAC-SHA1" hmac-sha1-signature))

(define oauth1a-signature-hmac-sha1
  (oauth1-signature
   "HMAC-SHA1"
   (lambda (client request token-secret)
     (uri-encode (hmac-sha1-signature client request token-secret)))))

(define (oauth1-signature-sign-request signature client request token-secret)
  (let ((proc (oauth1-signature-proc signature)))
    (oauth1-request-add-param request
                              'oauth_signature_method
                              (oauth1-signature-method signature))
    (oauth1-request-add-param request
                              'oauth_signature
                              (proc client request token-secret))))
