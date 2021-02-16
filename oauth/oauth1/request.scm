;;; (oauth oauth1 request) --- Guile OAuth 1.0 implementation.

;; Copyright (C) 2013-2021 Aleix Conchillo Flaque <aconchillo@gmail.com>
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

(define-module (oauth oauth1 request)
  #:use-module (oauth oauth1 credentials)
  #:use-module (oauth oauth1 response)
  #:use-module (oauth oauth1 signature)
  #:use-module (oauth oauth1 utils)
  #:use-module (oauth request)
  #:use-module (web client)
  #:use-module (web uri)
  #:export (oauth1-request-add-default-params
            oauth1-request-http-headers
            oauth1-request-signature-base-string
            oauth1-request-sign
            oauth1-http-request))

(define VERSION "1.0")

(define (oauth1-request-add-default-params request)
  "Add default parameters to the given @var{request}. Default parameters
are: oauth_version, oauth_timestamp and oauth_nonce."
  (oauth-request-add-params
     request
     `((oauth_version . ,VERSION)
       (oauth_timestamp . ,(oauth1-timestamp))
       (oauth_nonce . ,(oauth1-nonce)))))

(define (oauth1-request-http-headers request)
  "Obtain the HTTP headers for the given @var{request}. This is the
Authorization header with all the list of the OAuth @var{request}
parameters."
  (let* ((params (filter oauth1-param? (oauth-request-params request)))
         (header-params (oauth1-authorization-header-params params)))
    `((Authorization . ,(string-append "OAuth " header-params)))))

;;
;; Signing request
;;

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

(define (oauth1-request-signature-base-string request)
  "Get the Signature Base String for the given @var{request}. The
Signature Base String is a consistent reproducible concatenation of the
@var{request} elements into a single string."
  (let ((method (oauth-request-method request))
        (uri (string->uri (oauth-request-url request)))
        (params (oauth-request-params request)))
    (string-join
     (map (lambda (p) (uri-encode p))
          (list (symbol->string method)
                (signature-request-url uri)
                (oauth1-normalized-params params)))
     "&")))

(define* (oauth1-request-sign request credentials response
                              #:key (signature oauth1-signature-hmac-sha1))
  "Adds the signature and signature method parameters to the given
@var{request}. The signature will be computed using the client
@var{credentials} and @var{response} token and the given @var{signature}
method."
  ;; Before computing signature, we need to add signature method
  ;; parameter first.
  (oauth-request-add-param request
                           'oauth_signature_method
                           (oauth1-signature-method signature))
  (let ((proc (oauth1-signature-proc signature))
        (base-string (oauth1-request-signature-base-string request))
        (client-secret (oauth1-credentials-secret credentials))
        (token-secret (oauth1-response-token-secret response)))
    (oauth-request-add-param request
                             'oauth_signature
                             (proc base-string client-secret token-secret))))

;;
;; Request HTTP/HTTPS
;;

(define* (oauth1-http-request request #:key (extra-headers '()))
  "Perform an HTTP (or HTTPS) @var{request}. The HTTP method and parameters are
already defined in the given @var{request} object."
  (let* ((request-url (oauth-request-http-url request #:param-filter (compose not oauth1-param?))))
    (http-request (string->uri request-url)
                  #:method (oauth-request-method request)
                  #:headers (append (oauth1-request-http-headers request)
                                    extra-headers))))

;;; (oauth oauth1 request) ends here
