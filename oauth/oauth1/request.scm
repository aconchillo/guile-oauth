;;; (oauth oauth1 request) --- Guile OAuth 1.0 implementation.

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

(define-module (oauth oauth1 request)
  #:use-module (oauth oauth1 signature)
  #:use-module (oauth oauth1 utils)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-9)
  #:use-module (web uri)
  #:export (oauth1-request
            oauth1-request?
            oauth1-request-url
            oauth1-request-method
            oauth1-request-params
            oauth1-request-param
            oauth1-request-add-default-params
            oauth1-request-add-param
            oauth1-request-add-params
            oauth1-request-http-headers
            oauth1-request-http-url
            oauth1-request-signature-base-string
            oauth1-request-sign
            oauth1-http-request))

(define VERSION "1.0")

(define-record-type <oauth1-request>
  (make-oauth1-request url method params)
  oauth1-request?
  (url oauth1-request-url)
  (method oauth1-request-method)
  (params oauth1-request-params oauth1-request-set-params))

(define* (oauth1-request url #:key
                         (method 'POST)
                         (params '()))
  "Creates an empty request for the given @var{url}, HTTP @var{method}
and additional @var{params}. A request might end up being a request to
get a request token, an access token or a protected resource."
  (make-oauth1-request url method params))

(define (oauth1-request-param request name)
  "Get the parameter with the given @var{name} from the list of
parameters in @var{request}."
  (assq-ref (oauth1-request-params request) name))

(define (oauth1-request-add-default-params request)
  "Add default parameters to the given @var{request}. Default parameters
are: oauth_version, oauth_timestamp and oauth_nonce."
  (oauth1-request-add-params
     request
     `((oauth_version . ,VERSION)
       (oauth_timestamp . ,(oauth1-timestamp))
       (oauth_nonce . ,(oauth1-nonce)))))

(define (oauth1-request-add-param request name value)
  "Add a single parameter with the given @var{name} and @var{value} to
the @var{request}."
  (let ((params (oauth1-request-params request)))
    (oauth1-request-set-params request (assoc-set! params name value))))

(define (oauth1-request-add-params request params)
  "Add a list of parameters to the given @var{request}. @var{params}
must be an association list."
  (cond
   ((null? params) (oauth1-request-params request))
   (else
    (let ((param (car params)))
      (oauth1-request-add-param request (car param) (cdr param))
      (oauth1-request-add-params request (cdr params))))))

(define (oauth1-request-http-headers request)
  "Obtain the HTTP headers for the given @var{request}. This is the
Authorization header with all the list of @var{request} parameters."
  (let* ((params (oauth1-request-params request))
         (norm-params (oauth1-normalized-header-params params)))
    `((Authorization . ,(string-append "OAuth realm=\"\", " norm-params)))))

(define (oauth1-request-http-url request)
  "Obtain the URI for the given @var{request}. The URI will contain all
the @var{request} parameters as URI query arguments."
  (let* ((url (oauth1-request-url request))
         (params (oauth1-request-params request))
         (norm-params (oauth1-normalized-params params)))
    (string-append url (if (null? params) "" "?") norm-params)))

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
  (let ((method (oauth1-request-method request))
        (uri (string->uri (oauth1-request-url request)))
        (params (oauth1-request-params request)))
    (string-join
     (map (lambda (p) (uri-encode p))
          (list (symbol->string method)
                (signature-request-url uri)
                (oauth1-normalized-params params)))
     "&")))

(define* (oauth1-request-sign request credentials token
                              #:key (signature oauth1-signature-hmac-sha1))
  "Adds the signature and signature method parameters to the given
@var{request}. The signature will be computed using the client
@var{credentials} and @var{token} and the given @var{signature} method."
  ;; Before computing signature, we need to add signature method
  ;; parameter first.
  (oauth1-request-add-param request
                            'oauth_signature_method
                            (oauth1-signature-method signature))
  (let ((proc (oauth1-signature-proc signature))
        (base-string (oauth1-request-signature-base-string request)))
    (oauth1-request-add-param request
                              'oauth_signature
                              (proc base-string credentials token))))

;;
;; Request HTTP/HTTPS
;;

(define* (oauth1-http-request request)
  "Perform an HTTP (or HTTPS) @var{request}. The HTTP method and
parameters are already defined in the given @var{request}
object. Currently, only the GET and POST methods are supported."
  (let ((uri (string->uri (oauth1-request-url request)))
        (method (oauth1-request-method request))
        (headers (oauth1-request-http-headers request)))
    (case method
      ((GET) (oauth1-http-get uri headers))
      ((POST) (oauth1-http-post uri headers))
      (else throw 'oauth-invalid-method))))
