;;; (oauth oauth1 client) --- Guile OAuth 1.0 implementation.

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

(define-module (oauth oauth1 client)
  #:use-module (oauth oauth1 credentials)
  #:use-module (oauth oauth1 signature)
  #:use-module (oauth oauth1 request)
  #:use-module (oauth oauth1 utils)
  #:use-module (ice-9 receive)
  #:use-module (rnrs bytevectors)
  #:use-module (web uri)
  #:export (oauth1-client-request-token
            oauth1-client-authorize-url
            oauth1-client-access-token
            oauth1-client-request))

(define* (oauth1-client-request-token url credentials
                                      #:optional (callback "oob")
                                      #:key
                                      (method 'POST)
                                      (params '())
                                      (signature oauth1-signature-hmac-sha1))
  "Obtain a request token from the server @var{url} for the given client
@var{credentials} (key and secret). Takes one optional argument,
@var{callback}, to set the callback URI that the server will redirect
after authorization is completed, it defaults to 'oob' (no redirection
is performed). An HTTP method can be selected with @var{method} and
additional parameters can be given in @var{params}."
  (let ((token (oauth1-credentials "" ""))
        (request (oauth1-request url #:method method #:params params)))
    (oauth1-request-add-default-params request)
    (oauth1-request-add-param request 'oauth_callback callback)
    (oauth1-request-add-param request
                              'oauth_consumer_key
                              (oauth1-credentials-id credentials))
    (oauth1-request-sign request credentials token #:signature signature)
    (receive (response body)
        (oauth1-http-request request)
      (oauth1-token-body->credentials body))))

(define* (oauth1-client-authorize-url url
                                      #:optional (token #f)
                                      #:key (params '()))
  "Returns a complete authorization URI given the server @var{url} and a
request @var{token}. A web application can automatically redirect to the
returned URI otherwise ask the client to connect to it with a web
browser."
  (let ((request (oauth1-request url #:method 'GET #:params params)))
    (when token
      (oauth1-request-add-param request
                                'oauth_token
                                (oauth1-credentials-id token)))
    (oauth1-request-http-url request)))

(define* (oauth1-client-access-token url credentials token verifier
                                     #:key
                                     (method 'POST)
                                     (signature oauth1-signature-hmac-sha1))
  "Obtain an access token from the server @var{url} for the given client
@var{credentials} (key and secret), request @var{token} and
@var{verifier}. Access tokens are used to connect to protected
resources. An HTTP method can be selected with @var{method}."
  (let ((request (oauth1-request url #:method method)))
    (oauth1-request-add-default-params request)
    (oauth1-request-add-param request
                              'oauth_token
                              (oauth1-credentials-id token))
    (oauth1-request-add-param request 'oauth_verifier verifier)
    (oauth1-request-add-param request
                              'oauth_consumer_key
                              (oauth1-credentials-id credentials))
    (oauth1-request-sign request credentials token #:signature signature)
    (receive (response body)
        (oauth1-http-request request)
      (oauth1-token-body->credentials body))))

(define* (oauth1-client-request url credentials token
                                #:key
                                (method 'GET)
                                (params '())
                                (signature oauth1-signature-hmac-sha1))
  "Access a server's protected resource @var{url} with the given client
@var{credentials} (key and secret) and an access @var{token}. Returns a
string. An HTTP method can be selected with @var{method} and additional
parameters can be given in @var{params}."
  (let ((request (oauth1-request url #:method method #:params params)))
    (oauth1-request-add-default-params request)
    (oauth1-request-add-param request
                              'oauth_token
                              (oauth1-credentials-id token))
    (oauth1-request-add-param request
                              'oauth_consumer_key
                              (oauth1-credentials-id credentials))
    (oauth1-request-sign request credentials token #:signature signature)
    (receive (response body)
        (oauth1-http-request request)
      (utf8->string body))))
