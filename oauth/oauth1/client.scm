;;; (oauth oauth1 client) --- Guile OAuth 1.0 implementation.

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

(define-module (oauth oauth1 client)
  #:use-module (oauth oauth1 credentials)
  #:use-module (oauth oauth1 signature)
  #:use-module (oauth oauth1 request)
  #:use-module (oauth oauth1 response)
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
@var{callback}, to set the callback URL that the server will redirect
after authorization is completed, it defaults to 'oob' (no redirection
is performed). An HTTP method can be selected with @var{method} and
additional parameters can be given in @var{params}."
  (let ((response (make-oauth1-response "" "" '()))
        (request (oauth1-request url #:method method #:params params)))
    (oauth1-request-add-default-params request)
    (oauth1-request-add-param request 'oauth_callback callback)
    (oauth1-request-add-param request
                              'oauth_consumer_key
                              (oauth1-credentials-key credentials))
    (oauth1-request-sign request credentials response #:signature signature)
    (receive (response body)
        (oauth1-http-request request)
      (oauth1-http-body->response body))))

(define* (oauth1-client-authorize-url url
                                      #:optional (response #f)
                                      #:key (params '()))
  "Returns a complete authorization URL given the server @var{url} and a
request token @var{response}. A web application can automatically redirect to
the returned URL otherwise ask the client to connect to it with a web
browser."
  (let ((request (oauth1-request url #:method 'GET #:params params)))
    (when response
      (oauth1-request-add-param request
                                'oauth_token
                                (oauth1-response-token response)))
    (oauth1-request-http-url request #:param-filter (lambda (_) #t))))

(define* (oauth1-client-access-token url credentials response verifier
                                     #:key
                                     (method 'POST)
                                     (signature oauth1-signature-hmac-sha1))
  "Obtain an access token from the server @var{url} for the given client
@var{credentials} (key and secret), request token @var{response} and
@var{verifier}. Access tokens are used to connect to protected resources. An
HTTP method can be selected with @var{method}."
  (let ((request (oauth1-request url #:method method)))
    (oauth1-request-add-default-params request)
    (oauth1-request-add-param request
                              'oauth_token
                              (oauth1-response-token response))
    (oauth1-request-add-param request 'oauth_verifier verifier)
    (oauth1-request-add-param request
                              'oauth_consumer_key
                              (oauth1-credentials-key credentials))
    (oauth1-request-sign request credentials response #:signature signature)
    (receive (response body)
        (oauth1-http-request request)
      (oauth1-http-body->response body))))

(define* (oauth1-client-request url credentials response
                                #:key
                                (method 'GET)
                                (params '())
                                (signature oauth1-signature-hmac-sha1))
  "Access a server's protected resource @var{url} with the given client
@var{credentials} (key and secret) and an access token @var{response}. Returns
a string. An HTTP method can be selected with @var{method} and additional
parameters can be given in @var{params}."
  (let ((request (oauth1-request url #:method method #:params params)))
    (oauth1-request-add-default-params request)
    (oauth1-request-add-param request
                              'oauth_token
                              (oauth1-response-token response))
    (oauth1-request-add-param request
                              'oauth_consumer_key
                              (oauth1-credentials-key credentials))
    (oauth1-request-sign request credentials response #:signature signature)
    (receive (response body)
        (oauth1-http-request request)
      (if (string? body) body (utf8->string body)))))

;;; (oauth oauth1 client) ends here
