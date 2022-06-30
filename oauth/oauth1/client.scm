;;; (oauth oauth1 client) --- Guile OAuth 1.0 implementation.

;; Copyright (C) 2013-2022 Aleix Conchillo Flaque <aconchillo@gmail.com>
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
  #:use-module (oauth request)
  #:use-module (ice-9 receive)
  #:use-module (rnrs bytevectors)
  #:use-module (web client)
  #:use-module (web uri)
  #:export (oauth1-client-request-token
            oauth1-client-authorization-url
            oauth1-client-access-token
            oauth1-client-http-request))

(define* (oauth1-client-request-token url credentials
                                      #:optional (callback "oob")
                                      #:key
                                      (method 'POST)
                                      (params '())
                                      (params-location 'header)
                                      (signature oauth1-signature-hmac-sha1)
                                      (http-proc http-request))
  "Obtain a request token from the server @var{url} for the given client
@var{credentials}. Takes one optional argument, @var{callback}, to set the
callback URL that the server will redirect after authorization is completed, it
defaults to 'oob' (no redirection is performed). An HTTP method can be selected
with @var{method} and additional parameters can be given in @var{params}. The
@{params-location} can be the Authorization 'header (default), the 'query or the
'body. Finally, an @var{http-proc} procedure can be specified to provide an HTTP
request implementation (defaults to (http-request))."
  (let ((response (make-oauth1-response "" "" '()))
        (request (make-oauth-request url method params)))
    (oauth1-request-add-default-params request)
    (oauth-request-add-param request 'oauth_callback callback)
    (oauth-request-add-param request
                             'oauth_consumer_key
                             (oauth1-credentials-key credentials))
    (oauth1-request-sign request credentials response #:signature signature)
    (receive (response body)
        (oauth1-http-request request
                             #:params-location params-location
                             #:http-proc http-proc)
      (oauth1-http-body->response response body))))

(define* (oauth1-client-authorization-url url
                                          #:optional (response #f)
                                          #:key (params '()))
  "Returns a complete authorization URL given the server @var{url} and a
request token @var{response}. A web application can automatically redirect to
the returned URL otherwise ask the user to connect to it with a web
browser."
  (let ((request (make-oauth-request url 'GET params)))
    (when response
      (oauth-request-add-param request
                               'oauth_token
                               (oauth1-response-token response)))
    (oauth-request-url-with-query request)))

(define* (oauth1-client-access-token url credentials response verifier
                                     #:key
                                     (method 'POST)
                                     (extra-headers '())
                                     (params-location 'header)
                                     (signature oauth1-signature-hmac-sha1)
                                     (http-proc http-request))
  "Obtain an access token from the server @var{url} for the given client
@var{credentials} (key and secret), request token @var{response} and
@var{verifier}. Access tokens are used to connect to protected resources. An
HTTP method can be selected with @var{method} and additional @var{extra-headers}
can be provided. The @{params-location} can be the Authorization
'header (default), the 'query or the 'body. Finally, an @var{http-proc}
procedure can be specified to provide an HTTP request implementation (defaults
to (http-request))."
  (let ((request (make-oauth-request url method '())))
    (oauth1-request-add-default-params request)
    (oauth-request-add-param request
                             'oauth_token
                             (oauth1-response-token response))
    (oauth-request-add-param request 'oauth_verifier verifier)
    (oauth-request-add-param request
                             'oauth_consumer_key
                             (oauth1-credentials-key credentials))
    (oauth1-request-sign request credentials response #:signature signature)
    (receive (response body)
        (oauth1-http-request request
                             #:params-location params-location
                             #:extra-headers extra-headers
                             #:http-proc http-proc)
      (oauth1-http-body->response response body))))

(define* (oauth1-client-http-request url credentials response
                                     #:key
                                     (method 'GET)
                                     (body #f)
                                     (params '())
                                     (params-location 'header)
                                     (extra-headers '())
                                     (signature oauth1-signature-hmac-sha1)
                                     (http-proc http-request))
  "Access a server's protected resource @var{url} with the given client
@var{credentials} and an access token @var{response}. Returns two values, the
response and the body as a string. An HTTP method can be selected with
@var{method}, and a request @var{body} can be provided as well, additional
parameters can be given in @var{params} as an alist and a list of
@var{extra-headers} can be provided as well. The @{params-location} can be the
Authorization 'header (default), the 'query or the 'body. Also, an optional
@var{signature} algorithm can be specified. Finally, an @var{http-proc}
procedure can be specified to provide an HTTP request implementation (defaults
to (http-request))."
  (let ((request (make-oauth-request url method params)))
    (oauth1-request-add-default-params request)
    (oauth-request-add-param request
                             'oauth_token
                             (oauth1-response-token response))
    (oauth-request-add-param request
                             'oauth_consumer_key
                             (oauth1-credentials-key credentials))
    (oauth1-request-sign request credentials response #:signature signature)
    (receive (response resp-body)
        (oauth1-http-request request
                             #:params-location params-location
                             #:extra-headers extra-headers
                             #:body body
                             #:http-proc http-proc)
      (values response (if (string? resp-body) resp-body (utf8->string resp-body))))))

;;; (oauth oauth1 client) ends here
