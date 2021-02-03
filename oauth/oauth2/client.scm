;;; (oauth oauth2 client) --- Guile OAuth 2.0 implementation.

;; Copyright (C) 2021 Aleix Conchillo Flaque <aconchillo@gmail.com>
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

;; OAuth 2.0 module for Guile

;;; Code:

(define-module (oauth oauth2 client)
  #:use-module (oauth oauth2 request)
  #:use-module (oauth request)
  #:use-module (oauth utils)
  #:use-module (ice-9 receive)
  #:use-module (rnrs bytevectors)
  #:use-module (web client)
  #:use-module (json)
  #:export (oauth2-client-authorization-url
            oauth2-client-access-token-from-code
            oauth2-client-access-token-from-credentials
            oauth2-client-http-request))

(define* (oauth2-client-authorization-url url client-id
                                          #:key
                                          (response-type 'code) (redirect-uri #f)
                                          (scopes #f) (params '()))
  "Returns a couple of values: the complete authorization URL and the internally
auto-generated state. The complete authorization URL is built from the given
@var{url}, @var{client-id}, @var{redirect-uri}, a list of @var{scopes} and the
state. A web application can then automatically redirect to the returned URL
otherwise ask the user to connect to it with a web browser."
  (let ((request (make-oauth-request url 'GET params)))
    (oauth-request-add-params request
                              `((response_type . ,(symbol->string response-type))
                                (client_id . ,client-id)))
    (when redirect-uri
      (oauth-request-add-param request 'redirect_uri redirect-uri))
    (when scopes
      (oauth-request-add-param request 'scope (string-join scopes " ")))
    (let ((state (oauth-generate-token)))
      (oauth-request-add-param request 'state state)
      (values (oauth-request-http-url request) state))))

(define* (oauth2-client-access-token-from-code url code
                                               #:key
                                               (client-id #f) (redirect-uri #f)
                                               (method 'POST) (params '())
                                               (auth '()) (extra-headers '()))
  "Obtain an access token from the server @var{url} for the given @var{code}
using an Authorization Code grant. Access tokens are used to connect to
protected resources. The @var{code} is the value obtained after connecting to
the authorization url. Optional @var{client-id}, @var{redirect-uri},
authorization header @var{auth} (e.g.  @var{oauth-http-basic-auth}), additional
parameters @var{params} as an alist and a list of @var{extra-headers} can be
provided. An HTTP method can be selected with @var{method}."
  (let ((request (make-oauth-request url method params)))
    (oauth-request-add-params request
                              `((grant_type . "authorization_code")
                                (code . ,code)))
    (when client-id
      (oauth-request-add-param request 'client_id client-id))
    (when redirect-uri
      (oauth-request-add-param request 'redirect_uri redirect-uri))
    (receive (response body)
        (oauth2-http-request request
                             #:headers (append `((authorization . ,auth))
                                               extra-headers))
      (json-string->scm (utf8->string body)))))

(define* (oauth2-client-access-token-from-credentials url client-id client-secret
                                                      #:key
                                                      (auth-type 'header)
                                                      (method 'POST) (params '())
                                                      (extra-headers '()))
  "Obtain an access token from the server @var{url} for the given
@var{client-id} and @var{client-secret} using a Client Credentials grant. Access
tokens are used to connect to protected resources. Optional... An HTTP method
can be selected with @var{method}."
  (let ((request (make-oauth-request url method params))
        (auth (oauth-http-basic-auth client-id client-secret)))
    (oauth-request-add-param request 'grant_type "client_credentials")
    (receive (response body)
        (oauth2-http-request request
                             #:headers (append `((authorization . ,auth))
                                               extra-headers))
      (json-string->scm (utf8->string body)))))

(define* (oauth2-client-http-request url token
                                     #:key
                                     (method 'GET) (params '()) (extra-headers '()))
  "Access a server's protected resource @var{url} with the given access
@var{token} response previsouly obtained. Returns a string. An HTTP method can
be selected with @var{method}, additional parameters can be given in
@var{params} as an alist and a list of @var{extra-headers} can also be
specified."
  (let ((request (make-oauth-request url method params))
        (auth (oauth2-http-auth-from-token token)))
    (receive (response body)
        (oauth2-http-request request
                             #:headers (append `((authorization . ,auth))
                                               extra-headers))
      (if (string? body) body (utf8->string body)))))

;;; (oauth oauth2 client) ends here
