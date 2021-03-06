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
  #:use-module (oauth oauth2 response)
  #:use-module (oauth request)
  #:use-module (oauth utils)
  #:use-module (ice-9 receive)
  #:use-module (rnrs bytevectors)
  #:use-module (web client)
  #:use-module (json)
  #:use-module (gcrypt random)
  #:export (oauth2-client-authorization-url
            oauth2-client-access-token-from-code
            oauth2-client-access-token-from-credentials
            oauth2-client-refresh-token
            oauth2-client-http-request))

(define (auth-type-header? type)
  (eq? type 'header))

(define (auth-type-params? type)
  (eq? type 'params))

(define* (oauth2-client-authorization-url url client-id
                                          #:key
                                          (redirect-uri #f)
                                          (scopes #f) (params '()))
  "Returns a couple of values: the complete authorization URL and the internally
auto-generated state. The complete authorization URL is built from the given
@var{url}, @var{client-id}, @var{redirect-uri}, a list of @var{scopes} and the
state. A web application can then automatically redirect to the returned URL
otherwise ask the user to connect to it with a web browser."
  (let ((request (make-oauth-request url 'GET params)))
    (oauth-request-add-params request
                              `((response_type . "code")
                                (client_id . ,client-id)))
    (when redirect-uri
      (oauth-request-add-param request 'redirect_uri redirect-uri))
    (when scopes
      (oauth-request-add-param request 'scope (string-join scopes " ")))
    (let ((state (random-token)))
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
provided. An HTTP method can also be selected with @var{method}."
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
                             #:headers (append auth extra-headers))
      (oauth2-http-body->token response body))))

(define* (oauth2-client-access-token-from-credentials url client-id client-secret
                                                      #:key
                                                      (auth-type 'header)
                                                      (method 'POST) (params '())
                                                      (extra-headers '()))
  "Obtain an access token from the server @var{url} for the given
@var{client-id} and @var{client-secret} using a Client Credentials grant. the
authentication method to provide the client ID and secret can be specified in
@var{auth-type} ('params or 'header, defaults to 'header). Access tokens are
used to connect to protected resources. Optional parameters @var{params} can be
provided as an alist, as well as a list of @var{extra-headers}. An HTTP method
can be selected with @var{method}."
  (let ((request (make-oauth-request url method params)))
    (oauth-request-add-param request 'grant_type "client_credentials")
    (when (auth-type-params? auth-type)
      (oauth-request-add-params request
                                `((client_id . ,client-id)
                                  (client_secret . ,client-secret))))
    (let ((auth (if (auth-type-header? auth-type)
                    (oauth-http-basic-auth client-id client-secret) '())))
      (receive (response body)
        (oauth2-http-request request
                             #:headers (append auth extra-headers))
      (oauth2-http-body->token response body)))))

(define* (oauth2-client-refresh-token url token
                                      #:key
                                      (client-id #f) (client-secret #f)
                                      (auth-type 'header)
                                      (method 'POST) (params '())
                                      (extra-headers '()))
  "Refresh an access token from the server @var{url} with the previously
obtained @var{token}. If needed, @var{client-id} and @var{client-secret} can be
specified to authenticate this request using the authentication method specified
in @var{auth-type} ('params or 'header, defaults to 'header). Optional
parameters @var{params} can be provided as an alist, as well as a list of
@var{extra-headers}. An HTTP method can be selected with @var{method}."
  (let ((request (make-oauth-request url method params))
        (refresh-token (assoc-ref token "refresh_token")))
    (unless refresh-token
      (throw 'oauth-invalid-token token))
    (oauth-request-add-params request
                              `((grant_type . "refresh_token")
                                (refresh_token . ,refresh-token)))
    (when (auth-type-params? auth-type)
      (oauth-request-add-params request
                                `((client_id . ,client-id)
                                  (client_secret . ,client-secret))))
    (let ((auth (if (auth-type-header? auth-type)
                    (oauth-http-basic-auth client-id client-secret) '())))
      (receive (response body)
        (oauth2-http-request request
                             #:headers (append auth extra-headers))
      (oauth2-http-body->token response body)))))

(define* (oauth2-client-http-request url token
                                     #:key
                                     (method 'GET) (params '())
                                     (extra-headers '()))
  "Access a server's protected resource @var{url} with the access @var{token}
previously obtained. Returns two values, the response and the body as a
string. An HTTP method can be selected with @var{method}, additional parameters
can be given via @var{params} as an alist and a list of @var{extra-headers} can
also be specified."
  (let ((request (make-oauth-request url method params))
        (auth (oauth2-http-auth-from-token token)))
    (receive (response body)
        (oauth2-http-request request
                             #:headers (append auth extra-headers))
      (values response (if (string? body) body (utf8->string body))))))

;;; (oauth oauth2 client) ends here
