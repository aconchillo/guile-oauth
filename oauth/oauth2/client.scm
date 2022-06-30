;;; (oauth oauth2 client) --- Guile OAuth 2.0 implementation.

;; Copyright (C) 2021, 2022 Aleix Conchillo Flaque <aconchillo@gmail.com>
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
                                          (scopes #f)
                                          (params '()))
  "Returns a couple of values: the complete authorization URL and the internally
auto-generated state. The complete authorization URL is built from the given
@var{url}, @var{client-id}, @var{redirect-uri}, a list of @var{scopes} and the
state. An additional list of query @var{params} can also be specified. A web
application can then automatically redirect to the returned URL otherwise ask
the user to connect to it with a web browser."
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
      (values (oauth-request-url-with-query request) state))))

(define* (oauth2-client-access-token-from-code url code
                                               #:key
                                               (client-id #f)
                                               (redirect-uri #f)
                                               (auth '())
                                               (extra-headers '())
                                               (http-proc http-request))
  "Obtain an access token from the server @var{url} for the given @var{code} using
an Authorization Code grant. Access tokens are used to connect to protected
resources. The @var{code} is the value obtained after connecting to the
authorization url. Optional @var{client-id}, @var{redirect-uri}, authorization
header @var{auth} (e.g.  @var{oauth-http-basic-auth}) and a list of
@var{extra-headers} can be provided. Finally, an @var{http-proc} procedure can
be specified to provide an HTTP request implementation (defaults
to (http-request))."
  (let ((request (make-oauth-request url 'POST '())))
    (oauth-request-add-params request
                              `((grant_type . "authorization_code")
                                (code . ,code)))
    (when client-id
      (oauth-request-add-param request 'client_id client-id))
    (when redirect-uri
      (oauth-request-add-param request 'redirect_uri redirect-uri))
    (receive (response body)
        (let* ((req-body (oauth-www-form-urlencoded (oauth-request-params request)))
               (content-headers `((content-type . (application/x-www-form-urlencoded))
                                  (content-length . ,(string-utf8-length req-body)))))
          (oauth2-http-request request
                               #:body (string->utf8 req-body)
                               #:headers (append auth content-headers extra-headers)
                               #:http-proc http-proc))
      (oauth2-http-body->token response body))))

(define* (oauth2-client-access-token-from-credentials url client-id client-secret
                                                      #:key
                                                      (auth-type 'header)
                                                      (extra-headers '())
                                                      (http-proc http-request))
  "Obtain an access token from the server @var{url} for the given @var{client-id}
and @var{client-secret} using a Client Credentials grant. The authentication
method to provide the client ID and secret can be specified in
@var{auth-type} ('params or 'header, defaults to 'header). Access tokens are
used to connect to protected resources. Optional @var{extra-headers} can be
provided as an alist. Finally, an @var{http-proc} procedure can be specified to
provide an HTTP request implementation (defaults to (http-request))."
  (let ((request (make-oauth-request url 'POST '())))
    (oauth-request-add-param request 'grant_type "client_credentials")
    (when (auth-type-params? auth-type)
      (oauth-request-add-params request
                                `((client_id . ,client-id)
                                  (client_secret . ,client-secret))))
    (let ((auth (if (auth-type-header? auth-type)
                    (oauth-http-basic-auth client-id client-secret) '())))
      (receive (response body)
          (let* ((req-body (oauth-www-form-urlencoded (oauth-request-params request)))
                 (content-headers `((content-type . (application/x-www-form-urlencoded))
                                    (content-length . ,(string-utf8-length req-body)))))
            (oauth2-http-request request
                                 #:body (string->utf8 req-body)
                                 #:headers (append auth content-headers extra-headers)
                                 #:http-proc http-proc))
        (oauth2-http-body->token response body)))))

(define* (oauth2-client-refresh-token url token
                                      #:key
                                      (client-id #f) (client-secret #f)
                                      (auth-type 'header)
                                      (extra-headers '())
                                      (http-proc http-request))
  "Refresh an access token from the server @var{url} with the previously obtained
@var{token}. If needed, @var{client-id} and @var{client-secret} can be specified
to authenticate this request using the authentication method specified in
@var{auth-type} ('params or 'header, defaults to 'header). Optional
@var{extra-headers} can be provided as an alist. Finally, an @var{http-proc}
procedure can be specified to provide an HTTP request implementation (defaults
to (http-request))."
  (let ((request (make-oauth-request url 'POST '()))
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
          (let* ((req-body (oauth-www-form-urlencoded (oauth-request-params request)))
                 (content-headers `((content-type . (application/x-www-form-urlencoded))
                                    (content-length . ,(string-utf8-length req-body)))))
            (oauth2-http-request request
                                 #:body (string->utf8 req-body)
                                 #:headers (append auth content-headers extra-headers)
                                 #:http-proc http-proc))
        (oauth2-http-body->token response body)))))

(define* (oauth2-client-http-request url token
                                     #:key
                                     (method 'GET)
                                     (body #f)
                                     (params '())
                                     (extra-headers '())
                                     (http-proc http-request))
  "Access a server's protected resource @var{url} with the access @var{token}
previously obtained. Returns two values, the response and the body as a
string. An HTTP method can be selected with @var{method}, and a request
@var{body} can be provided as well, an additional list of query @var{params} and
@var{extra-headers} can also be specified. Finally, an @var{http-proc} procedure
can be specified to provide an HTTP request implementation (defaults
to (http-request))."
  (let ((request (make-oauth-request url method params))
        (auth (oauth2-http-auth-from-token token)))
    (receive (response body)
        (oauth2-http-request-with-query request
                                        #:body body
                                        #:headers (append auth extra-headers)
                                        #:http-proc http-proc)
      (values response (if (string? body) body (utf8->string body))))))

;;; (oauth oauth2 client) ends here
