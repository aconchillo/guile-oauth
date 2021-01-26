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
  #:export (oauth2-client-authorization-url
            oauth2-client-access-token))

(define* (oauth2-client-authorization-url url client-id
                                          #:key (redirect-uri #f) (scopes #f))
  "Returns a couple of values: the complete authorization URL and the internally
auto-generated state. The complete authorization URL is built from the given
@var{url}, @var{client-id}, @var{redirect-uri}, a list of @var{scopes} and the
state. A web application can then automatically redirect to the returned URL
otherwise ask the user to connect to it with a web browser."
  (let ((request (make-oauth-request url 'GET '())))
    (oauth-request-add-params request
                              `((response_type . "code")
                                (client_id . ,client-id)))
    (when redirect-uri
      (oauth-request-add-param request 'redirect_uri redirect-uri))
    (when scopes
      (oauth-request-add-param request 'scope (string-join scopes " ")))
    (let ((state (oauth-generate-token)))
      (oauth-request-add-param request 'state state)
      (values (oauth-request-http-url request) state))))

(define* (oauth2-client-access-token url client-id code
                                     #:key (redirect-uri #f) (method 'POST) (headers '()))
  "Obtain an access token from the server @var{url} for the given
@var{client-id} and @var{code}. The @var{code} is the value obtained after
connecting to the authorization url. Access tokens are used to connect to
protected resources. An HTTP method can be selected with @var{method}."
  (let ((request (make-oauth-request url method '())))
    (oauth-request-add-params request
                              `((grant_type . "authorization_code")
                                (client_id . ,client-id)
                                (code . ,code)))
    (when redirect-uri
      (oauth-request-add-param request 'redirect_uri redirect-uri))
    (receive (response body)
        (oauth2-http-request request #:headers headers)
      (pk (utf8->string body)))))

;;; (oauth oauth2 client) ends here
