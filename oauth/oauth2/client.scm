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
  #:use-module (oauth request)
  #:export (oauth2-client-authorize-url))

(define* (oauth2-client-authorize-url url client-id
                                      #:optional
                                      (redirect-uri #f) (scope #f))
  "Returns a complete authorization URL given the server @var{url} and the
client id @var{client-id}. A web application can automatically redirect to the
returned URL otherwise ask the user to connect to it with a web browser."
  (let ((request (make-oauth-request url 'GET '())))
    (oauth-request-add-params request
                              '((response_type . "code")
                                (client_id . client-id)))
    (when redirect-uri
      (oauth-request-add-param request 'redirect_uri redirect-uri))
    (when scope
      (oauth-request-add-param request 'scope scope))
    (oauth-request-http-url request)))

;;; (oauth oauth2 client) ends here
