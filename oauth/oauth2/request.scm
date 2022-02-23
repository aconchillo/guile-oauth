;;; (oauth oauth2 request) --- Guile OAuth 2.0 implementation.

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

(define-module (oauth oauth2 request)
  #:use-module (oauth request)
  #:use-module (web client)
  #:use-module (web http)
  #:use-module (web uri)
  #:export (oauth2-http-auth-from-token
            oauth2-http-request))

(define (oauth2-http-auth-from-token token)
  "Create HTTP authorization credentials depending on the specified @var{token}
type (e.g. bearer or mac)."
  (let ((token-type (assoc-ref token "token_type"))
        (access-token (assoc-ref token "access_token")))
    (unless (and token-type access-token)
      (throw 'oauth-invalid-token token))
    (cond
     ((string=? token-type "bearer")
      (let ((bearer (string-append "bearer " access-token)))
        `((authorization . ,(parse-header 'authorization bearer)))))
     (else (throw 'oauth-invalid-token token)))))

;;
;; Request HTTP/HTTPS
;;

(define* (oauth2-http-request request #:key (body #f) (headers '()))
  "Perform an HTTP (or HTTPS) @var{request}. The HTTP method and parameters are
already defined in the given @var{request} object."
  (let* ((request-url (oauth-request-http-url request)))
    (http-request (string->uri request-url)
                  #:method (oauth-request-method request)
                  #:body body
                  #:headers headers)))

;;; (oauth oauth2 request) ends here
