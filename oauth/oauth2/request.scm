;;; (oauth oauth2 request) --- Guile OAuth 1.0 implementation.

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

(define-module (oauth oauth2 request)
  #:use-module (oauth request)
  #:use-module (web client)
  #:use-module (web uri)
  #:export (oauth2-http-request))

;;
;; Request HTTP/HTTPS
;;

(define* (oauth2-http-request request #:key (headers '()))
  "Perform an HTTP (or HTTPS) @var{request}. The HTTP method and parameters are
already defined in the given @var{request} object."
  (let* ((request-url (oauth-request-http-url request)))
    (http-request (string->uri request-url)
                  #:method (oauth-request-method request)
                  #:headers headers)))

;;; (oauth oauth2 request) ends here
