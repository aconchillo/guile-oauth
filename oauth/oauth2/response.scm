;;; (oauth oauth2 response) --- Guile OAuth 2.0 implementation.

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

(define-module (oauth oauth2 response)
  #:use-module (json)
  #:export (oauth2-http-body->token))

(define (oauth2-http-body->token response body)
  "Verify the HTTP response @var{body} contains a valid access token and throw
an exception if not."
  (let ((str-body (if (string? body) body (utf8->string body))))
    (catch 'json-invalid
      (lambda ()
        (let* ((token-response (json-string->scm str-body))
               (token-type (assoc-ref token-response "token_type"))
               (access-token (assoc-ref token-response "access_token")))
          (unless (and token-type access-token)
            (throw 'oauth-invalid-response response body))
          token-response))
      (lambda _ (throw 'oauth-invalid-response response body)))))

;;; (oauth oauth2 response) ends here
