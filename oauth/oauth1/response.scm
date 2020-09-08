;;; (oauth oauth1 response) --- Guile OAuth 1.0 implementation.

;; Copyright (C) 2020 Aleix Conchillo Flaque <aconchillo@gmail.com>
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

(define-module (oauth oauth1 response)
  #:use-module (oauth oauth1 utils)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:export (make-oauth1-response
            oauth1-response?
            oauth1-response-token
            oauth1-response-token-secret
            oauth1-response-params
            oauth1-http-body->response))

(define-record-type <oauth1-response>
  (make-oauth1-response token secret params)
  oauth1-response?
  (token oauth1-response-token)
  (secret oauth1-response-token-secret)
  (params oauth1-response-params))

(define (oauth1-http-body->response body)
  "Create a service response record from the given HTTP response
@var{body}. The service response includes the token, the token secret and
might include additional parameters defined by the service provider. The token
and the token secret are included as 'application/x-www-form-urlencoded'
content and obtained with the 'oauth_token' and 'oauth_token_secret'
parameters respectively. Note that this function will also check if the
'oauth_callback_confirmed' parameter is present as required in OAuth1.0a, if
not it will display a warning."
  (let ((str-body (if (string? body) body (utf8->string body))))
    (catch 'uri-error
      (lambda ()
        (let* ((params (oauth1-parse-www-form-urlencoded str-body))
               (token (assoc-ref params "oauth_token"))
               (secret (assoc-ref params "oauth_token_secret"))
               (callback-confirmed (assoc-ref params "oauth_callback_confirmed")))
          (unless (and token secret)
            (throw 'oauth-invalid-response str-body))
          (unless (and callback-confirmed (string=? callback-confirmed "true"))
            (warn "Missing oauth_callback_confirmed=true in response as required in OAuth1.0a."))
          ;; Only leave optional parameters in params alist.
          (set! params (assoc-remove! params "oauth_token"))
          (set! params (assoc-remove! params "oauth_token_secret"))
          (set! params (assoc-remove! params "oauth_callback_confirmed"))
          (make-oauth1-response token secret params)))
      (lambda _ (throw 'oauth-invalid-response str-body)))))

;;; (oauth oauth1 response) ends here
