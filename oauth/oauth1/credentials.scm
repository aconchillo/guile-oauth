;;; (oauth oauth1 credentials) --- Guile OAuth 1.0 implementation.

;; Copyright (C) 2013 Aleix Conchillo Flaque <aconchillo@gmail.com>
;;
;; This file is part of guile-oauth.
;;
;; guile-oauth is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.
;;
;; guile-oauth is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with guile-oauth; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Commentary:

;; OAuth 1.0 module for Guile

;;; Code:

(define-module (oauth oauth1 credentials)
  #:use-module (oauth oauth1 utils)
  #:use-module (srfi srfi-9)
  #:export (oauth1-credentials
            oauth1-credentials?
            oauth1-credentials-id
            oauth1-credentials-secret
            oauth1-token-body->credentials))

(define-record-type <oauth1-credentials>
  (oauth1-credentials id secret)
  oauth1-credentials?
  (id oauth1-credentials-id)
  (secret oauth1-credentials-secret))

(define (oauth1-token-body->credentials body)
  "Create a token credentials object with the token identifier and
secret defined in the given HTTP response @var{body}. The token
credentials are included using the 'application/x-www-form-urlencoded'
content type and obtained with the 'oauth_token' and
'oauth_token_secret' parameters respectively."
  (let ((params (oauth1-parse-www-form-urlencoded body)))
    (oauth1-credentials (assoc-ref params "oauth_token")
                        (assoc-ref params "oauth_token_secret"))))
