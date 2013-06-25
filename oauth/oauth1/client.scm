;;; (oauth oauth1 client) --- Guile OAuth 1.0 implementation.

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

(define-module (oauth oauth1 client)
  #:use-module (oauth oauth1 signature)
  #:use-module (oauth oauth1 request)
  #:use-module (oauth oauth1 token)
  #:use-module (oauth oauth1 utils)
  #:use-module (ice-9 receive)
  #:use-module (web uri)
  #:export (oauth1-client-request-token
            oauth1-client-authorize-token
            oauth1-client-access-token))

(define* (oauth1-client-request-token url key secret
                                      #:optional (callback "oob")
                                      #:key
                                      (method 'POST)
                                      (params '())
                                      (signature oauth1-signature-hmac-sha1))
  (let ((token (oauth1-token "" ""))
        (request (oauth1-request url #:method method #:params params)))
    (oauth1-request-add-default-params request)
    (oauth1-request-add-param request 'oauth_callback callback)
    (oauth1-request-add-param request 'oauth_consumer_key key)
    (oauth1-sign-request request secret token #:signature signature)
    (receive (response body)
        (oauth1-http-request request)
      (string->oauth1-token-params body))))

(define* (oauth1-client-authorize-token url
                                        #:optional (token #f)
                                        #:key (params '()))
  (let ((request (oauth1-request url #:method 'GET #:params params)))
    (when token
      (oauth1-request-add-param request
                                'oauth_token
                                (oauth1-token-token token)))
    (oauth1-request-http-url request)))

(define* (oauth1-client-access-token url key secret token verifier
                                     #:key
                                     (method 'POST)
                                     (signature oauth1-signature-hmac-sha1))
  (let ((request (oauth1-request url #:method method)))
    (oauth1-request-add-default-params request)
    (oauth1-request-add-param request
                              'oauth_token
                              (oauth1-token-token token))
    (oauth1-request-add-param request 'oauth_verifier verifier)
    (oauth1-request-add-param request 'oauth_consumer_key key)
    (oauth1-sign-request request secret token #:signature signature)
    (receive (response body)
        (oauth1-http-request request)
      (string->oauth1-token-params body))))
