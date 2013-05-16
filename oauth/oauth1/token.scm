;;; (oauth oauth1 token) --- Guile OAuth 1.0 implementation.

;; Copyright (C) 2013 Aleix Conchillo Flaque <aconchillo at gmail dot com>
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

(define-module (oauth oauth1 token)
  #:use-module (srfi srfi-9)
  #:export (oauth1-token
            oauth1-token?
            oauth1-token-token
            oauth1-token-secret
            oauth1-token-params-callback-confirmed
            oauth1-token-params->token
            string->oauth1-token-params))

(define-record-type <oauth1-token>
  (oauth1-token token secret)
  oauth1-token?
  (token oauth1-token-token)
  (secret oauth1-token-secret))

(define (oauth1-token-params-callback-confirmed params)
  (assq-ref params 'oauth_callback_confirmed))

(define (oauth1-token-params->token params)
  (oauth1-token (assq-ref params 'oauth_token)
                (assq-ref params 'oauth_token_secret)))

(define (string->oauth1-token-params str)
  (map
   (lambda (param)
     (let ((pair (string-split param #\=)))
       (cons (string->symbol (car pair))
             (car (cdr pair)))))
   (string-split str #\&)))
