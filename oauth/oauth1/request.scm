;;; (oauth oauth1 request) --- Guile OAuth 1.0 implementation.

;; Copyright (C) 2013 Aleix Conchillo Flaque <aconchillo at gmail dot com>
;;
;; This file is part of guile-oauth.
;;
;; guile-oauth is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of
;; the License, or (at your option) any later version.
;;
;; guile-oauth is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;;; Commentary:

;; OAuth 1.0 module for Guile

;;; Code:

(define-module (oauth oauth1 request)
  #:use-module (oauth oauth1 utils)
  #:use-module (srfi srfi-9)
  #:export (oauth1-request
            oauth1-request?
            oauth1-request-url
            oauth1-request-method
            oauth1-request-params
            oauth1-request-signature-method
            oauth1-request-set-default-params
            oauth1-request-add-param
            oauth1-request-add-params
            oauth1-request-header
            ))

(define VERSION "1.0")

(define-record-type <oauth1-request>
  (make-oauth1-request url method params signature-method)
  oauth1-request?
  (url oauth1-request-url)
  (method oauth1-request-method)
  (params  oauth1-request-params oauth1-request-set-params)
  (signature-method oauth1-request-signature-method))

(define* (oauth1-request url #:key
                         (method 'POST)
                         (params '())
                         (signature-method 'HMAC-SHA1))
  (make-oauth1-request url method params signature-method))

(define (oauth1-request-set-default-params request)
  (let ((method (oauth1-request-signature-method request)))
    (oauth1-request-add-params
     request
     `((oauth_version . ,VERSION)
       (oauth_timestamp . ,(oauth1-timestamp))
       (oauth_nonce . ,(oauth1-nonce))
       (oauth_signature_method . ,(symbol->string method))))))

(define (oauth1-request-add-param request key value)
  (oauth1-request-set-params
   request (acons key value (oauth1-request-params request))))

(define (oauth1-request-add-params request params)
  (oauth1-request-set-params
   request (append (oauth1-request-params request) params)))

(define (oauth1-request-header request)
  (let ((params (oauth1-request-params request)))
    `((Authorization . ,(oauth1-normalized-header-params params)))))
