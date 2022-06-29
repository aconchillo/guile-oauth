;;; (oauth request) --- Guile OAuth implementation.

;; Copyright (C) 2013-2022 Aleix Conchillo Flaque <aconchillo@gmail.com>
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

;; OAuth module for Guile

;;; Code:

(define-module (oauth request)
  #:use-module (oauth utils)
  #:use-module (srfi srfi-9)
  #:export (make-oauth-request
            oauth-request?
            oauth-request-url
            oauth-request-method
            oauth-request-params
            oauth-request-param
            oauth-request-add-param
            oauth-request-add-params
            oauth-request-url-with-query))

(define-record-type <oauth-request>
  (make-oauth-request url method params)
  oauth-request?
  (url oauth-request-url)
  (method oauth-request-method)
  (params oauth-request-params oauth-request-set-params))

(define (oauth-request-param request name)
  "Get the parameter with the given @var{name} from the list of
parameters in @var{request}."
  (assq-ref (oauth-request-params request) name))

(define (oauth-request-add-param request name value)
  "Add a single parameter with the given @var{name} and @var{value} to
the @var{request}."
  (let ((params (oauth-request-params request)))
    (oauth-request-set-params request (assoc-set! params name value))))

(define (oauth-request-add-params request params)
  "Add a list of parameters to the given @var{request}. @var{params}
must be an association list."
  (cond
   ((null? params) (oauth-request-params request))
   (else
    (let ((param (car params)))
      (oauth-request-add-param request (car param) (cdr param))
      (oauth-request-add-params request (cdr params))))))

(define* (oauth-request-url-with-query request #:key (param-filter (lambda (_) #t)))
  "Obtain the URL for the given @var{request}. The URI will contain all
the @var{request} parameters that satisfy @var{param-filter} as URL
query arguments."
  (let* ((url (oauth-request-url request))
         (params (filter param-filter (oauth-request-params request)))
         (query-params (oauth-www-form-urlencoded params)))
    (string-append url (if (null? params) "" "?") query-params)))

;;; (oauth request) ends here
