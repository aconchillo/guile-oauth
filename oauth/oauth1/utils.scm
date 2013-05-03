;;; (oauth oauth1 utils) --- Guile OAuth 1.0 implementation.

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

(define-module (oauth oauth1 utils)
  #:use-module (srfi srfi-19)
  #:use-module (web uri)
  #:export (oauth1-timestamp
            oauth1-nonce
            oauth1-normalized-params
            oauth1-normalized-header-params
            string->oauth1-token-params))

(define (oauth1-timestamp)
  (date->string (current-date) "~s"))

(define (oauth1-nonce)
  (date->string (current-date) "~s"))

(define (encode-and-sort-params params)
  (stable-sort
   (map
      (lambda (pair) (cons (uri-encode (symbol->string (car pair)))
                           (uri-encode (cdr pair))))
      (assq-remove! params 'oauth_signature))
   (lambda (a b)
     (cond
      ;; If it's less, simply return true
      ((string< (car a) (car b)) #t)
      ;; If it's equal, compare value
      ((string= (car a) (car b)) (string< (cdr a) (cdr b)))
      (else #f)))))

(define (oauth1-normalized-params params)
  (string-join
   (map (lambda (p) (string-append (car p) "=" (cdr p)))
        (encode-and-sort-params params))
   "&"))

(define (oauth1-normalized-header-params params)
  (string-join
   (map (lambda (p) (string-append (symbol->string (car p))
                                   "=\"" (cdr p) "\""))
        params)
   ", "))

(define (string->oauth1-token-params str)
  (map
   (lambda (param)
     (let ((pair (string-split param #\=)))
       (cons (string->symbol (car pair))
             (car (cdr pair)))))
   (string-split str #\&)))
