;;; (oauth oauth1 request) --- Guile OAuth 1.0 implementation.

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

(define-module (oauth oauth1 request)
  #:use-module (oauth oauth1 utils)
  #:use-module (gnutls)
  #:use-module (ice-9 format)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-9)
  #:use-module (web client)
  #:use-module (web uri)
  #:export (oauth1-request
            oauth1-request?
            oauth1-request-url
            oauth1-request-method
            oauth1-request-params
            oauth1-request-add-default-params
            oauth1-request-add-param
            oauth1-request-add-params
            oauth1-request-http-headers
            oauth1-request-http-url
            oauth1-request-signature-base-string
            oauth1-http-request))

(define VERSION "1.0")

(define-record-type <oauth1-request>
  (make-oauth1-request url method params)
  oauth1-request?
  (url oauth1-request-url)
  (method oauth1-request-method)
  (params  oauth1-request-params oauth1-request-set-params))

(define* (oauth1-request url #:key
                         (method 'POST)
                         (params '()))
  (make-oauth1-request url method params))

(define (oauth1-request-add-default-params request)
  (oauth1-request-add-params
     request
     `((oauth_version . ,VERSION)
       (oauth_timestamp . ,(oauth1-timestamp))
       (oauth_nonce . ,(oauth1-nonce)))))

(define (oauth1-request-add-param request key value)
  (let ((params (oauth1-request-params request)))
    (oauth1-request-set-params request (assoc-set! params key value))))

(define (oauth1-request-add-params request params)
  (cond
   ((null? params) (oauth1-request-params request))
   (else
    (let ((param (car params)))
      (oauth1-request-add-param request (car param) (cdr param))
      (oauth1-request-add-params request (cdr params))))))

(define (oauth1-request-http-headers request)
  (let* ((params (oauth1-request-params request))
         (norm-params (oauth1-normalized-header-params params)))
    `((Authorization . ,(string-append "OAuth realm=\"\", " norm-params)))))

(define (oauth1-request-http-url request)
  (let* ((url (oauth1-request-url request))
         (params (oauth1-request-params request))
         (norm-params (oauth1-normalized-params params)))
    (string-append url (if (null? params) "" "?") norm-params)))

(define (standard-port? uri)
  (or (not (uri-port uri))
      (and (eq? 'http (uri-scheme uri))
           (= 80 (uri-port uri)))
      (and (eq? 'https (uri-scheme uri))
           (= 443 (uri-port uri)))))

(define (port->string uri)
  (if (standard-port? uri)
      ""
      (string-append ":" (number->string (uri-port uri)))))

(define (signature-request-url uri)
  (string-append (symbol->string (uri-scheme uri))
                 "://"
                 (uri-host uri)
                 (port->string uri)
                 (uri-path uri)))

(define (oauth1-request-signature-base-string request)
  (let ((method (oauth1-request-method request))
        (uri (string->uri (oauth1-request-url request)))
        (params (oauth1-request-params request)))
    (string-join
     (map (lambda (p) (uri-encode p))
          (list (symbol->string method)
                (signature-request-url uri)
                (oauth1-normalized-params params)))
     "&")))

(define (https-call https-proc)
  (lambda* (uri #:key (headers '()))
    (let* ((socket (open-socket-for-uri uri))
           (session (make-session connection-end/client)))
      ;; (set-log-level! 9)
      ;; (set-log-procedure!
      ;;  (lambda (level msg) (format #t "|<~d>| ~a" level msg)))

      ;; Use the file descriptor that underlies SOCKET.
      (set-session-transport-fd! session (fileno socket))

      ;; Use the default settings.
      (set-session-priorities! session "NORMAL")

      ;; Create anonymous credentials.
      (set-session-credentials! session
                                (make-anonymous-client-credentials))
      (set-session-credentials! session
                                (make-certificate-credentials))

      ;; Perform the TLS handshake with the server.
      (handshake session)

      (receive (response body)
          (https-proc uri
                     #:port (session-record-port session)
                     #:keep-alive? #t
                     #:headers headers)
        (bye session close-request/rdwr)
        (values response body)))))

(define https-get (https-call http-get))
(define https-post (https-call http-post))

(define* (oauth1-http-get request)
  (let ((uri (string->uri (oauth1-request-url request)))
        (headers (oauth1-request-http-headers request)))
    (if (eq? (uri-scheme uri) 'http)
      (http-get uri #:headers headers)
      (https-get uri #:headers headers))))

(define* (oauth1-http-post request)
  (let ((uri (string->uri (oauth1-request-url request)))
        (headers (oauth1-request-http-headers request)))
    (if (eq? (uri-scheme uri) 'http)
      (http-post uri #:headers headers)
      (https-post uri #:headers headers))))

(define* (oauth1-http-request request)
  (let ((method (oauth1-request-method request)))
    (case method
      ((GET) (oauth1-http-get request))
      ((POST) (oauth1-http-post request))
      (else throw 'oauth-invalid-method))))
