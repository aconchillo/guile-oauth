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

(define* (https-get uri #:key (headers '()))
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
        (http-get uri
                   #:port (session-record-port session)
                   #:keep-alive? #t
                   #:headers headers)
      (bye session close-request/rdwr)
      (values response body))))

(define* (https-post uri #:key (headers '()))
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
        (http-post uri
                   #:port (session-record-port session)
                   #:keep-alive? #t
                   #:headers headers)
      (bye session close-request/rdwr)
      (values response body))))

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
