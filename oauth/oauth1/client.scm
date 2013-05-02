;;; (oauth oauth1 client) --- Guile OAuth 1.0 implementation.

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

(define-module (oauth oauth1 client)
  #:use-module (oauth oauth1 request)
  #:use-module (oauth oauth1 utils)
  #:use-module (ice-9 format)
  #:use-module (ice-9 receive)
  #:use-module (gnutls)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (web client)
  #:use-module (web uri)
  #:use-module (weinholt crypto sha-1)
  #:use-module (weinholt text base64)
  #:export (oauth1-client
            oauth1-client?
            oauth1-client-consumer-key
            oauth1-client-consumer-secret
            oauth1-client-request-token))

(define VERSION "1.0")

(define-record-type <oauth1-client>
  (make-oauth1-client name
                      consumer-key
                      consumer-secret)
  oauth1-client?
  (name oauth1-client-name)
  (consumer-key oauth1-client-consumer-key)
  (consumer-secret oauth1-client-consumer-secret))

(define (oauth1-client name consumer-key consumer-secret)
  (make-oauth1-client name consumer-key consumer-secret))

(define* (http-oauth-post uri #:key (headers '()))
  (let* ((socket (open-socket-for-uri uri))
         (session (make-session connection-end/client)))

  (set-log-level! 0)

  (set-log-procedure!
   (lambda (level msg) (format #t "|<~d>| ~a" level msg)))

  ;; Use the file 8descriptor that underlies SOCKET.
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

(define (hmac-sha1-signature client request)
  (let* ((method (oauth1-request-method request))
         (uri (string->uri (oauth1-request-url request)))
         (params (oauth1-request-params request))
         (base-string (oauth1-signature-base-string method uri params))
         (key (oauth1-client-consumer-secret client)))
    (pk base-string)
    (base64-encode
     (sha-1->bytevector (hmac-sha-1 (string->utf8 key)
                                    (string->utf8 base-string))))))

(define* (oauth1-client-request-token client request
                                      #:optional (callback "oob"))
  (let* ((url (oauth1-request-url request))
         (uri (string->uri url)))
    (oauth1-request-set-default-params request)
    ;(oauth1-request-add-param request 'oauth_callback callback)
    (oauth1-request-add-param request
                              'oauth_consumer_key
                              (oauth1-client-consumer-key client))
    (oauth1-request-add-param request
                              'oauth_signature
                              (hmac-sha1-signature client request))
    (pk (oauth1-request-params request))))

;; (receive (response body)
;;         (http-oauth-post uri #:headers (oauth1-request-header request))
;;       (pk body))
