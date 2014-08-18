#!/usr/bin/guile -s
!#

;;; Guile OAuth client example.

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

;; OAuth Twitter client example.

;;; Code:

(use-modules (json)
             (oauth oauth1)
             (ice-9 receive)
             (sxml simple)
             (rnrs bytevectors)
             (web server)
             (web request)
             (web response)
             (web uri))

(define title "guile-oauth twitter example")

(define xhtml-doctype
  (string-append
   "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" "
   "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"))

(define *twitter-request-url* "https://api.twitter.com/oauth/request_token")
(define *twitter-auth-url* "https://api.twitter.com/oauth/authorize")
(define *twitter-access-url* "https://api.twitter.com/oauth/access_token")
(define *twitter-credentials* (oauth1-credentials "" ""))

(define *request-token* (oauth1-credentials "" ""))
(define *access-token* (oauth1-credentials "" ""))

(define (request-query-ref request param)
  (let ((query (uri-query (request-uri request))))
    (assoc-ref (oauth1-parse-www-form-urlencoded query) param)))

(define (twitter-main-editing-form)
  `(div
    (form (@ (method "POST")
             (action "http://localhost:8080/twitter/auth"))
          (p (label (@ (for "key")) "Consumer key: ")
             (input (@ (name "key") (type "text") (size "50")
                       (value ""))))
          (p (label (@ (for "secret")) "Consumer secret: ")
             (input (@ (name "secret") (type "text") (size "50")
                       (value ""))))
          (p (label (@ (for "request")) "Request token URL: ")
             (input (@ (name "request") (type "text") (size "50")
                       (value ,*twitter-request-url*))))
          (p (label (@ (for "auth")) "Authorize token URL: ")
             (input (@ (name "auth") (type "text") (size "50")
                       (value ,*twitter-auth-url*))))
          (p (label (@ (for "access")) "Access token URL: ")
             (input (@ (name "access") (type "text") (size "50")
                       (value ,*twitter-access-url*))))
          (input (@ (type "submit") (name "status") (value "Home timeline"))))))

(define (twitter-main-form request body)
  `(html
    (head (title ,title))
    (body ,(twitter-main-editing-form))))

(define (twitter-main-form-handler request body)
  (values (build-response
           #:headers '((content-type . (text/html))))
          (lambda (port)
            (display xhtml-doctype port)
            (sxml->xml (twitter-main-form request body) port))))

(define (twitter-authenticate)
  (let ((callback "http://localhost:8080/twitter/access"))
    (set! *request-token*
          (oauth1-client-request-token *twitter-request-url*
                                       *twitter-credentials*
                                       callback))
    (oauth1-client-authorize-url *twitter-auth-url* *request-token*)))

(define (twitter-auth request body)
  (let ((params (oauth1-parse-www-form-urlencoded (utf8->string body))))
    (set! *twitter-credentials*
          (oauth1-credentials (assoc-ref params "key")
                              (assoc-ref params "secret")))
    (set! *twitter-request-url* (assoc-ref params "request"))
    (set! *twitter-auth-url* (assoc-ref params "auth"))
    (set! *twitter-access-url* (assoc-ref params "access"))
    (twitter-authenticate)))

(define (twitter-auth-handler request body)
  (values (build-response
           #:code 302
           #:headers `((content-type . (text/html))
                       (location . ,(string->uri (twitter-auth request body)))))
          (lambda (port) #nil)))

(define (twitter-access-handler request body)
  (let ((location "http://localhost:8080/twitter/home_timeline")
        (verifier (request-query-ref request "oauth_verifier")))
    (set! *access-token*
          (oauth1-client-access-token *twitter-access-url*
                                      *twitter-credentials*
                                      *request-token*
                                      verifier))
    (values (build-response
           #:code 302
           #:headers `((content-type . (text/html))
                       (location . ,(string->uri location))))
          (lambda (port) #nil))))

(define (twitter-timeline-html json)
  (let ((tweets (json-string->scm json)))
    `(html
      (head (title ,title))
      (body
       "(" (a (@ (href "http://localhost:8080/twitter/home_timeline"))
              "Home timeline")
       ") "
       "(" (a (@ (href "http://localhost:8080/twitter/user_timeline"))
              "User timeline")
       ") "
       ,(map
         (lambda (tweet)
           (let ((user (hash-ref tweet "user")))
             `(p (img (@ (src ,(hash-ref user "profile_image_url"))))
                 ,(hash-ref tweet "text"))))
         tweets)))))

(define (twitter-timeline url)
  (twitter-timeline-html
   (oauth1-client-request url *twitter-credentials* *access-token*)))

(define (twitter-tweets-handler url)
  (lambda (request body)
    (values
     (build-response #:headers '((content-type . (text/html))))
     (lambda (port)
       (display xhtml-doctype port)
       (sxml->xml (twitter-timeline url) port)))))

(define twitter-user-timeline-handler
  (twitter-tweets-handler
   "https://api.twitter.com/1.1/statuses/user_timeline.json"))

(define twitter-home-timeline-handler
  (twitter-tweets-handler
   "https://api.twitter.com/1.1/statuses/home_timeline.json"))

;; Build a resource not found (404) response
(define (not-found request)
  (values (build-response #:code 404)
          (string-append "Resource not found: "
                         (uri->string (request-uri request)))))

(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))

;; This is the server main handler. It will check if the given request
;; is valid, and if so it will call the right handler.
(define (main-handler request body)
  (cond
   ;; /twitter
   ((equal? (request-path-components request) '("twitter"))
    (twitter-main-form-handler request body))
   ;; /twitter/auth
   ((equal? (request-path-components request) '("twitter" "auth"))
    (twitter-auth-handler request body))
   ;; /twitter/access
   ((equal? (request-path-components request) '("twitter" "access"))
    (twitter-access-handler request body))
   ;; /twitter/home_timeline
   ((equal? (request-path-components request) '("twitter" "home_timeline"))
    (twitter-home-timeline-handler request body))
   ;; /twitter/user_timeline
   ((equal? (request-path-components request) '("twitter" "user_timeline"))
    (twitter-user-timeline-handler request body))
   ;; Resource not found (404)
   (else (not-found request))))

(display "\nNow go to http://localhost:8080/twitter\n")

;; We start the server. (main-handler) will be called every time a
;; request is received.
(run-server main-handler)

;;; code ends here
