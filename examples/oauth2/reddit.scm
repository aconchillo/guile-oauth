#!/usr/bin/env guile -s
!#

;;; Guile OAuth client example.

;; Copyright (C) 2013-2021 Aleix Conchillo Flaque <aconchillo@gmail.com>
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

;; OAuth Reddit client example.

;;; Code:

(use-modules (json)
             (oauth oauth2)
             (oauth utils)
             (ice-9 match)
             (ice-9 receive)
             (sxml simple)
             (rnrs bytevectors)
             (web client)
             (web server)
             (web request)
             (web response)
             (web uri))

(define title "guile-oauth reddit example")

(define xhtml-doctype
  (string-append
   "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" "
   "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n"))

(define *reddit-user-agent* "guile:guile-oauth:1.0.0 (by /u/aconchillo)")
(define *reddit-auth-url* "https://www.reddit.com/api/v1/authorize")
(define *reddit-access-url* "https://www.reddit.com/api/v1/access_token")
(define *reddit-subreddits-mine-url* "https://oauth.reddit.com/subreddits/mine/subscriber")

(define *client-id* "")
(define *client-secret* "")
(define *access-token* '())
(define *redirect-uri* "http://127.0.0.1:8080/reddit/access")

(define-json-type <post>
  (title)
  (author)
  (url))

(define-json-type <subreddit>
  (display-name "display_name")
  (url))

(define (reddit-main-editing-form)
  `(div (@ (class "ui centered page grid"))
        (div (@ (class "ten wide column"))
             (form (@ (class "ui form")
                         (method "POST")
                         (action "http://127.0.0.1:8080/reddit/auth"))
                      (div (@ (class "field"))
                           (label (@ (for "client_id")) "Client ID: ")
                           (input (@ (name "client_id") (type "text") (size "50")
                                     (value ""))))
                      (div (@ (class "field"))
                           (label (@ (for "secret")) "Client secret: ")
                           (input (@ (name "secret") (type "text") (size "50")
                                     (value ""))))
                      (button (@ (class "ui button") (type "submit")) "Go Reddit!")))))

(define (reddit-main-form request body)
  `(html
    (head (title ,title)
          (script (@ (src "https://code.jquery.com/jquery-3.1.1.min.js")
                       (integrity "sha256-hVVnYaiADRTO2PzUGmuLJr8BLUSjGIZsDYGmIJLv2b8=")
                       (crossorigin "anonymous")) "")
          (script (@ (src "https://cdnjs.cloudflare.com/ajax/libs/fomantic-ui/2.8.6/semantic.min.js")) "")
          (link (@ (rel "stylesheet")
                   (href "https://cdnjs.cloudflare.com/ajax/libs/fomantic-ui/2.8.6/semantic.min.css"))))
    (body ,(reddit-main-editing-form))))

(define (reddit-main-form-handler request body)
  (values (build-response
           #:headers '((content-type . (text/html))))
          (lambda (port)
            (display xhtml-doctype port)
            (sxml->xml (reddit-main-form request body) port))))

(define (reddit-auth request body)
  (let ((params (oauth-parse-www-form-urlencoded (utf8->string body))))
    (set! *client-id* (assoc-ref params "client_id"))
    (set! *client-secret* (assoc-ref params "secret"))
    (oauth2-client-authorization-url *reddit-auth-url* *client-id*
                                   #:redirect-uri *redirect-uri*
                                   #:scopes '("identity" "mysubreddits" "read"))))

(define (reddit-auth-handler request body)
  (values (build-response
           #:code 302
           #:headers `((content-type . (text/html))
                       (location . ,(string->uri (reddit-auth request body)))))
          (lambda (port) #nil)))

(define (reddit-access-handler request body)
  (let* ((location "http://127.0.0.1:8080/reddit/home")
         (query (uri-query (request-uri request)))
         (params (oauth-parse-www-form-urlencoded query))
         (code (assoc-ref params "code")))
    (set! *access-token*
      (oauth2-client-access-token-from-code *reddit-access-url* code
                                            #:client-id *client-id*
                                            #:redirect-uri *redirect-uri*
                                            #:auth (oauth-http-basic-auth *client-id* *client-secret*)))
    (values (build-response
             #:code 302
             #:headers `((content-type . (text/html))
                         (location . ,(string->uri location))))
            (lambda (port) #nil))))

(define (reddit-listing->list json scm->json-type)
  (let* ((listing (json-string->scm json))
         (data (assoc-ref listing "data"))
         (children (assoc-ref data "children")))
    (map (lambda (c) (scm->json-type (assoc-ref c "data"))) (vector->list children))))

(define (reddit-get-my-subreddits-list)
  (receive (response body)
      (oauth2-client-http-request *reddit-subreddits-mine-url* *access-token*
                                  #:extra-headers `((user-agent . ,*reddit-user-agent*)))
    (reddit-listing->list body scm->subreddit)))

(define (reddit-feed-from-json json)
  (reddit-listing->list json scm->post))

(define* (render-home subreddits #:optional (feed '()))
  `(html
    (head (title ,title)
          (script (@ (src "https://code.jquery.com/jquery-3.1.1.min.js")
                     (integrity "sha256-hVVnYaiADRTO2PzUGmuLJr8BLUSjGIZsDYGmIJLv2b8=")
                     (crossorigin "anonymous")) "")
          (script (@ (src "https://cdnjs.cloudflare.com/ajax/libs/fomantic-ui/2.8.6/semantic.min.js")) "")
          (link (@ (rel "stylesheet")
                   (href "https://cdnjs.cloudflare.com/ajax/libs/fomantic-ui/2.8.6/semantic.min.css"))))
    (body
     (div (@ (style "margin-top: 20px") (class "ui page compact grid"))
          (h1 (@ (class "ui header"))
              ,(render-subreddits-list subreddits))
          ,(render-subreddit-feed feed)))))

(define (render-subreddits-list subreddits)
  (map
   (lambda (subreddit)
     `("(" (a (@ (href ,(string-append "http://127.0.0.1:8080/reddit" (subreddit-url subreddit))))
              ,(subreddit-display-name subreddit))
       ") "))
   subreddits))

(define (render-subreddit-feed feed)
  `(div (@ (class "ui relaxed divided list"))
        ,(map render-subreddit-post feed)))

(define (render-subreddit-post post)
  `(div (@ (class "item"))
        (div (@ (class "content"))
             (a (@ (class "header") (href ,(post-url post))) ,(post-title post))
             (div (@ (class "description")) "Posted by /u/" ,(post-author post)))))

(define (reddit-subreddit-html subreddit)
  (let ((url (string-append "https://www.reddit.com/r/" subreddit "/.json")))
    (receive (response body)
        (http-request url)
      (let ((feed (reddit-feed-from-json (utf8->string body))))
        (render-home (reddit-get-my-subreddits-list) feed)))))

(define (reddit-home-handler request body)
  (values
   (build-response #:headers '((content-type . (text/html))))
   (lambda (port)
     (display xhtml-doctype port)
     (sxml->xml (render-home (reddit-get-my-subreddits-list)) port))))

(define (reddit-subreddit-handler request body subreddit)
  (values
   (build-response #:headers '((content-type . (text/html))))
   (lambda (port)
     (display xhtml-doctype port)
     (sxml->xml (reddit-subreddit-html subreddit) port))))

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
  (match (request-path-components request)
   ;; /reddit
   (("reddit") (reddit-main-form-handler request body))
   ;; /reddit/auth
   (("reddit" "auth") (reddit-auth-handler request body))
   ;; /reddit/access
   (("reddit" "access") (reddit-access-handler request body))
   ;; /reddit/home
   (("reddit" "home") (reddit-home-handler request body))
   ;; /reddit/r/<subreddit>
   (("reddit" "r" subreddit) (reddit-subreddit-handler request body subreddit))
   ;; Resource not found (404)
   (_ (not-found request))))

(display "\nNow go to http://127.0.0.1:8080/reddit\n")

;; We start the server. (main-handler) will be called every time a
;; request is received.
(run-server main-handler)

;;; code ends here
