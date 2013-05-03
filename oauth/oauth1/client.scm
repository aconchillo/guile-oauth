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
  #:use-module (srfi srfi-9)
  #:export (oauth1-client
            oauth1-client?
            oauth1-client-key
            oauth1-client-secret
            oauth1-client-request-token-url
            oauth1-client-authorize-token-url
            oauth1-client-access-token-url))

(define-record-type <oauth1-client>
  (oauth1-client name key secret request auth access)
  oauth1-client?
  (name oauth1-client-name)
  (key oauth1-client-key)
  (secret oauth1-client-secret)
  (request oauth1-client-request-token-url)
  (auth oauth1-client-authorize-token-url)
  (access oauth1-client-access-token-url))
