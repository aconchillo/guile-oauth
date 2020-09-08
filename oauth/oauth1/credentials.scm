;;; (oauth oauth1 credentials) --- Guile OAuth 1.0 implementation.

;; Copyright (C) 2013-2020 Aleix Conchillo Flaque <aconchillo@gmail.com>
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

;; OAuth 1.0 module for Guile

;;; Code:

(define-module (oauth oauth1 credentials)
  #:use-module (oauth oauth1 utils)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:export (make-oauth1-credentials
            oauth1-credentials?
            oauth1-credentials-key
            oauth1-credentials-secret))

(define-record-type <oauth1-credentials>
  (make-oauth1-credentials key secret)
  oauth1-credentials?
  (key oauth1-credentials-key)
  (secret oauth1-credentials-secret))

;;; (oauth oauth1 credentials) ends here
