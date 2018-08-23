;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2018 Göran Weinholt <goran@weinholt.se>
;;
;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.
;;
;; SPDX-License-Identifier: MIT
#!r6rs

;; Easy definition of special-cased fixnum procedures.

(library (oauth hashing fixnums)
  (export
    define-fixnum-procedures)
  (import
    (rnrs (6)))

  (define-syntax define-fx
    (lambda (x)
      (syntax-case x ()
        ((k prefix bit-width op-name fxname bitwise-name)
         (with-syntax ((name (datum->syntax #'prefix
                                            (string->symbol
                                             (string-append
                                              (symbol->string (syntax->datum #'prefix))
                                              (symbol->string (syntax->datum #'op-name)))))))
           #'(define name
               (if (> (fixnum-width) bit-width)
                   fxname bitwise-name)))))))

  (define-syntax define-fixnum-procedures
    (lambda (x)
      (syntax-case x ()
        ((_ prefix bit-width)
         #'(begin
             ;; FIXME: not complete.
             (define-fx prefix bit-width and fxand bitwise-and)
             (define-fx prefix bit-width xor fxxor bitwise-xor)
             (define-fx prefix bit-width ior fxior bitwise-ior)
             (define-fx prefix bit-width not fxnot bitwise-not)
             (define-fx prefix bit-width + fx+ +)
             (define-fx prefix bit-width - fx- -)
             (define-fx prefix bit-width bit-set? fxbit-set? bitwise-bit-set?)
             (define-fx prefix bit-width arithmetic-shift-right
                        fxarithmetic-shift-right bitwise-arithmetic-shift-right)
             (define-fx prefix bit-width arithmetic-shift-left
                        fxarithmetic-shift-left bitwise-arithmetic-shift-left)
             (define-fx prefix bit-width zero? fxzero? zero?)
             (define-fx prefix bit-width bit-field fxbit-field bitwise-bit-field)))))))
