;; Copyright (C) 2015, 2016  Erik Edrosa <erik.edrosa@gmail.com>
;;
;; This file is part of guile-commonmark
;;
;; guile-commonmark is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; guile-commonmark is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with guile-commonmark.  If not, see <http://www.gnu.org/licenses/>.

(define-module (commonmark utils)
  #:use-module (ice-9 rdelim)
  #:export (filter-map&co
            read-line-without-nul))


(define (filter-map&co f l k) ;; f是过滤函数,k是个连续函数，用来承接所有结果
  "like filter-map but uses a continuation to collect an extra list of values"
  (if (null? l) ;; 如果list是空的
      (k '() '())
      (f (car l) (lambda (v d)
                   (filter-map&co f (cdr l)
                                  (lambda (v2 d2)
                                    (k (if v (cons v v2) v2) (append d d2))))))))

;; (define f (v k)
;;    (k v (list v))
;;(define k (v d)
;;  (list v d))
;;(filter-map&co f '(a b) k) =>
;;(f a (lambda (v d) '(a '(a))
;;          (filter-map&co f '(b) (lambda (v2 d2) (k (cons 'a v2) (append '(a) d2))))))
;;此时k是初始的函数k
;;(f a (lambda (v d) '(a '(a))
;;       (f b (lambda (v d) '(b '(b))
;;          (filter-map&co f '() (lambda (v2 d2) (k (cons 'b v2) (append '(b) d2))))))))
;;此时k是(lambda (v2 d2) (k (cons 'a v2) (append '(a) d2)))
;;(f a (lambda (v d)
;;       (f b (lambda (v d)
;;           (k '() '())))))
;;此时的k是 k1 = (lambda (v2 d2) (k (cons 'a v2) (append '(a) d2)))
;; (lambda (v2 d2) (k1 (cons 'b v2) (append '(b) d2)))


;; Line is one of:
;;  - String
;;  - eof-object

;; Port -> Line
(define (read-line-without-nul port)
  "Return a line of text from PORT replacing '\0' with '\uFFFD' or
returns eof-object."
  (define (replace-nul x)
    (if (char=? x #\nul) #\xFFFD x))
  (let ((line (read-line port)))
    (cond ((eof-object? line) line)
          ((string-any #\nul line) (string-map replace-nul line))
          (else line))))
