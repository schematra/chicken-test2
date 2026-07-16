;;;; test2.scm -- simple friendly test suite (with optional JUnit XML output)
;;
;; Copyright (c) 2007-2008 Alex Shinn. All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt
;;
;; test2 is a drop-in superset of the `test' egg: the whole `test' API is
;; re-exported unchanged, plus `current-test-xml-output' and `test-write-xml'
;; for emitting a JUnit XML report that CI systems (GitHub Actions, GitLab,
;; Jenkins, ...) can parse into pass/fail/error/skip counts.

(module test2
  (test test-error test-assert
   test-group test-group-inc! current-test-group
   test-begin test-end test-syntax-error test-info
   test-vars test-run test-exit
   current-test-verbosity current-test-epsilon current-test-comparator
   current-test-filters current-test-group-filters
   current-test-applier current-test-handler current-test-skipper
   current-test-group-reporter test-failure-count test-total-count
   ;; test2 additions:
   current-test-xml-output test-write-xml test-xml-reset!)
  (import scheme chicken.base)

  (include "test-support.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test interface

(define-syntax test
  (syntax-rules ()
    ((test expect expr)
     (test #f expect expr))
    ((test name expect (expr ...))
     (test-info name expect (expr ...) ()))
    ((test name (expect ...) expr)
     (test-syntax-error
      'test
      "the test expression should come last "
      (test name (expect ...) expr)))
    ((test name expect expr)
     (test-info name expect expr ()))
    ((test a ...)
     (test-syntax-error 'test "2 or 3 arguments required"
                        (test a ...)))
    ))

(define-syntax test-assert
  (syntax-rules ()
    ((_ expr)
    (test-assert #f expr))
    ((_ name expr)
     (test-info name #f expr ((assertion . #t))))
    ((test a ...)
     (test-syntax-error 'test-assert "1 or 2 arguments required"
                        (test a ...)))
    ))

(define-syntax test-error
  (syntax-rules ()
    ((_ expr)
     (test-error #f expr))
    ((_ name expr)
     (test-info name #f expr ((expect-error . #t))))
    ((test a ...)
     (test-syntax-error 'test-error "1 or 2 arguments required"
                        (test a ...)))
    ))

;;    (define-syntax test-error*
;;      (syntax-rules ()
;;        ((_ ?msg (?error-type ...) ?expr)
;;         (let-syntax ((expression:
;;                       (syntax-rules ()
;;                         ((_ ?expr)
;;                          (condition-case (begin ?expr "<no error thrown>")
;;                            ((?error-type ...) '(?error-type ...))
;;                            (exn () (##sys#slot exn 1)))))))
;;           (test ?msg '(?error-type ...) (expression: ?expr))))
;;        ((_ ?msg ?error-type ?expr)
;;         (test-error* ?msg (?error-type) ?expr))
;;        ((_ ?error-type ?expr)
;;         (test-error* (sprintf "~S" '?expr) ?error-type ?expr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; group interface

(define-syntax test-group
  (syntax-rules ()
    ((_ name-expr body ...)
     (let ((name name-expr)
           (old-group (current-test-group)))
       (if (not (string? name))
           (syntax-error 'test-group "a name is required, got " 'name-expr name))
       (test-begin name)
       (condition-case (begin body ...)
                       (e ()
                          (warning "error in group outside of tests")
                          (print-error-message e)
                          (test-record-group-error! e)
                          (test-group-inc! (current-test-group) 'count)
                          (test-group-inc! (current-test-group) 'ERROR)
                          (test-failure-count (+ 1 (test-failure-count)))))
       (test-end name)
       (current-test-group old-group)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities

(define-syntax test-syntax-error
  (syntax-rules ()
    ((_) (syntax-error "invalid use of test-syntax-error"))))

(define-syntax test-info
  (syntax-rules ()
    ((test-info name expect expr info)
     (test-vars () name expect expr ((source . expr) . info)))))

(define-syntax test-vars
  (syntax-rules ()
    ;; Consider trying to determine "interesting" variables as in
    ;; Oleg's ASSERT macro (which unfortunately requires code walking
    ;; to detect lambda's, a point Oleg ignores).  We could hack it by
    ;; not walking into let/lambda's and/or wrapping the value binding
    ;; in error handlers.
    ((_ (vars ...) n expect expr ((key . val) ...))
     (test-run (lambda () expect)
               (lambda () expr)
               (cons (cons 'name n)
                     '((source . expr)
                       ;;(var-names . (vars ...))
                       ;;(var-values . ,(list vars))
                       (key . val) ...)))))))
