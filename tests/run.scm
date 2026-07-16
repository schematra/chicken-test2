;;;; run.scm -- self-test for the test2 egg
;;
;; Runs an *isolated* sample suite (with a known mix of pass/fail/error/skip
;; results) into a temporary JUnit file, then asserts on the generated XML.
;; The isolation (fresh counters + a nulled current-test-group) keeps the
;; sample's failures from affecting this script's own pass/fail tally, so
;; `chicken-install -test' stays green.

(import test2
        (chicken file)
        (chicken io)
        (chicken port)
        (chicken string))

;; Run a small sample suite in isolation and return the JUnit XML it produced.
(define (generate-sample-xml)
  (let ((tmp (create-temporary-file ".xml")))
    (parameterize ((current-test-group #f)      ; sample groups are top-level
                   (test-failure-count 0)       ; don't taint our own tally
                   (test-total-count 0)
                   (current-test-verbosity #f)
                   (current-test-xml-output tmp)); enable recording
      (with-output-to-string          ; swallow the sample's console output
        (lambda ()
          (test-begin "sample")
          (test "pass1" 4 (+ 2 2))                 ; PASS
          (test "fail1" 5 (+ 2 2))                 ; FAIL
          (test-error "err-ok" (car '()))          ; PASS (error expected)
          (test "err1" 1 (car '()))                ; ERROR
          (test "a<b>&\"c\"" 3 3)                   ; PASS (name needs escaping)
          (test-group "inner"
            (test "pass2" 'a (car '(a))))          ; PASS (nested suite)
          (parameterize ((current-test-filters (list (lambda (info) #f))))
            (test "skip1" 1 1))                    ; SKIP
          (test-end "sample")
          (test-write-xml tmp))))
    (let ((xml (with-input-from-file tmp (lambda () (read-string)))))
      (delete-file* tmp)
      xml)))

(define xml (generate-sample-xml))

(define (has? sub) (and (substring-index sub xml) #t))

(test-begin "test2 junit xml")

;; --- structure ---
(test-assert "xml declaration"     (has? "<?xml version=\"1.0\""))
(test-assert "testsuites root"     (has? "<testsuites "))
(test-assert "testsuites closed"   (has? "</testsuites>"))
(test-assert "named suite"         (has? "<testsuite name=\"sample\""))
(test-assert "nested suite name"   (has? "name=\"sample/inner\""))

;; --- aggregate counts on the <testsuites> element ---
;; cases: pass1 fail1 err-ok(PASS) err1 escaped(PASS) pass2 skip1  => 7 total
(test-assert "total tests"         (has? "tests=\"7\""))
(test-assert "one failure"         (has? "failures=\"1\""))
(test-assert "one error"           (has? "errors=\"1\""))
(test-assert "one skipped"         (has? "skipped=\"1\""))

;; --- per-status elements ---
(test-assert "failure element"     (has? "<failure message="))
(test-assert "failure explanation" (has? "expected 5 but got 4"))
(test-assert "error element"       (has? "<error message="))
(test-assert "skipped element"     (has? "<skipped/>"))

;; --- XML escaping in a testcase name ---
(test-assert "escapes < and >"     (has? "a&lt;b&gt;"))
(test-assert "escapes ampersand"   (has? "&amp;"))
(test-assert "escapes quote"       (has? "&quot;c&quot;"))
(test-assert "no raw angle in name"
             (not (has? "name=\"a<b>")))

;; --- classname attribute is the (possibly nested) suite path ---
(test-assert "classname attr"      (has? "classname=\"sample\""))
(test-assert "nested classname"    (has? "classname=\"sample/inner\""))

(test-end "test2 junit xml")
(test-exit)
