;;;; mixed.scm -- a deliberately mixed suite, used to demonstrate CI reporting.
;;
;; Run it with the TEST_XML environment variable set to capture a JUnit report:
;;
;;     TEST_XML=test-results/junit.xml csi -s examples/mixed.scm
;;
;; It intentionally contains failures and an error so the CI dashboard has
;; something interesting to show.  `test-exit' returns non-zero, so in CI wrap
;; the invocation with `|| true' and let the JUnit report surface the details.

(import test2)

(test-begin "arithmetic")
(test "addition"        4 (+ 2 2))
(test "subtraction"     0 (- 2 2))
(test "off by one"      5 (+ 2 2))          ; FAIL (demo)
(test-assert "positive" (> (* 3 3) 0))
(test-end "arithmetic")

(test-begin "lists")
(test "map"    '(2 4 6) (map (lambda (x) (* 2 x)) '(1 2 3)))
(test "length" 3        (length '(a b c)))
(test-error "car of empty" (car '()))        ; PASS (error expected)
(test "bad index" 'z (list-ref '(a b c) 9))  ; ERROR (demo)
(test-group "nested"
  (test "reverse" '(c b a) (reverse '(a b c)))
  (test "append"  '(1 2 3 4) (append '(1 2) '(3 4))))
(test-end "lists")

(test-exit)
