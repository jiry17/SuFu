(set-option :print-success true)
(set-logic UFLIA)
(set-option :produce-models true)
(declare-fun f2 (Int) Int)
(assert
 (forall ((i0 Int) (x Int) (y Int))
  (and (= (- x y) (+ i0 (f2 i0))))))
(check-sat)
