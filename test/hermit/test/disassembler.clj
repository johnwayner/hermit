(ns hermit.test.disassembler
  (:use [hermit.disassembler])
  (:use [clojure.test]))

(deftest test-ops
  (is (= (ops 1) :set) "op 0x01 should be :set"))
