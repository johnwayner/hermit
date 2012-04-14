(ns hermit.test.integrated
  (:use [hermit disassembler assembler cpu])
  (:use [clojure.test]))

(defmacro execute-asm
  ([n & body] `(last (take (+ 1 ~n) (iterate step (load-data init-machine 0 (asm ~@body)))))))

(defmacro def-reg-test
  [name [iters reg expect] & asm]
  `(deftest ~name (is (= (reg-val (execute-asm ~iters ~@asm) ~reg) ~expect))))

(defn watch-reg
  "Shows the value of register 'reg' from step 'start' to step 'stop'"
  ([reg start stop code] (map #(reg-val % reg)
                         (drop start
                               (take stop
                                     (iterate step (load-data init-machine 0 code)))))))

(def-reg-test add_i_1
  [1 :i 1]
  (add i 1))

(def-reg-test add_i_0xFFFF
  [1 :i 0xFFFF]
  (add i 0xFFFF))

(def-reg-test ife_success
  [4 :x 1]
  (set i 3)
  (ife i 3)
  (set x 1)
  (set y 2))

(def-reg-test ife_fail
  [3 :x 0]
  (set i 3)
  (ife i 0)
  (set x 1)
  (set y 2))

(def-reg-test o_add
  [2 :o 1]
  (set x 0x1001)
  (add x, 0xFFFF))

(def-reg-test o_mul
  [2 :o 0x1000]
  (set x 0x1001)
  (mul x, 0xFFFF))
