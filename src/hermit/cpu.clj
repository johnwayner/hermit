(ns hermit.cpu
  (:use (hermit core disassembler)))

(def init-regs {
                :a 0,
                :b 0,
                :c 0,
                :x 0,
                :y 0,
                :z 0,
                :i 0,
                :j 0,
                :o 0,
                :sp 0,
                :pc 0})
(def init-mem {})
(def key-input-loc 0x9000)
(def init-machine {:cycle 0
                   :key-input-offset 0
                   :regs init-regs
                   :mem init-mem})

(defn load-data
  "Returns new machine with the word-seq ws starting at memory location l."
  [m l ws] (assoc m :mem (reduce merge (:mem m) (map-indexed #(hash-map (+ l %1) %2) ws))))

(defn load-data-file
  "Loads data from file with file-name into memory m at location l."
  [m l file-name] (load-data m l (file-to-words file-name)))

(defn mem-val
  "Returns the value of memory location l in machine m."
  ([m l] (if-let [v (get (:mem m) (long l))]
           v
           0))
  ([m l c] (map (partial mem-val m)
                (range l (+ l c)))))


(defn mem-set
  "Returns new memory for machine with location l set to v."
  [m l v] (if (= v 0)
            (dissoc! (:mem m) (long l))
            (assoc! (:mem m) (long l) (bit-and 0xFFFF v))))

(defn machine-with-mem-set
  [m l v] (assoc! m :mem (mem-set m l v)))

(defn reg-val
  "Retruns the value of register-keyword rk (:a, :b, etc) in machine m."
  [m rk] (rk (:regs m)))

(defn reg-set
  "Returns new regs for machine with reg rk set to v."
  [m rk v] (assoc! (:regs m) rk (bit-and 0xFFFF v)))

(defn machine-with-reg-set
  [m rk v] (assoc! m :regs (reg-set m rk v)))

(defn machine-with-reg-delta
  [m rk d] (assoc! m :regs (reg-set m rk (+ d (reg-val m rk)))))

(defn machine-with-key-input
  [m c] (assoc 
          (machine-with-mem-set m
            (+ key-input-loc (:key-input-offset m)) c)
          :key-input-offset (mod (+ 1 (:key-input-offset m)) 16)))

(defn reg-from-regop-kw
  "There must be a better way... :("
  [kw] (keyword (str (first (name kw)))))

(defn machine-with-cycle-delta
  "Advances the cycle count for a machine by 1 or d."
  ([m d] (assoc! m :cycle (+ (:cycle m) d)))
  ([m] (machine-with-cycle-delta m 1)))

;;stack stuff
(defn machine-with-stack-push
  [m v] (let [m1 (machine-with-reg-delta m :sp -1)]
          (machine-with-mem-set m1 (reg-val m1 :sp) v)))

;; (defn machine-with-stack-pop
;;   [m op-val] (let [m1 (set-lval-op-val m op-val (mem-val m (reg-val m :sp)))]
;;                (machine-with-reg-delta m1 :sp 1)))

(defn get-op-val-value
  [m op-val]
  (let [val (:val op-val)]
    (case val
      :ptr-nxt (mem-val m (:nxt op-val))
      :nxt (:nxt op-val)
      :literal-20 (:literal op-val)
      (:a-ptr-nxt
       :b-ptr-nxt
       :c-ptr-nxt
       :x-ptr-nxt
       :y-ptr-nxt
       :z-ptr-nxt
       :i-ptr-nxt
       :j-ptr-nxt) (mem-val m (+ (reg-val m (reg-from-regop-kw val))
                                 (:nxt op-val))) 
       (:a-ptr
        :b-ptr
        :c-ptr
        :x-ptr
        :y-ptr
        :z-ptr
        :i-ptr
        :j-ptr) (mem-val m (reg-val m (reg-from-regop-kw val)))
        (:pop
         :push
         :peek) (let [sp (reg-val m :sp)]
                  (mem-val m sp))
         (reg-val m val))))

(defn get-a-operand-destination
  "Takes an A operand and returns either a memory location or reg to write to"
  [m op-val]
  (let [val (:val op-val)]
    (case val
      :ptr-nxt {:type :mem :val (:nxt op-val)}
      (:nxt :literal-20) {:type :nop} ;fail silently as per spec
      (:a-ptr-nxt
       :b-ptr-nxt
       :c-ptr-nxt
       :x-ptr-nxt
       :y-ptr-nxt
       :z-ptr-nxt
       :i-ptr-nxt
       :j-ptr-nxt) {:type :mem
                    :val (+ (reg-val m (reg-from-regop-kw val))
                            (:nxt op-val))} 
       (:a-ptr
        :b-ptr
        :c-ptr
        :x-ptr
        :y-ptr
        :z-ptr
        :i-ptr
        :j-ptr) {:type :mem
                 :val (reg-val m (reg-from-regop-kw val))}
        (:pop
         :push
         :peek) {:type :mem :val (reg-val m :sp)}

         {:type :reg :val val})))

(defn set-op-result
  [m {:keys [type val]} value]
  (case type
    :mem (machine-with-mem-set m val value)
    :reg (machine-with-reg-set m val value)
    :nop m))

(defn get-a-value
  [m {:keys [type val]}]
  (case type
    :mem (mem-val m val)
    :reg (reg-val m val)
    :nop 0))

(defn next-instr
  "Returns the next instruction to be executed."
  [m] (first (disassemble-memoized (mem-val m (reg-val m :pc) 3))))

(defn apply-func-to-ops
  [m i f a b]  (let [[op-res o-val] (f (get-a-value m a) b)
                     m1 (set-op-result m a op-res)]
                 (if o-val
                   (machine-with-reg-set m1 :o o-val)
                   m1)))

(defn apply-func-to-branch-ops
  [m i f a b]  (if-let [result (f a b)]
                 m
                 (machine-with-reg-delta (machine-with-cycle-delta m 1)
                   :pc (instruction-size (next-instr m))))) ;pay if check penalty

(defn non-overflow-op
  [f] (fn [a b] [(bit-and 0xFFFF (f a b)) nil]))

(def ops-map {:set (fn [a b] [b nil])
              :add (fn [a b] [(bit-and 0xFFFF (+ a b)) (if (> (+ a b) 0xFFFF) 1 0)])
              :sub (fn [a b] [(bit-and 0xFFFF (- a b)) (if (< (- a b) 0) 1 0)])
              :mul (fn [a b] (let [p (* a b)]
                              [(bit-and 0xFFFF p)
                               (bit-and 0xFFFF (bit-shift-right p 16))]))
              :div (fn [a b] (let [d (int (/ a b))]
                              [(if (= b 0) 0 (bit-and 0xFFFF d))
                               (if (= b 0)
                                 0
                                 (bit-and 0xFFFF (/ (bit-shift-left a 16) b)))]))
              :mod (non-overflow-op mod)
              :shl (fn [a b] [(bit-and 0xFFFF (bit-shift-left a b)) (bit-and 0xFFFF
                                                           (bit-shift-right (bit-shift-left
                                                                             a
                                                                             16)
                                                                            b))])
              :shr (fn [a b] [(bit-and 0xFFFF (bit-shift-right a b)) (bit-and 0xFFFF
                                                            (bit-shift-right
                                                             (bit-shift-left a 16) b))])
              :and (non-overflow-op bit-and)
              :bor (non-overflow-op bit-or)
              :xor (non-overflow-op bit-xor)})

(def branch-ops-map {:ife =
                     :ifn (comp not =)
                     :ifg >
                     :ifb #(> (bit-and %1 %2) 0)})

;; Pre-Op Planning code
(defn plan-for-op-val
  [op-val operand]
  (let [plan-for-evaling-operand {:type :eval
                                  :args {:operand operand
                                         :op-val op-val}
                                  :comment (str "Evaling Operand " operand)}]
    (case (:val op-val)
      (:ptr-nxt
       :nxt
       :a-ptr-nxt
       :b-ptr-nxt
       :c-ptr-nxt
       :x-ptr-nxt
       :y-ptr-nxt
       :z-ptr-nxt
       :i-ptr-nxt
       :j-ptr-nxt) [{:type :cycle
                     :args {:delta 1}
                     :comment (str "Increasing cycle count. Reason: Read next word for " operand)}
                    plan-for-evaling-operand]
       :pop [plan-for-evaling-operand
             {:type :reg-mod
              :args {:reg :sp
                     :delta 1}
              :comment "Moving SP for POP"}]
       :push [{:type :reg-mod
               :args {:reg :sp
                      :delta -1}
               :comment "Moving SP for PUSH"}
              plan-for-evaling-operand]
       nil []
       [plan-for-evaling-operand])))



(defn pre-op-plan-for-instruction
  "Returns a vector of machine transformations that need to happen for an instruction to be considered executed."
  [i] (into [{:type :reg-mod
              :args {:reg :pc
                     :delta (instruction-size i)}
              :comment "Increasing PC after instruction read."}]
            (into  (plan-for-op-val (:a i) :a)
                   (plan-for-op-val (:b i) :b))))

(def pre-op-plan-for-instruction-memoized (memoize pre-op-plan-for-instruction))

(defn execute-plan
  [m i plan] (reduce (fn [[m a b] step]
                     (case (:type step)
                       :cycle [(machine-with-cycle-delta m (:delta (:args step))) a b]
                       :reg-mod [(machine-with-reg-delta m (:reg (:args step)) (:delta (:args step))) a b]
                       :eval (case (:op i)
                               :jsr [m (get-op-val-value m (:op-val (:args step))) b]
                               (:ife
                                :ifn
                                :ifg
                                :ifb) (let [op-val (:op-val (:args step))]
                                        (case (:operand (:args step))
                                          :a [m (get-op-val-value m op-val) b]
                                          :b [m a (get-op-val-value m op-val)]))
                                (let [op-val (:op-val (:args step))]
                                  (case (:operand (:args step))
                                    :a [m (get-a-operand-destination m op-val) b]
                                    :b [m a (get-op-val-value m op-val)])))))
                     [m nil nil]
                     plan))

(defn make-transient-machine
  [m] (transient (merge m
                        {:mem (transient (:mem m))}
                        {:regs (transient (:regs m))})))

(defn make-persistent-machine
  [tm] (persistent! (assoc! tm
                            :mem (persistent! (:mem tm))
                            :regs (persistent! (:regs tm)))))

(defn step
  "Steps a machine one instruction."
  [m] (let [trans-m (make-transient-machine m)
            i (next-instr trans-m)
            op (:op i)
            [post-plan-m a b] (execute-plan trans-m i (pre-op-plan-for-instruction-memoized i))
            post-op-m (cond
                       (op ops-map) (apply-func-to-ops post-plan-m i (op ops-map) a b)
                       (op branch-ops-map) (apply-func-to-branch-ops post-plan-m i (op branch-ops-map) a b)
                       (= :jsr op) (let [m1 (machine-with-stack-push
                                              post-plan-m
                                              (reg-val post-plan-m :pc))]
                                     (machine-with-reg-set m1 :pc a))
                       :default (do (println (str "INVALID OPCODE: "
                                                  op
                                                  " PC:0x"
                                                  (Integer/toHexString (reg-val m :pc))
                                                  " <0x"
                                                  (Integer/toHexString (:w i))
                                                  ">"))
                                    m)
                       )]
        (make-persistent-machine (machine-with-cycle-delta post-op-m (:cost (op op-props))))))

(defn nth-step [n m] (last (take (+ 1 n) (iterate step m))))