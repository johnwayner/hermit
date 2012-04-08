(ns hermit.dis)

(def ops [:ext
          :set
          :add :sub :mul :div :mod
          :shl :shr
          :and :bor :xor
          :ife :ifn :ifg :ifb])
(def ext-ops [:ext :jsr])