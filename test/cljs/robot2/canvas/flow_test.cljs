(ns robot2.canvas.flow-test
  (:require [cljs.test :refer [deftest is testing]]
            [robot2.canvas.flow :as flow]))

(deftest remove-flow-to-test
  (testing "quita la transicion hacia el id dado, y si el elemento que queda al final tenia regex se vuelve default"
    (is (= [[:b]] (flow/remove-flow-to [[:a "err"] [:b "ok"]] :a))))
  (testing "mismo caso con el orden invertido"
    (is (= [[:b]] (flow/remove-flow-to [[:b "ok"] [:a]] :a)))))

(deftest reconnect-flow-test
  (testing "mueve la transicion de old-state a new-state preservando la regex"
    (is (= [[:c "ok"] [:d]]
           (flow/reconnect-flow [[:b "ok"] [:d]] :b :c))))
  (testing "no permite duplicar un destino existente"
    (let [flow [[:b "ok"] [:c]]]
      (is (= flow (flow/reconnect-flow flow :b :c))))))

(deftest add-connection-test
  (testing "agrega la nueva transicion al final y marca las previas sin regex como undefined"
    (is (= [[:a "undefined"] [:b]]
           (flow/add-connection [[:a]] :b)))))

(deftest fix-flow-test
  (testing "quita transiciones hacia id y corrige el default final"
    (is (= [[:b]] (flow/fix-flow [[:b "ok"] [:x]] :x)))))

(deftest delete-state-test
  (testing "quita el estado y arregla los flows que le apuntaban"
    (let [states {:a {:flow [[:x] [:y "ok"]]}
                  :x {:flow []}
                  :y {:flow [[:x "ok"]]}}
          result (flow/delete-state states :x)]
      (is (not (contains? result :x)))
      (is (= [[:y]] (get-in result [:a :flow])))
      (is (= [] (get-in result [:y :flow]))))))
