(ns potato.core-test
  (:require [cljs.test :refer-macros [deftest testing is]]
            [potato.core]))

(deftest printable-size-test
  (is (= (potato.core/printable-size 2048) (str 2 "\u00a0" "MB"))))
