(ns robot2.test-runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [robot2.canvas.geometry-test]
            [robot2.canvas.flow-test]
            [robot2.canvas.interactions-test]
            [robot2.operations.registry-test]))

(doo-tests 'robot2.canvas.geometry-test
           'robot2.canvas.flow-test
           'robot2.canvas.interactions-test
           'robot2.operations.registry-test)
