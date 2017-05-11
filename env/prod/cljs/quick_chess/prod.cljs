(ns quick-chess.prod
  (:require [quick-chess.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
