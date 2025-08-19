(ns clojnix-compiler.stdlib
  (:require
   [clojnix-compiler.builtins :refer [builtins]]
   [clojnix-compiler.main :refer [compile]]
   [clojnix-compiler.reader :refer [read-str]]))

(def get-impl
  (compile builtins (read-str "(fn [target path]
    (if (vector? path)
  (attr-by-path path \"failure to find path\" target)
  (get-attr path target)
  )
  )")))

(def stdlib 
  (merge builtins 
         {'get get-impl}))
