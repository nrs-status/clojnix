(ns clojnix-compiler.compiler-core
  (:require
   [clj-stdlib.cli-tools :refer [handle-possibly-relative-path]]
   [clojure.string :refer [join]]
   [clj-stdlib.lazy :as lazy]
   [clojure.java.io :as io]))

(def compile-vec-form
  (fn [compile-fn x]
    (str "[ " (join " " (map compile-fn x)) " ]")))

(def mk-aset-entry
  (fn [[k v]]
    (fn [continue]
      (let [handle-key (if (keyword? k)
                         (name k)
                         (str \" k \")) ;assuming string
            ]
        (str " " handle-key " = " (continue v) " ;")))))

(def compile-aset
  (fn [compile-fn x]
    (str "{ " (join " " (map (comp #(% compile-fn) mk-aset-entry) (vec x))) " }")))

(def fn-form?
  (fn [x]
    (and
     (seq? x)
     (= 'fn (first x)))))

(def compile-fn-form
  (fn [compile-fn prev-id-table fn-form]
    (let [nm-pos (when (= 4 (count fn-form)) 1)
          nm (when nm-pos (nth fn-form nm-pos))
          bindings-pos (if (= 4 (count fn-form)) 2 1)
          bindings (nth fn-form bindings-pos)
          bindings-w-uid (into {} (map (fn [sy] [sy (gensym sy)]) bindings))
          bindings-as-fn-head (str (join ": " (vals bindings-w-uid)) ": ")
          body-pos (if (= 4 (count fn-form)) 3 2)
          body (nth fn-form body-pos)
          new-id-table (#(if nm (assoc % nm nm) %) (merge prev-id-table bindings-w-uid))
          expr (str "( " bindings-as-fn-head (compile-fn new-id-table body) " )")]
      (if nm
        (str "( let " (name nm) " = " expr " ; in " (name nm) " )")
        expr))))

(def let-form?
  (fn [x]
    (and
     (seq? x)
     (= 'let (first x)))))

(def let-bindings->compiled-pairs
  (fn this [compile-fn-wo-id-table prev-id-table let-bindings]
    (when-let [next-batch (seq (take 2 let-bindings))]
      (let [compilation-result (compile-fn-wo-id-table prev-id-table (second next-batch))
            new-entry {(first next-batch) compilation-result}
            next-table (merge prev-id-table new-entry)]
        (into (vec new-entry) (this compile-fn-wo-id-table next-table (drop 2 let-bindings)))))))

(def compile-let-form
  (fn this [compile-fn-wo-id-table prev-id-table let-form]
    (let [bindings (second let-form)
          final-sym (gensym "final")
          body-as-binding [final-sym (last let-form)]
          compilation (let-bindings->compiled-pairs compile-fn-wo-id-table prev-id-table (concat bindings body-as-binding))
          nix-let-entries (join " " (map #(str (name (first %)) " = " (second %) " ;") compilation))]
      (str "( let " nix-let-entries " in " (name final-sym) " )"))))

(def escape-dq
  (fn [string]
    (let [l-nbds (lazy/r-nbds (reverse (vec string)))
          escaping (map #(if (= \" (first %))
                           (if (= (seq [\" \\]) (take 2 %))
                             (first %)
                             (seq [\" \\]))
                           (first %)) l-nbds)]
      (apply str (reverse (flatten escaping))))))

(def import?
  (fn [x]
    (and (seq? x)
         (= 'import (first x)))))

(def compile
  (fn this-compile [pwd read-str-fn id-table x]
    (cond
      (keyword? x) (str \" (name x) \")
      (symbol? x) (if-let [res (x id-table)] res
                          (str "(throw \"unresolved sym: " (name x) "\")"))
      (import? x) (let [filepath (io/file (handle-possibly-relative-path pwd (second x)))
                        new-pwd (.getParent filepath)]
                    (this-compile new-pwd read-str-fn id-table (read-str-fn (slurp (.getPath filepath)))))
      (nil? x) "null"
      (string? x) (apply str (cons \" (conj (vec (escape-dq x)) \")))
      (true? x) "true"
      (false? x) "false"
      (int? x) (str x)
      (map? x) (compile-aset (partial this-compile pwd read-str-fn id-table) x)
      (vector? x) (compile-vec-form (partial this-compile pwd read-str-fn id-table) x)
      (fn-form? x) (compile-fn-form (partial this-compile pwd read-str-fn) id-table x)
      (let-form? x) (compile-let-form (partial this-compile pwd read-str-fn) id-table x)
      (seq? x) (str "( " (join " " (map (partial this-compile pwd read-str-fn id-table) x)) " )"))))

