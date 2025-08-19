(ns clojnix-compiler.reader
  (:require
   [clj-stdlib.fp :refer [enum]]
   [clj-stdlib.parser :refer :all]
   [clojure.set :refer [difference union]]))

(def nilp
  (rename "nilp" (fn<$> (constantly nil) (successive-elms "nil"))))

(def truep
  (rename "truep" (fn<$> (constantly true) (successive-elms "true"))))

(def falsep
  (rename "falsep" (fn<$> (constantly false) (successive-elms "false"))))

(def str-interpolationp
  (rename "str-interpolationp" (between (charp \`) (many (notp (charp \`))) (charp \`))))

(def strlit-charp
  (->unpathed-parser "strlit-charp" (satisfy-of-set (difference (union alphanum special-syms) #{\"}))))

(def strp
  (rename "strp" 
          (fn<$> (fn [x] (apply str (map second x)))
          (between (charp \")
                          (many (somep [strlit-charp
                                        (successive-elms "\\\\")
                                        (successive-elms "\\\"")]))
                          (charp \")))))


(def special-sym-chars #{\- \. \_ \< \> \= \' \? \! \+})

(def sym-chars (set (concat alphanum special-sym-chars)))

(def sym-charp
  (->unpathed-parser "sym-charp" (satisfy-of-set sym-chars)))

(def symp
  (rename
   "symp"
   (fn<$>
    (fn [x] (symbol (apply str (remove int? (flatten (second x))))))
    (successive-ps [(notp (somep [(charp \.) (charp \/) (charp \:) (charp \@)]))
                    (many
                     (somep [(<*> (charp \:) sym-charp)
                             sym-charp]))]))))

(def pathp'
  (rename "pathp'"
          (fn<$> (fn [x] [:!path x]) pathp)))


(def vecp
  (fn [cntp]
    (rename "vecp"
            (fn<$>
             (fn [x] (let [target (:target (first (filter map? x)))]
                       (if (coll? target) (vec target) [target])))
             (successive-ps [(charp \[) (?p spacesp) (tag :target (<|> (separated-by cntp spacesp) cntp)) (?p spacesp) (charp \])])))))

(def listp
  (fn [cntp]
    (rename "listp"
            (fn<$>
             (fn [x] (let [target (:target (first (filter map? x)))]
                       (if (seq? target) target (cons target nil))))
             (successive-ps [(charp \() (?p spacesp) (tag :target (<|> (separated-by cntp spacesp) cntp)) (?p spacesp) (charp \))])))))

(def kwp
  (rename "kwp"
          (fn<$> (fn [x] (keyword (name (first (second x)))))
          (successive-ps [(charp \:) (many symp)])
          )))

(def map-entryp
  (fn [cntp] (successive-ps [(<|> strp kwp) spacesp cntp])))



(def mapp
  (fn [cntp]
    (rename "mapp"
            (fn<$>
             (fn [x]
               (let [target (:target (first (filter map? x)))
                     ]
                      (into {} (map #(conj [] (first %) (last %)) target))
                       ))
             (successive-ps [(charp \{) (?p spacesp)
                             (tag :target (<|> (separated-by (map-entryp cntp) spacesp) 
                                               (fn<$> #(conj [] %) (map-entryp cntp))))
                             (?p spacesp) (charp \})])))))

(def literalp
  (somep [intp nilp truep falsep strp pathp' symp kwp]))


(def parse
  (fn this [path]
    (->parser
     path
     "parse-source"
     (fn [charseq]
       (when-let [x (run ((somep [literalp (vecp this) (listp this) (mapp this)]) path) charseq)] x)))))

(def read-str
  (fn [string]
    (first (start parse (enum string)))
    ))
