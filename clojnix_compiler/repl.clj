(ns clojnix-compiler.repl
  (:require
   [clojnix-compiler.main :refer [compile]]
   [clojure.core.async :refer [go go-loop alt! timeout <!]]
   )
  (:import
   [java.io FileWriter BufferedReader InputStreamReader PrintWriter OutputStreamWriter]))

(defn create-nix-process []
  (let [pb (ProcessBuilder. ["nix" "repl"])
        process (.start pb)]
    {:process process
     :stdin (PrintWriter. (OutputStreamWriter. (.getOutputStream process)) true)
     :stdout (BufferedReader. (InputStreamReader. (.getInputStream process)))
     :stderr (BufferedReader. (InputStreamReader. (.getErrorStream process)))}))

(def write-to-pipe
  (fn [pipe-path msg]
    (let [writer (FileWriter. pipe-path)]
      (go (alt! (go
                  (.write writer (str msg \newline))
                  (.flush writer)) nil
                (timeout 200) nil)))))

;
(def mk-read-loop
  (fn [proc pipe-path]
    (let [stdout-reader (:stdout proc)
          stderr-reader (:stderr proc)]
      (go-loop []
        (cond
          (.ready stdout-reader)
          (<! (write-to-pipe pipe-path (.readLine stdout-reader)))

          (.ready stderr-reader)
          (<! (write-to-pipe pipe-path (.readLine stderr-reader)))

          :else
          (<! (timeout 200)))
        (recur)))))

(def write-of
  (fn [proc]
    (fn [msg]
      (let [writer (:stdin proc)]
        (.println writer msg)
        (.flush writer)))))

(def mk-repl-api
  (fn [fifo-path]
    (let [proc (create-nix-process)
          read-loop (mk-read-loop proc fifo-path)]
      {:proc proc
       :fifo-path fifo-path
       :write (write-of proc)
       :chan read-loop})))
