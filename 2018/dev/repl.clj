(ns repl
  (:require
    [clojure.tools.nrepl.server :as nrepl-server]
    [clojure.java.io :as io]
    [rebel-readline.main :as rebel-main]
    ))

(defn handler []
  "Invoke Cider nREPL handler."
  (require 'cider.nrepl)
  (ns-resolve 'cider.nrepl 'cider-nrepl-handler))

(defn tidy-file
  "Create a file object that will be deleted on JVM exit."
  [filename]
  (doto (io/as-file filename) (.deleteOnExit)))

(defn -main [& args]
  "Start nREPL and then Rebel Readline."
  (let [server (nrepl-server/start-server :port 0 :bind "127.0.0.1" :handler (handler))
        port (:port server)]
    (spit (tidy-file ".nrepl-port") port)
    (println "nREPL server started on port" port)
    (apply rebel-main/-main args)))

