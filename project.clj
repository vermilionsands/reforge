(defproject vermilionsands.reforge "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.ow2.asm/asm "6.0"]
                 [org.ow2.asm/asm-util "6.0"]]
  :profiles {:dev  {:source-paths ["dev"]
                    :aot [types]}})