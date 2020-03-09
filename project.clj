(defproject uolueris "1.0.0"
  :description "generate bit squatting for a given domain"
  :url "https://github.com/ezwal"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]]
  :main ^:skip-aot uolueris.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})