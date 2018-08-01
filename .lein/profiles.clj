{:user
 {:signing {:gpg-key "67D85DDC"}
  :dependencies [;;[alembic "0.3.2"]
                 ;;[org.clojure/tools.nrepl "0.2.12"]
                 ;;[im.chit/vinyasa "0.4.7"]
                 [pjstadig/humane-test-output "0.8.0"]]
  :injections [#_(require '[vinyasa.inject :as inject])
               (require 'pjstadig.humane-test-output)
               (pjstadig.humane-test-output/activate!)
               #_(vinyasa.inject/in
                clojure.core >
                [alembic.still [distill pull]]
                [clojure.pprint pprint]
                [clojure.repl doc source pst]
                [clojure.data diff])
               ;;don't work:
               #_(defn >ppr [x] (clojure.pprint/pprint x) x)
               #_(defn >show-diff [a b]
                 (let [[removed added] (clojure.data/diff a b)]
                   (println "Removed:")
                   (clojure.pprint/pprint removed)
                   (println "\nAdded:")
                   (clojure.pprint/pprint added)))]
  :test-refresh {;;:notify-command ["tmux" "display-message"]
                 ;;:notify-on-success true
                 }
  :plugins [[cider/cider-nrepl "0.17.0"]
            [lein-count "1.0.8"]
            [lein-cloverage "1.0.10"]
            [jonase/eastwood "0.2.5"]
            ;;[lein-pprint "1.1.2"]
            ;;[refactor-nrepl "2.3.1"]
            [lein-ancient "0.6.15"]
            [lein-ns-dep-graph "0.2.0-SNAPSHOT"] ;;run with: lein ns-dep-graph
            [com.jakemccrary/lein-test-refresh "0.16.0"]]}}
