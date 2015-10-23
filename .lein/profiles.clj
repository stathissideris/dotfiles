{:user
 {:signing {:gpg-key "7AF00D02"}
  :dependencies [#_[org.clojars.gjahad/debug-repl "0.3.3"]
                 #_[difform "1.1.2"]
                 [alembic "0.3.2"]
                 [im.chit/vinyasa "0.4.2"]
                 [pjstadig/humane-test-output "0.7.0"]
                 [org.clojure/tools.nrepl "0.2.11"]]
  :injections [(require '[vinyasa.inject :as inject])
               #_(require 'alex-and-georges.debug-repl)
               #_(require 'com.georgejahad.difform)
               (require 'pjstadig.humane-test-output)
               (pjstadig.humane-test-output/activate!)
               (vinyasa.inject/in
                clojure.core >
                [alembic.still [distill pull]]
                [clojure.pprint pprint]
                [clojure.repl doc source pst])]
  :test-refresh {:notify-command ["tmux" "display-message"]
                 :notify-on-success true}
  :plugins [[lein-pprint "1.1.2"]
            [refactor-nrepl "2.0.0-SNAPSHOT"]
            [cider/cider-nrepl "0.10.0-SNAPSHOT"]
            [com.jakemccrary/lein-test-refresh "0.5.4"]]}}
