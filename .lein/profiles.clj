{:user {:signing {:gpg-key "25C79D92"}
        :dependencies [#_[org.clojars.gjahad/debug-repl "0.3.3"]
                       #_[im.chit/vinyasa "0.2.1"
                        :exclusions [org.apache.httpcomponents/httpclient
                                     org.apache.httpcomponents/httpcore]]
                       [difform "1.1.2"]
                       [pjstadig/humane-test-output "0.7.0"]
                       [org.clojure/tools.nrepl "0.2.11"]]
        :injections [#_(require 'vinyasa.inject)
                     #_(require 'vinyasa.pull)
                     #_(require 'alex-and-georges.debug-repl)
                     #_(require 'com.georgejahad.difform)
                     (require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)
                     #_(vinyasa.inject/inject
                      'clojure.core
                      '[[vinyasa.pull pull]
                        [alex-and-georges.debug-repl debug-repl]
                        [com.georgejahad.difform difform]])]
        :test-refresh {:notify-command ["tmux" "display-message"]
                       :notify-on-success true}
        :plugins [[lein-pprint "1.1.2"]
                  [refactor-nrepl "2.0.0-SNAPSHOT"]
                  [cider/cider-nrepl "0.10.0-SNAPSHOT"]
                  [com.jakemccrary/lein-test-refresh "0.5.4"]]}}
