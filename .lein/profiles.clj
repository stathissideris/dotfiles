{:user {:signing {:gpg-key "25C79D92"}
        :dependencies [[org.clojars.gjahad/debug-repl "0.3.3"]
                       [im.chit/vinyasa "0.2.1"
                        :exclusions [org.apache.httpcomponents/httpclient
                                     org.apache.httpcomponents/httpcore]]
                       [difform "1.1.2"]
                       [pjstadig/humane-test-output "0.6.0"]]
        :injections [(require 'vinyasa.inject)
                     (require 'vinyasa.pull)
                     (require 'alex-and-georges.debug-repl)
                     (require 'com.georgejahad.difform)
                     (require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)
                     (vinyasa.inject/inject
                      'clojure.core
                      '[[vinyasa.pull pull]
                        [alex-and-georges.debug-repl debug-repl]
                        [com.georgejahad.difform difform]])]
        :test-refresh {:notify-command ["tmux" "display-message"]
                       :notify-on-success true}
        :plugins [[lein-pprint "1.1.2"]
                  [cider/cider-nrepl "0.8.1"]
                  [com.jakemccrary/lein-test-refresh "0.5.4"]]}}
