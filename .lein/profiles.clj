{:user {:signing {:gpg-key "25C79D92"}
        :dependencies [[org.clojars.gjahad/debug-repl "0.3.3"]
                       [im.chit/vinyasa "0.1.8"]
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
        :plugins [[lein-pprint "1.1.1"]
                  [quickie "0.2.5"]
                  [cider/cider-nrepl "0.8.0-SNAPSHOT"]
                  [com.jakemccrary/lein-test-refresh "0.5.0"]]}}
