(defproject org.clojars.mihaelkonjevic/path-matcher "0.1.0"
  :description "FIXME: write description"
  :url "https://github.com/retro/path-matcher"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies []
  :plugins [[lein-cloverage "1.0.13"]
            [lein-shell "0.5.0"]
            [lein-ancient "0.6.15"]
            [lein-changelog "0.3.2"]]
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.10.0"]]}}
  :deploy-repositories [["releases" :clojars]]
  :aliases {"update-readme-version" ["shell" "sed" "-i" "s/\\\\[org\\.clojars\\.mihaelkonjevic\\\\/path-matcher \"[0-9.]*\"\\\\]/[org\\.clojars\\.mihaelkonjevic\\\\/path-matcher \"${:version}\"]/" "README.md"]}
  :release-tasks [["shell" "git" "diff" "--exit-code"]
                  ["change" "version" "leiningen.release/bump-version"]
                  ["change" "version" "leiningen.release/bump-version" "release"]
                  ["changelog" "release"]
                  ["update-readme-version"]
                  ["vcs" "commit"]
                  ["vcs" "tag"]
                  ["deploy"]
                  ["vcs" "push"]])
