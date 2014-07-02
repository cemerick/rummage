(defproject bitsai/rummage "1.0.3-SNAPSHOT" 
  :parent [org.clojure/pom.contrib "0.0.20"]
  :dependencies [[com.amazonaws/aws-java-sdk "1.3.21.1"]
                 [com.cemerick/utc-dates "0.0.2"]]
  :url "http://github.com/bitsai/rummage"
  :description "A Clojure client library for Amazon's SimpleDB (SDB)."
  :java-source-paths ["src/main/java"])
