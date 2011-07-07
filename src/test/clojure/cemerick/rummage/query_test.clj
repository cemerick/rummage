(ns cemerick.rummage.query-test
  (:use [cemerick.rummage :as sdb]
    [cemerick.rummage-test :as test :only (*test-domain-name* defsdbtest client)]
    clojure.test)
  (:require [cemerick.rummage.encoding :as enc]))

(use-fixtures :once test/verify-domain-cleanup)

; dataset pulled from http://docs.amazonwebservices.com/AmazonSimpleDB/latest/DeveloperGuide/UsingSelectSampleDataset.html
(def dataset [{:year 1959 :pages 336 :author "Kurt Vonnegut" :title "The Sirens of Titan" ::sdb/id "0385333498"
               :keyword #{:book :paperback}
               :rating #{:***** "5 stars" "Excellent"}}
              {:year 1934 :pages 318 :author "Henry Miller" :title "Tropic of Cancer" ::sdb/id "0802131786"
               :rating :*****}
              {:year 1979 :pages 304 :author "Tom Wolfe" :title "The Right Stuff" ::sdb/id "1579124585"
               :keyword #{:book :hardcover :american}
               :rating #{"4 stars" :****}}
              {:year 2006 :author "Paul Van Dyk" :title "In Between" ::sdb/id "B000T9886K"
               :keyword #{:trance :CD}
               :rating #{"4 stars"}}
              {:year 2007 :author "Zack Snyder" :title "300" ::sdb/id "B00005JPLW"
               :keyword #{"Frank Miller" :action :DVD}
               :rating #{:*** "3 stars" "Not bad"}}
              {:year 2002 :author "Thievery Corporation" :title "Heaven's Gonna Burn Your Eyes" ::sdb/id "B000SF3NGK"
               :rating :*****}])

(defsdbtest test-special-returns
  (let [config (assoc enc/all-strings :client client :consistent-read? true)]
    (batch-put-attrs config *test-domain-name* dataset)
    (is (= 6 (query config `{select count from ~*test-domain-name*})
          ; verify that string queries are just passed through
          (query config (str "select count(*) from " (#'sdb/escape-encode *test-domain-name*)))))
    
    (is (= (->> dataset (map ::sdb/id) set)
          (set (query config `{select id from ~*test-domain-name*}))
          ; verify that string queries are just passed through
          (set (query config (str "select itemName() from " (#'sdb/escape-encode *test-domain-name*))))))
    
    (is (empty? (query config `{select id from ~*test-domain-name* where (= :unknown-key 42)})))))

(defsdbtest test-attribute-retrieval
  (let [config (assoc enc/all-strings :client client :consistent-read? true)]
    (batch-put-attrs config *test-domain-name* dataset)
    
    (is (= (->> (filter :keyword dataset)
             (map (comp count :keyword))
             set)
          (->> (query config `{select [:keyword] from ~*test-domain-name* where (not-null :keyword)})
            (map #(-> % (get ":keyword") count))
            set)))
    
    (are [expected select] (= (set expected) (set (query config select)))
      (->> (filter :keyword dataset)
        (map #(select-keys % [:year :author :title ::sdb/id]))
        ; testing of all-strings leaking in here
        (map #(into {} (for [[k v] %] [(if (= ::sdb/id k) k (str k)) (str v)]))))
      `{select [:year :title :author] from ~*test-domain-name* where (not-null :keyword)})))

(defsdbtest queries
  (let [config (assoc enc/all-strings :client client :consistent-read? true)]
    (batch-put-attrs config *test-domain-name* dataset)
    
    (are [expected select] (= (->> expected (map ::sdb/id) set)
                            (set (query config select)))
      (filter #(< 1975 (:year %) 2005) dataset)
      `{select id from ~*test-domain-name* where (between :year 1975 2005)}
      
      (remove :keyword dataset)
      `{select id from ~*test-domain-name* where (null :keyword)}
      
      (filter :rating dataset)
      `{select id from ~*test-domain-name* where (not-null :rating)}
      
      (filter (comp (partial some #{:*** :*****}) set sdb/as-collection :rating) dataset)
      `{select id from ~*test-domain-name* where (in :rating #{:*** :*****})}
      
      (->> dataset
        (filter (comp (partial some #{:*****}) set sdb/as-collection :rating))
        (filter (comp #(.endsWith % "Vonnegut") :author)))
      `{select id from ~*test-domain-name* where (and (in :rating #{:*****})
                                                   (like :author "%Vonnegut"))}
      
      (->> dataset
        (remove (comp :book set sdb/as-collection :keyword))
        (filter :keyword))
      `{select id from ~*test-domain-name* where (and (not-null :keyword)
                                                   (!= (every :keyword) :book))}

      ;; 'and' with more than 2 args
      (->> dataset
           (filter (comp :book set sdb/as-collection :keyword))
           (filter (comp (partial some #{:***** :****}) set sdb/as-collection :rating))
           (filter (comp #(.endsWith % "Vonnegut") :author)))
      `{select id from ~*test-domain-name* where (and (= :keyword :book)
                                                      (in :rating #{:***** :****})
                                                      (like :author "%Vonnegut"))}
      
      (filter (comp (partial some #{:CD :DVD}) set sdb/as-collection :keyword) dataset)
      `{select id from ~*test-domain-name* where (or (= :keyword :DVD) (= :keyword :CD))}
      
      ; live itemName() query
      (filter #(pos? (compare (::sdb/id %) "1579124585")) dataset)
      `{select id from ~*test-domain-name* where (> ::sdb/id "1579124585")}
      )))

(defsdbtest ordered-queries
  (let [config (assoc enc/all-strings :client client :consistent-read? true)]
    (batch-put-attrs config *test-domain-name* dataset)
    
    (are [expected select] (= (map ::sdb/id expected)
                             (query config select))
      
      (sort-by :year dataset)
      `{select id from ~*test-domain-name* where (> :year 0) order-by [:year]}
      
      (reverse (sort-by :year dataset))
      `{select id from ~*test-domain-name* where (> :year 0) order-by [:year desc]}
      
      ; order + limit
      (take 1 (sort-by :year dataset))
      `{select id from ~*test-domain-name* where (> :year 0) order-by [:year] limit 1}
      
      ; order + limit + desc
      (->> (sort-by :year dataset)
        reverse
        (take 1))
      `{select id from ~*test-domain-name* where (> :year 0) order-by [:year "desc"] limit 1}
      
      ; itemName()
      (->> (sort-by ::sdb/id dataset)
        (filter #(pos? (compare (::sdb/id %) "2")))
        reverse)
      `{select id from ~*test-domain-name* where (> ::sdb/id "2") order-by [::sdb/id desc]})))

(defsdbtest test-query-all
  (let [config (assoc (enc/fixed-domain-schema {:key Integer}) :client client :consistent-read? true)
        dataset (for [x (range 5000)] {::sdb/id x :key x})]
    (batch-put-attrs config *test-domain-name* dataset)
    
    (is (= (->> dataset (map :key) (apply +))
          (->> (query-all config `{select [:key] from ~*test-domain-name*})
            (map :key)
            (apply +))))))
