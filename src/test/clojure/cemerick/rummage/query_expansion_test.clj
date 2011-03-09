(ns cemerick.rummage.query-expansion-test
  (:use [cemerick.rummage :as sdb]
    clojure.test)
  (:require [cemerick.rummage.encoding :as enc]))

(deftest basic-queries
  (are [query-string select-map] (= query-string (#'sdb/select-string enc/all-strings select-map))
    ; *, count, id
    "select * from `foo`" `{select * from foo}
    "select * from `foo`" `{:select * :from "foo"}
    "select count(*) from `foo`" `{select count from foo}
    "select itemName() from `foo`" `{:select id :from "foo"}
    
    ; attribute lists
    "select `a` from `foo`" `{:select [a] :from "foo"}
    "select `a`, `b`, `c` from `foo`" `{:select [a b c] :from "foo"}
    "select `a`, `b`, `c` from `:foo`" `{select [a b c] from :foo}
    
    ; simple comparisions
    "select * from `foo` where `a` = '5'" `{select * from foo where (= a 5)}
    "select * from `foo` where `a` != '5'" `{select * from foo where (!= a 5)}
    "select * from `foo` where `a` < '5'" `{select * from foo where (< a 5)}
    "select * from `foo` where `a` >= '5'" `{select * from foo where (>= a 5)}
    "select * from `foo` where `a` like 'bar%'" `{select * from foo where (like a "bar%")}
    "select * from `foo` where `a` not like '%bar'" `{select * from foo where (not-like a "%bar")}
    
    ; null, not-null
    "select * from `foo` where `a` is null" `{:select * :from "foo" :where (null a)}
    "select * from `foo` where `a` is not null" `{:select * :from "foo" :where (not-null a)}
    
    ; between, in
    "select * from `foo` where `a` between '2000' and '2010'" `{:select * :from "foo" :where (between a 2000 2010)}
    "select * from `foo` where `a` in ('1', '3', '5')" `{:select * :from "foo" :where (in a [1 3 5])}
    
    ; every
    "select * from `foo` where every(`a`) in ('1', '3', '5')" `{:select * :from "foo" :where (in (every a) [1 3 5])}
    
    ; and, or, intersection
    "select * from `foo` where (`a` > '5') intersection (`b` < '25')" `{select * from foo where (intersection (> a 5) (< b 25))}
    "select * from `foo` where (`c` is not null) or ((`a` > '5') and (`b` < '25'))" `{select * from foo where (or (not-null c) (and (> a 5) (< b 25)))}
    
    ; not
    "select * from `foo` where not ((`a` < '5') or (`b` > '25'))" `{select * from foo where (not (or (< a 5) (> b 25)))}
    ))

(deftest escaping
  (are [query-string select-map] (= query-string (#'sdb/select-string enc/all-strings select-map))
    "select ```a'\"` from `foo` where ```a'\"` = '`a''\"'" `{select ["`a'\""] from foo where (= "`a'\"" "`a'\"")}
    ))

(deftest invalid-queries
  (are [select-map exception-pattern] (thrown-with-msg? Exception exception-pattern
                                        (#'sdb/select-string enc/all-strings select-map))
    `{select a from foo} #"invalid attribute spec"
    `{select "a" from foo} #"invalid attribute spec"
    `{select ids from foo} #"invalid attribute spec"
    
    `{select * from foo where (> (foo a) 0)} #"was expecting `every`"
    ))