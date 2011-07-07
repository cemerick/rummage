;   Copyright (c) Chas Emerick, Rich Hickey, and other contributors. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns cemerick.rummage
  (:use [cemerick.rummage.encoding :as encoding :only (strip-symbol-ns)])
  (:import
    cemerick.rummage.DataUtils
    com.amazonaws.ClientConfiguration
    (com.amazonaws.services.simpledb AmazonSimpleDBClient)
    (com.amazonaws.services.simpledb.model CreateDomainRequest DeleteDomainRequest
      ListDomainsRequest DomainMetadataRequest Attribute
      ReplaceableItem ReplaceableAttribute PutAttributesRequest
      GetAttributesRequest UpdateCondition BatchPutAttributesRequest
      DeleteAttributesRequest DeletableItem BatchDeleteAttributesRequest
      SelectRequest Item)))

(defn create-client
  "Creates a client for talking to a specific AWS SimpleDB
  account. The same client can be reused for multiple requests."
  ([id secret-key]
    (create-client id secret-key (ClientConfiguration.)))
  ([id secret-key ^ClientConfiguration client-config]
    (AmazonSimpleDBClient. (com.amazonaws.auth.BasicAWSCredentials. id secret-key)
      (.withUserAgent client-config "Rummage - SDB for Clojure"))))

(defn- ^AmazonSimpleDBClient client
  [client-or-config]
  (or (:client client-or-config) client-or-config))

(defn create-domain
  "Creates a domain with the specified name.  Returns successfully if the domain
   already exists."
  [client-or-config name]
  (.createDomain (client client-or-config) (CreateDomainRequest. name)))

(defn delete-domain
  "Deletes the named domain."
  [client-or-config name]
  (.deleteDomain (client client-or-config)
    (DeleteDomainRequest. name)))

(defn- list-domains*
  [client-or-config next-token]
  (let [req (-> (ListDomainsRequest.) (.withNextToken next-token))
        res (.listDomains (client client-or-config) req)]
    (concat (.getDomainNames res)
      (when (.getNextToken res)
        (list-domains* client (.getNextToken res))))))

(defn list-domains
  "Returns a sequence of all domain names available from the given client."
  [client-or-config]
  (list-domains* (client client-or-config) nil))

(def domain-metadata-keys
  #{:timestamp :attributeValuesSizeBytes :attributeNameCount :itemCount
    :attributeValueCount :attributeNamesSizeBytes :itemNamesSizeBytes})

(defn domain-metadata 
  "Returns a map of domain metadata"
  [client-or-config domain]
  (select-keys
    (-> (client client-or-config)
      (.domainMetadata (DomainMetadataRequest. domain))
      bean)
    domain-metadata-keys))

(defn as-collection
  "If v is a collection, returns it, else returns a collection containing v."
  [v]
  (if (or (coll? v) (instance? java.util.Collection v)) v [v]))

(defn- as-set
  "If v is a set, returns it, else returns a set containing v."
  [v]
  (if (set? v) v (hash-set v)))

(defn- build-attrs
  [encode-fn item add-to?]
  (for [[k v] (dissoc item ::id)
        v (as-collection v)
        :let [[name value] (encode-fn k v)]]
    (ReplaceableAttribute. name value (if add-to?
                                        (not (add-to? k))
                                        true))))

(defn- update-condition
  [encode-fn [key value :as expecting] not-expecting]
  (when (and expecting not-expecting)
    (throw (IllegalArgumentException. "Cannot have both :expecting and :not-expecting update conditions")))
  (cond
    expecting (let [c (.withExists (UpdateCondition.) true)]
                (if value
                  (let [[k v] (encode-fn key value)]
                    (-> c (.withName k) (.withValue v)))
                  (.withName c (encode-fn key))))
    not-expecting (UpdateCondition. (encode-fn not-expecting) nil false)))

(defn put-attrs
  "Puts attrs for one item into the domain. By default, attrs replace
  all values present at the same attrs/keys. You can pass an add-to?
  function (usually a set), and when it returns true for a key, values
  will be added to the set of values at that key, if any."
  [client-config domain item & {:keys [add-to? expecting not-expecting]}]
  (let [id ((:encode-id client-config) (::id item))
        attrs (build-attrs (:encode client-config) item add-to?)
        update-condition (update-condition (:encode client-config) expecting not-expecting)]
    (.putAttributes
      (client client-config)
      (.withExpected (PutAttributesRequest. domain id attrs) update-condition))))

(defn- encode-item
  [{:keys [encode encode-id]} add-to? item]
  (ReplaceableItem. (encode-id (::id item))
    (build-attrs encode item add-to?)))

(defn batch-put-attrs
  "Puts the attrs for multiple items into a domain, with the same semantics as put-attrs"
  [client-config domain items & {:keys [add-to?]}]
  (doseq [batch (partition-all 25 (map (partial encode-item client-config add-to?) items))]
    (.batchPutAttributes
      (client client-config)
      (BatchPutAttributesRequest. domain batch))))

(defn- into-map
  [decode-fn item-id attributes]
  (reduce
    (fn [m ^Attribute a]
      (let [[k v] (decode-fn (.getName a) (.getValue a))]
        (update-in m [k] #(if % (-> % as-set (conj v)) v))))
    {::id item-id}
    attributes))

(defn get-attrs
  [client-config domain item-id & attr-names]
  (let [req (-> (GetAttributesRequest. domain ((:encode-id client-config) item-id))
              (.withConsistentRead (-> client-config :consistent-read? boolean))
              (.withAttributeNames ^java.util.Collection (map (:encode client-config) attr-names)))
        res (.getAttributes (client client-config) req)]
    (when-let [attrs (-> res .getAttributes seq)]
      (if-let [decode-fn (:decode client-config)]
        (into-map decode-fn item-id attrs)
        (throw (IllegalArgumentException. "No :decode function available in client-config"))))))

(defn- build-delete-attrs
  [encode-fn attrs]
  (let [attrs (cond
                (or (empty? attrs) (map? attrs)) attrs
                (set? attrs) (for [key attrs] [key nil])
                :else (throw (IllegalArgumentException.
                               ":attrs must be a set of attribute keys, a map of attribute keys and values, or absent to delete all attributes of item-id")))]
    (flatten
      (for [[key value] attrs]
        (if value
          (for [v (as-collection value)]
            (let [[name value] (encode-fn key v)]
              (Attribute. name value)))
          (.withName (Attribute.) (encode-fn key)))))))

(defn delete-attrs
  "Deletes the attrs from the item. If no attrs are supplied, deletes
  all attrs and the item. attrs can be a set, in which case all values
  at those keys will be deleted, or a map, in which case only the
  values supplied will be deleted."
  [client-config domain item-id & {:keys [attrs expecting not-expecting]}]
  (when (and expecting not-expecting)
    (throw (IllegalArgumentException. "Cannot have both :expecting and :not-expecting delete conditions")))
  (let [encode-fn (:encode client-config)
        attrs (build-delete-attrs encode-fn attrs)
        update-condition (update-condition encode-fn expecting not-expecting)]
    (.deleteAttributes
      (client client-config)
      (DeleteAttributesRequest. domain ((:encode-id client-config) item-id) attrs update-condition))))

(defn batch-delete-attrs
  [client-config domain item-attrs]
  (let [encode-id (:encode-id client-config)
        encode-fn (:encode client-config)
        del-items (for [[item-id attrs] item-attrs]
                    (DeletableItem. (encode-id item-id) (build-delete-attrs encode-fn attrs)))]
    (doseq [batch (partition-all 25 del-items)]
      (.batchDeleteAttributes
        (client client-config)
        (BatchDeleteAttributesRequest. domain batch)))))

(def ^{:private true} *select-encode-fn*)
(def ^{:private true} *select-encode-id-fn*)

(defn- escape
  [^String name-quote ^String value-quote]
  (let [name-doubled (str name-quote name-quote)
        value-doubled (str value-quote value-quote)]
    (fn esc
      ([name]
        ; encode fns return coll from 2-arg, scalar from 1-arg; making escape cleanly comp-able
        (if (coll? name)
          (apply esc name)
          (str name-quote (.replace (str name) name-quote name-doubled) name-quote)))
      ([name ^String value]
        [(esc name)
         (str value-quote (.replace value value-quote value-doubled) value-quote)]))))

(def ^{:private true} escape-encode (escape "`" "'"))
(def ^{:private true} escape-id-encode (escape "'" "'"))

(defn- attribute-clause
  [encode-fn encode-id-fn & [attr value :as args]]
  (let [value? (= 2 (count args))]
    (cond
      (= ::id attr) (if value?
                      ["itemName()" (encode-id-fn value)]
                      "itemName()")
      
      (= 'every (-> attr as-collection first strip-symbol-ns))
      (let [[name value] (apply encode-fn (cons (-> attr second strip-symbol-ns) (rest args)))
            name (format "every(%s)" name)]
        (if value? [name value] value))
      
      :else (apply encode-fn args))))

(defn- where-str
  [where-expansions expr]
  (binding [where-str (partial where-str where-expansions)]
    (let [expr-op (-> expr first strip-symbol-ns)
          expansion-fn (where-expansions expr-op)]
      (when-not expansion-fn
        (throw (IllegalArgumentException. (str "No expansion available for where expression " expr))))
      (apply expansion-fn (-> expr-op name (.replace "-" " ")) (map strip-symbol-ns (rest expr))))))

(def ^{:private true} where-expansions
  (let [base {'not #(format "not (%s)" (where-str %2))
              
              '[and or intersection]
              (fn where-expr
                ([op arg]
                   (format "(%1$s)" (where-str arg)))
                ([op arg1 arg2]
                   (format (str "(%1$s) " (name op) " (%2$s)") (where-str arg1) (where-str arg2)))
                ([op arg1 arg2 & args]
                   (format (str "(%1$s) " (name op) " (%2$s)")
                           (where-str arg1)
                           (apply where-expr op arg2 args))))
              
              '[= != < <= > >= like not-like]
              (fn comparison [op & name-value]
                (apply format "%2$s %1$s %3$s" op (apply *select-encode-fn* name-value)))
              
              '[null not-null]
              #(format "%s is %s" (*select-encode-fn* %2) %)
              
              'between
              (fn [_ name val1 val2]
                (let [[attr val1] (*select-encode-fn* name val1)
                      [_ val2] (*select-encode-fn* name val2)]
                  (format "%s between %s and %s" attr val1 val2)))
              
              'in
              (fn [_ name values]
                ; the repeated encoding of `name` here given large sets of values is unfortunate
                (let [pairs (map #(*select-encode-fn* name %) values)]
                  (format "%s in (%s)" (ffirst pairs) (->> pairs
                                                       (map second)
                                                       (interpose ", ")
                                                       (apply str)))))}]
    (->> base
      (mapcat (fn [[k v :as pair]]
                (if (coll? k)
                  (for [op k] [op v])
                  [pair])))
      (into {}))))

(def ^{:private true} query-language
  {:select #(case (strip-symbol-ns %)
              '* "*"
              'id "itemName()"
              'count "count(*)"
              (if-not (coll? %)
                (throw (IllegalArgumentException. (str "invalid attribute spec: " %)))
                (->> (map strip-symbol-ns %)
                  (map *select-encode-fn*)
                  (interpose ", ")
                  (apply str))))
   :from (comp escape-encode strip-symbol-ns)
   :where (partial where-str where-expansions)
   :order-by (fn [& [args]]
               (when-not (sequential? args)
                 (throw (IllegalArgumentException. (str "order-by expects vector of attribute name and optional asc/desc sort direction, got: " args))))
               (let [[attr direction] args]
                 (str (*select-encode-fn* (strip-symbol-ns attr))
                   \space
                   (or (strip-symbol-ns direction) "asc"))))
   :limit #(cond
             (not (integer? %))
             (throw (IllegalArgumentException. (str "limit expects an integer, got " count)))
             
             (not (<= 1 %))
             (throw (IllegalArgumentException. (str "limit expects an integer 1 <= n <= 2500, got " %)))
             
             :else (min 2500 %))})

(defn select-string
  "Produces a string representing the query map in the SDB Select language.
  `query` calls this for you.  This is public only to aid in diagnostics and otherwise
  understanding how query maps are translated into query strings."
  [config select-map]
  (binding [*select-encode-fn* (partial attribute-clause
                                 (comp escape-encode (:encode config))
                                 (comp escape-id-encode (:encode-id config)))]
    (let [select-map (->> select-map
                       (map (fn [[k v]] [(strip-symbol-ns k) v]))
                       (into {}))]
      (->> (for [k [:select :from :where :order-by :limit]
               :let [expansion-fn (k query-language)
                     expression (or (select-map k)
                                  (select-map (-> k name symbol)))]
               :when expression]
             [(-> k name (.replace "-" " ")) " "
              (expansion-fn expression)])
        (interpose " ")
        (apply concat)
        (apply str)))))

(defn- decode-item
  [{:keys [decode-id decode]} ^Item item]
  (->> (.getAttributes item)
    (map (fn [^Attribute a]
           (decode (.getName a) (.getValue a))))
    (reduce
      (fn [item [k v]]
        (update-in item [k] #(if % (-> % as-set (conj v)) v)))
      {::id (-> item .getName decode-id)})))

(defn query
  "Issue a query.

   If the `client-config` map contains a truthy :consistent-read? value, then the query will
   be performed with SDB's consistent read semantics.

   When `q` is a string, it is submitted directly without any interpretation.

   When `q` is a map, it is interpreted to generate a corresponding query string,
   using the configuration provided to drive attribute name and value formatting.

   The query map has mandatory keys:

     :select */id/count/[sequence-of-attrs]
     :from domain-name

   and optional keys:

     :where sexpr-based query expr supporting

        (not expr)
        (and/or/intersection expr expr)
        (=/!=/</<=/>/>=/like/not-like attr-name val)
        (null/not-null attr-name)
        (between attr-name val1 val2)
        (in attr-name #(val-set})

     :order-by [attr-name] or [attr-name asc/desc]
     :limit n

   The keys in the query map can be keywords or symbols; e.g. these two queries are
   functionally equivalent:

     '{select * from foo}
     '{:select * :from foo}

   Depending on the semantics of the encoding implemented by the provided configuration,
   attribute names and values in the query map should be specified using the same types as
   they were when those attributes were put into SDB.

   When :select is
      count - returns a number
      id - returns a sequence of ids
      * or [sequence-of-attr-names] - returns a sequence of item maps, containing all or specified attrs.

   When the return value is a sequence of items, it will have the following slots in its metadata map:

     :request-id - the ID of the request submitted to the SDB web service API
     :box-usage - a measure of the resources utilized to process the query; see the SDB documentation
                  for details.
     :next-token - an opaque string token that, when present, indicates that additional query results
                   are available.  This token can be provided as an optional third argument to `query`
                   to obtain the next chunk of results.
                   See `query-all` to get all results of a query in a single lazy seq.

   See:

      http://docs.amazonwebservices.com/AmazonSimpleDB/latest/DeveloperGuide/index.html?UsingSelect.html

   for further details of select semantics."
  ([client-config q] (query client-config q nil))
  ([client-config q next-token]
    (let [query (if (string? q)
                  q
                  (->> q
                    (map (fn [[k v]] [(-> k name keyword) v]))
                    (into {})
                    (select-string client-config)))
          request (-> query
                    (SelectRequest. (-> client-config :consistent-read? boolean))
                    (.withNextToken next-token))
          response (.select (client client-config) request)
          items (.getItems response)
          result (and (seq items)
                   (case (-> #"^\s*select\s+(count\(\*\)|itemName\(\))" (re-seq query) first second)
                     "count(*)" (-> items ^Item first .getAttributes ^Attribute first .getValue Long/valueOf)
                     "itemName()" (map #((:decode-id client-config) (.getName ^Item %)) items)
                     (map (partial decode-item client-config) items)))]
      (if-not (coll? result)
        result
        (let [response-meta (.getCachedResponseMetadata (client client-config) request)]
          (with-meta result {:box-usage (.getBoxUsage response-meta) 
                             :request-id (.getRequestId response-meta)
                             :next-token (.getNextToken response)}))))))

(defn- query-all*
  [client q next-token]
  (let [res (query client q next-token)]
    (if-let [next-token (-> res meta :next-token)]
      (concat res
        (lazy-seq (query-all* client q next-token)))
      res)))

(defn query-all
  "Returns a lazy seq of all results of the given query.  See `query` for details."
  [client q]
  (query-all* client
    ; maximize "chunk" size
    ; can't imagine when one wouldn't want to minimize round-trips when obtaining all results
    (if (map? q) (assoc q :limit 2500) q)
    nil))
