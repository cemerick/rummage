(ns cemerick.rummage.encoding
  (:import cemerick.rummage.DataUtils)
  (:require [cemerick.utc-dates :as dates])
  (:refer-clojure :exclude (identity)))

(defn strip-symbol-ns
  "If `s` is a namespaced symbol, returns a new symbol with no namespace, but the same name.
   Otherwise, returns `s` unchanged."
  [s]
  (if (and (symbol? s) (namespace s))
    (-> s name symbol)
    s))

(defn identity
  ([name] name)
  ([name value] [name value]))

;; thought about making this a protocol, but it's all just too concise & clear as regular fns

(def all-strings
  {:encode-id str
   :decode-id identity
   :encode (fn
             ([name] (str name))
             ([name value] (map str [name value])))
   :decode identity})

(defn- assert-keyword
  [k require-namespace?]
  (when-not (keyword? k)
    (throw (IllegalArgumentException. (str "Unsupported key type, expected keyword: " (pr-str k)))))
  (when (and require-namespace? (not (namespace k)))
    (throw (IllegalArgumentException. (format "Encountered %s key, expected namespaced keyword." k)))))

(def keyword-strings
  (assoc all-strings
    :encode (fn encode
              ([k]
                (assert-keyword k false)
                (subs (str k) 1))
              ([k v] [(encode k) (str v)]))
    :decode (fn
              ([k] (keyword k))
              ([k v] [(keyword k) v]))))

(defn from-prefixed-string
  "Reproduces the representation of the item from a string created by to-prefixed-string"
  ([formatting ^String s]
    (let [[[_ prefix value-str]] (re-seq #"(?s)([^:]+):(.*)" s)]
      (when (not prefix)
        (throw (IllegalArgumentException.
                 (format "Cannot decode (%s...), no prefix found"
                   (.substring s 0 (min 10 (count s)))))))
      (from-prefixed-string formatting value-str prefix)))
  ([formatting ^String value-str prefix]
    (let [formatter (get formatting prefix)]
      (if formatter
        ((:decode formatter) value-str)
        (throw (IllegalArgumentException.
                 (format "Cannot decode (%s...), no formatter found for prefix %s"
                   (.substring value-str 0 (min 10 (count value-str)))
                   prefix)))))))

(defn to-prefixed-string
  ([formatting v]
    (let [type (class v)
          {:keys [prefix encode]} (get formatting type)]
      (if-not prefix
        (throw (IllegalArgumentException. (str "No formatter available for value of type " type)))
        (str prefix ":" (encode v)))))
  ([formatting v prefix]
    (if-let [{:keys [encode]} (get formatting prefix)]
      (encode v)
      (throw (IllegalArgumentException. (str "No formatter available for prefix " prefix))))))

(def ^{:doc "The largest absolute value integer that can be encoded (/ Long/MAX_VALUE 2)."}
  max-abs-integer (Long. (long (/ Long/MAX_VALUE 2))))

(defn encode-integer
  [i]
  (when (> (Math/abs (long i)) max-abs-integer)
    (throw (IllegalArgumentException. (format "encode-integer can support only integers between %s and -%s" max-abs-integer max-abs-integer))))
  (DataUtils/encodeRealNumberRange (long i) (int 19) (long max-abs-integer)))

(defn decode-integer
  [istr]
  (DataUtils/decodeRealNumberRangeLong istr max-abs-integer))

(defn encode-float
  [f]
  (DataUtils/encodeDouble f))

(defn decode-float
  [fstr]
  (DataUtils/decodeDouble fstr))

(def prefix-formatting
  (let [base [[String "s" identity identity]
              [clojure.lang.Keyword "k" #(subs (str %) 1) keyword]
              [Long "i" encode-integer decode-integer]
              [Double "f" encode-float decode-float]
              [Boolean "z" str #(condp = %, "true" true, "false" false)]
              [java.util.Date "D" dates/format dates/parse]
              [java.net.URL "U" str #(java.net.URL. %)]]
        base (->> base
      (map (partial zipmap [:class :prefix :encode :decode]))
      (reduce
        (fn [m {:keys [class prefix] :as formatter}]
          (assoc m
            class formatter
            prefix formatter)) {}))]
    (assoc base
      Integer (assoc (base Long) :class Integer)
      Float (assoc (base Double) :class Float))))

(def prefixed-id-formatting
  {:encode-id (partial to-prefixed-string prefix-formatting)
   :decode-id (partial from-prefixed-string prefix-formatting)})

(defn all-prefixed-config
  ([] (all-prefixed-config prefix-formatting))
  ([prefix-formatting]
    (assoc prefixed-id-formatting
      :encode (fn
                ([k] (to-prefixed-string prefix-formatting k))
                ([k v]
                  (map #(to-prefixed-string prefix-formatting %) [k v])))
      :decode (fn
                ([k] (from-prefixed-string prefix-formatting k))
                ([k v]
                  (map #(from-prefixed-string prefix-formatting %) [k v]))))))

(defn name-typed-values-config
  ([] (name-typed-values-config prefix-formatting))
  ([prefix-formatting]
    (assoc prefixed-id-formatting
      :encode (fn encode
                ([k]
                  (assert-keyword k true)
                  (to-prefixed-string prefix-formatting k))
                ([k v]
                  [(encode k)
                   (to-prefixed-string prefix-formatting v (namespace k))]))
      :decode (fn decode
                ([k] (from-prefixed-string prefix-formatting k))
                ([k v]
                  (let [k (decode k)
                        ns (namespace k)]
                    [k (from-prefixed-string prefix-formatting v ns)]))))))

(defn fixed-domain-schema
  ([name-type-map]
    (fixed-domain-schema name-type-map prefix-formatting))
  ([name-type-map formatters]
    (let [name-formatter-map (into {} (for [[name type] name-type-map]
                                        (if-let [formatter (get formatters type)]
                                          [name formatter]
                                          (throw (IllegalArgumentException. (str "No formatter available for type " type))))))]
      (assoc prefixed-id-formatting
        :encode (fn encode
                  ([k] (to-prefixed-string formatters k))
                  ([k v]
                    [(encode k)
                     (to-prefixed-string name-formatter-map v k)]))
        :decode (fn decode
                  ([k] (from-prefixed-string formatters k))
                  ([k v]
                    (let [k (decode k)]
                      [k (from-prefixed-string name-formatter-map v k)])))))))
