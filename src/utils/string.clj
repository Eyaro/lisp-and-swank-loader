(in-ns 'utils.package)

(defn string-equal [#^String str1 #^String str2] (.equalsIgnoreCase str1 str2))
(defn char? [a] (instance? Character a))

