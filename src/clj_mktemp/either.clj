(ns clj-mktemp.either)

(defrecord Left [kind value])
(defrecord Right [kind value])

(def ^:dynamic *either-hierarchy*
  (-> (make-hierarchy)
      (derive Left ::either)
      (derive Right ::either)))

(defn right [x]
  (Right. :right x))

(defn left [x]
  (Left. :left x))

(defmulti emap (fn [kind f fa] [kind (class fa)]))

(defmethod emap [:right Right] [kind f fa]
  (right (f (:value fa))))

(defmethod emap [:left Left] [kind f fa]
  (left (f (:value fa))))

(defmethod emap [:right Left] [kind f fa]
  fa)

(defmethod emap [:left Right] [kind f fa]
  fa)

