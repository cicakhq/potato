;;; Source: https://gist.github.com/vishnuvyas/958488
;;; A purely functional implementation of the levenshtien distance in clojure
(ns potato.levenshtein)

(defn- compute-next-row
  "computes the next row using the prev-row current-element and the other seq"
  [prev-row current-element other-seq pred]
  (reduce
    (fn [row [diagonal above other-element]]
      (let [update-val
            (if (pred other-element current-element)
              ;; if the elements are deemed equivalent according to the predicate
              ;; pred, then no change has taken place to the string, so we are
              ;; going to set it the same value as diagonal (which is the previous edit-distance)
              diagonal

              ;; in the case where the elements are not considered equivalent, then we are going
              ;; to figure out if its a substitution (then there is a change of 1 from the previous
              ;; edit distance) thus the value is diagonal + 1 or if its a deletion, then the value
              ;; is present in the columns, but not in the rows, the edit distance is the edit-distance
              ;; of last of row + 1 (since we will be using vectors, peek is more efficient)
              ;; or it could be a case of insertion, then the value is above+1, and we chose
              ;; the minimum of the three
              (inc (min diagonal above (peek row))))]
        (conj row update-val)))
    ;; we need to initialize the reduce function with the value of a row, since we are
    ;; constructing this row from the previous one, the row is a vector of 1 element which
    ;; consists of 1 + the first element in the previous row (edit distance between the prefix so far
    ;; and an empty string)
    [(inc (first prev-row))]

    ;; for the reduction to go over, we need to provide it with three values, the diagonal
    ;; which is the same as prev-row because it starts from 0, the above, which is the next element
    ;; from the list and finally the element from the other sequence itself.
    (map vector prev-row (next prev-row) other-seq)))

(defn distance
  "Levenshtein Distance - http://en.wikipedia.org/wiki/Levenshtein_distance
  In information theory and computer science, the Levenshtein distance is a metric for measuring the amount of difference between two sequences. This is a functional implementation of the levenshtein edit distance with as little mutability as possible.
  Still maintains the O(n*m) guarantee."
  [a b & {p :predicate :or {p =}}]
  (peek
   (reduce
    ;; we use a simple reduction to convert the previous row into the next-row  using the
    ;; compute-next-row which takes a current element, the previous-row computed so far
    ;; and the predicate to compare for equality.
    (fn [prev-row current-element]
      (compute-next-row prev-row current-element b p))
    ;; we need to initialize the prev-row with the edit distance between the various prefixes of
    ;; b and the empty string.
    (map #(identity %2) (cons nil b) (range)) a)))
