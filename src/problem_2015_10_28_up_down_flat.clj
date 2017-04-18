(ns cenx.olympia.problems.2015.problem-2015-10-28-up-down-flat
  "Up Down Flat - Olympia 2015-10-28

  Given a string of any length that consists of 5 possible characters,
  generate a string that, when printed, goes up and down according to
  the following rules:

  ^ Up one
  ▲ Up two
  v Down one
  ▼ Down two
  - Flat

  Example (see tests for more examples):

  ^^▲-^v▼▲^-^v will generate:

            ^
      ^   ^- v
    ▲- v ▲

   ^    ▼
  ^

  Notes:
  - Use \n for line breaks
  - Extra line breaks at beginning and end are not allowed
  - Trailing whitespace is not allowed

  Inspired by:
  codegolf.stackexchange.com/questions/58759/this-question-has-its-ups-and-downs
  (golfing not required here, but swearjure could be cool)")



(declare code->paths path->output)

(defn up-down-flat
  ;;
  ;; A couple of playful ideas:  Let a string of the form 'v▼▲^-' be called an 'updown code' and the
  ;; decoded internal representation be an 'updown path'. The 'path' contains a matrix which can be printed
  ;; to produce the desired output.
  ;;
  ;; To embelish the problem a bit let us treat decoding part as complex task worthy of parrallelization.
  ;; One method is to divide and conquer the updown code, much like you would process a string
  ;; in a mergesort.
  ;;
  ;;                           ^^▲-^v▼▲^-^v
  ;;                           /           \
  ;;                       ^^▲-^v         ▼▲^-^v
  ;;                      /      \        /     \
  ;;                    ^^▲      -^v     ▼▲^    -^v
  ;;                   /  \     /  \    /  \    /  \
  ;;                  ^^   ▲   -^   v  ▼▲   ^  -^   v
  ;;                 / \      / \     / \     / \
  ;;                ^   ^    -   ^   ▼   ▲   -   ^
  ;;
  ;; The neat thing about this approach is that encountering the same substring is somewhat likely, so
  ;; may be some value in caching their solutions in a lookup table for re-use later. It is worth noting
  ;; that it doesn't make sense to cache a result for a string longer then 1/2 the original updown code,
  ;; (since it could never be re-used) but this is naturally avoided by virtue of our binary partitioning.
  ;;
  [s]
  (if (= 1 (count s)) s (path->output ((code->paths s) s))))

;;; utility functions

(defn ceil [n] (int (+ n 0.5)))

(defn after
  [pred coll]
  (when (seq coll)
    (if (pred (first coll)) coll (recur pred (rest coll)))))

(defn before
  [pred coll]
  (reverse (after pred (reverse coll))))

(defn path-join
  "Join path p1 to path p2"
  ;;
  ;; ^▼      {:start 1, :finish 2, :len 2 :matrix ["^", "", " ▼"]}
  ;; v▲▲^▼   {:start 4, :finish 2, :len 5 :matrix ["   ^", "  ▲", "    ▼", " ▲", "", "v"]}
  ;; ^▼      {:start 2, :finish 2, :len 2 :matrix ["^", "", " ▼"]}
  ;; -       {:start 0, :finish 0, :len 1 :matrix ["-"]}
  ;; ^       {:start 1, :finish 0, :len 1 :matrix ["^", ""]}
  ;;
  ;;      ---
  ;;     | 0 |
  ;;      --- ---
  ;;     | 1 | 0 |
  ;;  --- --- ---     ---
  ;; | 0 | 2>|>1 |   | 0>|
  ;;  --- --- --- --- ---
  ;; |>1 | 3 | 2>|>0>|>1 |
  ;;  --- --- --- --- ---
  ;; | 2>|>4 |
  ;;  --- ---
  ;;     | 5 |
  ;;      ---
  ;;
  [p1 p2]
  (let [p1-row-count (count (:matrix p1))
        p2-row-count (count (:matrix p2))
        row-count (max p1-row-count p2-row-count)
        p1-offset (min (- (:finish p1) (:start p2)) 0)
        p2-offset (+ p1-offset (- (:start p2) (:finish p1)))]
    (loop [p1-row p1-offset
           p2-row p2-offset
           exciting-new-matrix []]
      (let [p1-val (if
                     ;; does p1 contain any data in this range?
                     (and (>= p1-row 0) (< p1-row p1-row-count))
                     ;; get the row from the matrix
                     ((:matrix p1) p1-row)
                     ;; just a placeholder, this path doesn't have data for this row
                     "")
            p2-val (if (and (>= p2-row 0) (< p2-row p2-row-count)) ((:matrix p2) p2-row) "")]
        (if (or (< p1-row row-count) (< p2-row row-count))
          (recur (inc p1-row)
                 (inc p2-row)
                 (conj exciting-new-matrix
                       (if (empty? p2-val)
                         p1-val
                         ;; add leading whitespace when joining an empty first 'column' to one that contains data
                         (apply str [p1-val (apply str (repeat (- (:len p1) (count p1-val)) " ")) p2-val]))))

          {:start (- (:start p1) p1-offset)
           :finish (- (:finish p2) p2-offset)
           :len (+ (:len p1) (:len p2))
           :matrix exciting-new-matrix})))))

(defn- code->paths
  "Decode the updown sequence into a lookup table containing its path as well as paths for any
  sub-sequences calculated along the way."
  ([updown-code]
   (code->paths updown-code {"▲" {:start 2, :finish 0, :len 1 :matrix ["▲", "", ""]},
                             "^" {:start 1, :finish 0, :len 1 :matrix ["^", ""]},
                             "-" {:start 0, :finish 0, :len 1 :matrix ["-"]},
                             "v" {:start 0, :finish 1, :len 1 :matrix ["", "v"]},
                             "▼" {:start 0, :finish 2, :len 1 :matrix ["", "", "▼"]}}))
  ([updown-code boring-old-paths]
   (if-not (get boring-old-paths updown-code)
     (let [code-midpoint (ceil (/ (count updown-code) 2))
           first-half (subs updown-code 0 code-midpoint)
           second-half (subs updown-code code-midpoint)
           ;; why all the extra steps?  So we can have some fun caching solutions to substrings re-using them!
           shiny-new-paths (->> boring-old-paths (code->paths first-half) (code->paths second-half))]
       (conj shiny-new-paths
             {updown-code (path-join (get shiny-new-paths first-half) (get shiny-new-paths second-half))}))
     boring-old-paths)))

(defn path->output
  "Convert path into its printable form"
  [path]
  (->> path :matrix (after not-empty) (before not-empty) (clojure.string/join "\n")))
