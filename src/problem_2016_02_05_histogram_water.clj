(ns cenx.olympia.problems.2016.problem-2016-02-05-histogram-water
  "Histogram Water - Olympia 2016-02-05

  Given a sequence of integers representing a histogram (bar chart),
  determine how much water that histogram will hold.  The amount of
  water that can be held above a particular bar is limited to the
  lower of the highest bar to the right and the highest bar to the
  left.  Assume a non-porous bar chart.")

(defn histogram-water
  "Imagine the histogram as a large aquarium.  We calculate the total volume contained within the outside edges
  and subtract from it the total bar volume to get our result. To calculate volume we walk from either side
  matching high edges and meeting in the middle at the highest peak."
  [s]
  (loop [histogram (vec s), total-volume 0, bar-volume 0, low-water-mark 0] ; jvm doesn't do tail recursion, so use loop
    (let [left (first histogram)
          right (peek histogram)
          len (count histogram)
          next (if (< left right) left right) ; work from the smaller edge
          remainder (if (< left right) (vec (rest histogram)) (pop histogram))]
      (if (<= len 1)
        (- (+ total-volume (- left low-water-mark)) bar-volume (or left 0))
        (if (> next low-water-mark)
          ;; raise the low-water mark and add to our total volume
          (recur remainder (+ total-volume (* (- next low-water-mark) len)) (+ bar-volume next) next)
          ;; nothing exciting found, go to next bar
          (recur remainder total-volume (+ bar-volume next) low-water-mark))))))

