(defun d1p1 (p)
  (setq n
	(parse-integer
	 (read-line nil nil "-1")))
  (if (= -1 n)
      0
    (+ (if (> n p) 1 0) (d1p1 n))))
(write (- (d1p1 0) 1))
