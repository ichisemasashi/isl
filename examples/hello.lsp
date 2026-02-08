(defglobal x 10)

(defun fact (n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(print (fact x))
