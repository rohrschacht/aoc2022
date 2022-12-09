(ql:quickload :uiop)
(ql:quickload :lisp-stat)

(defparameter *input-list*
    (map 'list (lambda (input-line)
        (map 'list (lambda (input-character)
            (digit-char-p input-character))
        (coerce input-line 'list)))
    (uiop:read-file-lines "./input.txt")))

(defparameter *input-matrix*
    (make-array (list (length *input-list*) (length (first *input-list*))) :initial-contents *input-list*))

(defun trees-up (i j matrix ls)
    (if (= -1 (- i 1))
        ls
        (trees-up (- i 1) j matrix (cons (aref matrix (- i 1) j) ls))
    ))

(defun trees-down (i j matrix ls)
    (if (= (aops:nrow matrix) (+ i 1))
        ls
        (trees-down (+ i 1) j matrix (cons (aref matrix (+ i 1) j) ls))
    ))

(defun trees-left (i j matrix ls)
    (if (= -1 (- j 1))
        ls
        (trees-left i (- j 1) matrix (cons (aref matrix i (- j 1)) ls))
    ))

(defun trees-right (i j matrix ls)
    (if (= (aops:ncol matrix) (+ j 1))
        ls
        (trees-right i (+ j 1) matrix (cons (aref matrix i (+ j 1)) ls))
    ))

(defun maximum (list)
    (if list
        (reduce #'max list)
        -1
    ))

(defun tree-visible? (i j matrix)
    (cond
        ((= i 0) t)
        ((= j 0) t)
        ((= i (- (aops:nrow matrix)) 1) t)
        ((= j (- (aops:ncol matrix)) 1) t)
        (t (cond
                ((< (maximum (trees-up i j matrix nil)) (aref matrix i j)) t)
                ((< (maximum (trees-down i j matrix nil)) (aref matrix i j)) t)
                ((< (maximum (trees-left i j matrix nil)) (aref matrix i j)) t)
                ((< (maximum (trees-right i j matrix nil)) (aref matrix i j)) t)
                (t nil)
            ))
        ))

(let ((visible-trees 0))
    (aops:each-index (i j)
        (progn
            (aref *input-matrix* i j)
            (if (tree-visible? i j *input-matrix*)
                (setf visible-trees (+ visible-trees 1)))
        )
    )
    (pprint visible-trees))
