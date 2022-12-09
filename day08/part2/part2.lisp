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

(defun view-up (i j height matrix view)
    (if (= -1 (- i 1))
        view
        (if (>= (aref matrix (- i 1) j) height)
            (+ view 1)
            (view-up (- i 1) j height matrix (+ view 1))
        )
    ))

(defun view-down (i j height matrix view)
    (if (= (aops:nrow matrix) (+ i 1))
        view
        (if (>= (aref matrix (+ i 1) j) height)
            (+ view 1)
            (view-down (+ i 1) j height matrix (+ view 1))
        )
    ))

(defun view-left (i j height matrix view)
    (if (= -1 (- j 1))
        view
        (if (>= (aref matrix i (- j 1)) height)
            (+ view 1)
            (view-left i (- j 1) height matrix (+ view 1))
        )
    ))

(defun view-right (i j height matrix view)
    (if (= (aops:ncol matrix) (+ j 1))
        view
        (if (>= (aref matrix i (+ j 1)) height)
            (+ view 1)
            (view-right i (+ j 1) height matrix (+ view 1))
        )
    ))

(defun maximum (list)
    (if list
        (reduce #'max list)
        -1
    ))

(defun scenic-score (i j matrix)
    (*
        (view-up i j (aref matrix i j) matrix 0)
        (view-down i j (aref matrix i j) matrix 0)
        (view-left i j (aref matrix i j) matrix 0)
        (view-right i j (aref matrix i j) matrix 0)
    ))

(let ((highest-scenic-score 0))
    (aops:each-index (i j)
        (progn
            (aref *input-matrix* i j)
            (let ((score (scenic-score i j *input-matrix*)))
                (if (> score highest-scenic-score)
                    (setf highest-scenic-score score))
            )
        )
    )
    (pprint highest-scenic-score))
