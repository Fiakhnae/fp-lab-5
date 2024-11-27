(defun split-string (string separator)
  (loop with result = nil
        with length = (length string)
        for start = 0 then (1+ end)
        for end = (position separator string :start start)
        while (and end (< start length))
        do (push (subseq string start end) result)
        finally (push (subseq string start) result)
          (return (nreverse result))))

(defun parse-csv-line (line)
  (split-string line #\,)) 

(defun read-csv-to-alist (file-path)
  (with-open-file (stream file-path :direction :input)
    (let* ((lines (loop for line = (read-line stream nil nil)
                        while line collect line))
           (headers (parse-csv-line (first lines))) 
           (data-lines (rest lines))) 
      (mapcar (lambda (line)
                (let ((values (parse-csv-line line)))
                  (loop for header in headers
                        for value in values
                        collect (cons (intern (string-upcase header) :keyword)
                                      (string-trim '(#\Space #\") value)))))
          data-lines))))

(defun select (file-path &rest filters)
  (let ((data (read-csv-to-alist file-path))) 
    (if (null filters)
        data
        (let ((filter-pairs
               (loop for (key value) on filters by #'cddr
                     collect (cons key value)))) 
          (remove-if-not (lambda (row)
                           (every (lambda (filter)
                                    (equal (cdr filter) (cdr (assoc (car filter) row))))
                               filter-pairs))
              data)))))

(defun write-alist-to-csv (file-path alists &optional write-headers)
  (with-open-file (stream file-path :direction :output :if-exists :append :if-does-not-exist :create)
    (when write-headers
          (let ((headers (mapcar #'car (first alists)))) 
            (write-line
              (reduce (lambda (a b) (concatenate 'string a "," b))
                  (mapcar #'symbol-name headers))
              stream)))
    (dolist (alist alists)
      (let ((line
             (reduce (lambda (a b) (concatenate 'string a "," b))
                 (mapcar (lambda (header)
                           (or (cdr (assoc header alist)) ""))
                     (mapcar #'car (first alists))))))
        (write-line line stream)))))

(defun alist-to-hash-table (alist)
  (let ((hash (make-hash-table :test 'equal)))
    (dolist (pair alist)
      (setf (gethash (car pair) hash) (cdr pair)))
    hash))

(defun pretty-print-alist (alist)
  (let ((key-width (reduce #'max (mapcar (lambda (pair) (length (prin1-to-string (car pair)))) alist))))
    (format t "~%~v@a | Value~%" key-width "Key")
    (format t "~a-+-~a~%" (make-string key-width :initial-element #\-)
      (make-string 50 :initial-element #\-)) 
    (dolist (pair alist)
      (let ((key (car pair))
            (value (cdr pair)))
        (if (and (listp value) (listp (car value))) 
            (progn
             (format t "~v@a | ~%" key-width key) 
             (pretty-print-nested-alist value (+ key-width 4))) 
            (format t "~v@a | ~a~%" key-width key value)))))) 

(defun pretty-print-nested-alist (nested-alist indent)
  (dolist (pair nested-alist)
    (format t "~v@a- ~a: ~a~%" indent " " (car pair) (cdr pair))))

(defun test-read-display-data ()
  (let ((manufacturers (select "Manufacturers.csv"))
        (drones (select "Drones.csv")))
    (format t "Manufacturers data:~%")
    (pretty-print-alist manufacturers)
    (format t "~%Drones data:~%")
    (pretty-print-alist drones)))

(defun test-filter-data ()
  (let ((usa-manufacturers (select "Manufacturers.csv" :Country "USA"))
        (photography-drones (select "Drones.csv" :Usage "Photography")))
    (format t "USA Manufacturers:~%")
    (pretty-print-alist usa-manufacturers)
    (format t "~%Drones for photography:~%")
    (pretty-print-alist photography-drones)))

(defun test-data-transformation ()
  (let* ((drones (select "Drones.csv"))
         (drone (first drones)) 
         (hash (alist-to-hash-table drone)))
    (format t "Drone as hash table:~%")
    (maphash (lambda (key value)
               (format t "~a => ~a~%" key value))
             hash)
    (dolist (key '(:DRONEID :DRONENAME :MANUFACTURERID :MAXFLIGHTTIMEMINUTES :PRICEUSD :USAGE))
      (format t "~a => ~a~%" key (gethash key hash)))))

(defun test-add-drone ()
  (let ((data '(((:DRONEID . "101") (:DRONENAME . "Mavic Air 2") (:MANUFACTURERID . "1") (:MAXFLIGHTTIMEMINUTES . "34") (:PRICEUSD . "799") (:USAGE . "Photography")))))
    (write-alist-to-csv "Drones.csv" data))
  (with-open-file (stream "Drones.csv" :direction :input)
    (format t "Contents of ~a:~%" "Drones.csv")
    (loop for line = (read-line stream nil)
          while line
          do (format t "~a~%" line))))

(defun run-all-tests ()
  (test-read-display-data)
  (test-filter-data)
  (test-data-transformation)
  (test-add-drone))

(run-all-tests)
