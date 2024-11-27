<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 5</b><br/>
"Робота з базою даних"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Гармаш Дмитро Олегович КВ-13</p>
<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання
В роботі необхідно реалізувати утиліти для роботи з базою даних, заданою за варіантом (п. 5.1.1). База даних складається з кількох таблиць. Таблиці представлені у вигляді CSV файлів. При зчитуванні записів з таблиць, кожен запис має бути представлений певним типом в залежності від варіанту: структурою, асоціативним списком або геш-таблицею.
1. Визначити структури або утиліти для створення записів з таблиць (в залежності від типу записів, заданого варіантом).
2. Розробити утиліту(-и) для зчитування таблиць з файлів.
3. Розробити функцію select , яка отримує на вхід шлях до файлу з таблицею, а також якийсь об'єкт, який дасть змогу зчитати записи конкретного типу або структури. Це може бути ключ, список з якоюсь допоміжною інформацією, функція і
т. і. За потреби параметрів може бути кілька. select повертає лямбда-вираз, який, в разі виклику, виконує "вибірку" записів з таблиці, шлях до якої було передано у select . При цьому лямбда-вираз в якості ключових параметрів може отримати на вхід значення полів записів таблиці, для того щоб обмежити вибірку лише заданими значеннями (виконати фільтрування). Вибірка повертається у вигляді списку записів.
4. Написати утиліту(-и) для запису вибірки (списку записів) у файл.
5. Написати функції для конвертування записів у інший тип (в залежності від
варіанту):
* структури у геш-таблиці
* геш-таблиці у асоціативні списки
* асоціативні списки у геш-таблиці
6. Написати функцію(-ї) для "красивого" виводу записів таблиці.


## Варіант 3
* База даних - Виробництво дронів
* Тип записів - Асоціативний список
* Таблиці - Виробники дронів, Дрони 
* Опис - База даних виробників дронів та, власне, дронів

## Лістинг реалізації завдання
```lisp
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
```
### Тестові набори та утиліти
```lisp
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

```
### Вміст тестових файлів №1 (Manufacturers.csv)
```
ManufacturerID,ManufacturerName,Country,FoundedYear,Specialization
1,DJI,China,2006,Consumer Drones
2,Parrot,France,1994,Agricultural and Consumer Drones
3,Autel Robotics,USA,2014,Professional Drones
4,Skydio,USA,2014,Autonomous Drones
```
### Вміст тестових файлів №2 (Drones.csv)
```
DroneID,DroneName,ManufacturerID,MaxFlightTimeMinutes,PriceUSD,Usage
101,Mavic Air 2,1,34,799,Photography
102,Anafi,2,25,699,Photography
103,EVO II,3,40,1495,Surveying
104,Skydio 2+,4,27,1099,Autonomous Mapping
105,Phantom 4 Pro,1,30,1499,Photography
```
### Тестування
```lisp
Manufacturers data:
                    Key | Value
------------------------+---------------------------------------------------
   (MANUFACTURERID . 1) | 
                           - MANUFACTURERNAME: DJI
                           - COUNTRY: China
                           - FOUNDEDYEAR: 2006
                           - SPECIALIZATION: Consumer Drones
   (MANUFACTURERID . 2) | 
                           - MANUFACTURERNAME: Parrot
                           - COUNTRY: France
                           - FOUNDEDYEAR: 1994
                           - SPECIALIZATION: Agricultural and Consumer Drones
   (MANUFACTURERID . 3) | 
                           - MANUFACTURERNAME: Autel Robotics
                           - COUNTRY: USA
                           - FOUNDEDYEAR: 2014
                           - SPECIALIZATION: Professional Drones
   (MANUFACTURERID . 4) | 
                           - MANUFACTURERNAME: Skydio
                           - COUNTRY: USA
                           - FOUNDEDYEAR: 2014
                           - SPECIALIZATION: Autonomous Drones
Drones data:
               Key | Value
-------------------+---------------------------------------------------
   (DRONEID . 101) | 
                      - DRONENAME: Mavic Air 2
                      - MANUFACTURERID: 1
                      - MAXFLIGHTTIMEMINUTES: 34
                      - PRICEUSD: 799
                      - USAGE: Photography
   (DRONEID . 102) | 
                      - DRONENAME: Anafi
                      - MANUFACTURERID: 2
                      - MAXFLIGHTTIMEMINUTES: 25
                      - PRICEUSD: 699
                      - USAGE: Photography
   (DRONEID . 103) | 
                      - DRONENAME: EVO II
                      - MANUFACTURERID: 3
                      - MAXFLIGHTTIMEMINUTES: 40
                      - PRICEUSD: 1495
                      - USAGE: Surveying
   (DRONEID . 104) | 
                      - DRONENAME: Skydio 2+
                      - MANUFACTURERID: 4
                      - MAXFLIGHTTIMEMINUTES: 27
                      - PRICEUSD: 1099
                      - USAGE: Autonomous Mapping
   (DRONEID . 105) | 
                      - DRONENAME: Phantom 4 Pro
                      - MANUFACTURERID: 1
                      - MAXFLIGHTTIMEMINUTES: 30
                      - PRICEUSD: 1499
                      - USAGE: Photography
USA Manufacturers:
                    Key | Value
------------------------+---------------------------------------------------
   (MANUFACTURERID . 3) | 
                           - MANUFACTURERNAME: Autel Robotics
                           - COUNTRY: USA
                           - FOUNDEDYEAR: 2014
                           - SPECIALIZATION: Professional Drones
   (MANUFACTURERID . 4) | 
                           - MANUFACTURERNAME: Skydio
                           - COUNTRY: USA
                           - FOUNDEDYEAR: 2014
                           - SPECIALIZATION: Autonomous Drones
Drones for photography:
               Key | Value
-------------------+---------------------------------------------------
   (DRONEID . 101) | 
                      - DRONENAME: Mavic Air 2
                      - MANUFACTURERID: 1
                      - MAXFLIGHTTIMEMINUTES: 34
                      - PRICEUSD: 799
                      - USAGE: Photography
   (DRONEID . 102) | 
                      - DRONENAME: Anafi
                      - MANUFACTURERID: 2
                      - MAXFLIGHTTIMEMINUTES: 25
                      - PRICEUSD: 699
                      - USAGE: Photography
   (DRONEID . 105) | 
                      - DRONENAME: Phantom 4 Pro
                      - MANUFACTURERID: 1
                      - MAXFLIGHTTIMEMINUTES: 30
                      - PRICEUSD: 1499
                      - USAGE: Photography
Drone as hash table:
DRONEID => 101
DRONENAME => Mavic Air 2
MANUFACTURERID => 1
MAXFLIGHTTIMEMINUTES => 34
PRICEUSD => 799
USAGE => Photography
DRONEID => 101
DRONENAME => Mavic Air 2
MANUFACTURERID => 1
MAXFLIGHTTIMEMINUTES => 34
PRICEUSD => 799
USAGE => Photography
Contents of Drones.csv:
DroneID,DroneName,ManufacturerID,MaxFlightTimeMinutes,PriceUSD,Usage
101,Mavic Air 2,1,34,799,Photography
102,Anafi,2,25,699,Photography
103,EVO II,3,40,1495,Surveying
104,Skydio 2+,4,27,1099,Autonomous Mapping
105,Phantom 4 Pro,1,30,1499,Photography
101,Mavic Air 2,1,34,799,Photography
```
