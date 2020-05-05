;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname brodin-homework4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Lab 4
;; Robert Brodin - A03, Engling

;; Problem 1.

;; Student is a (make-student String String)
;; interp.
;; The name of the student, studentName, is a String
;; The email address of the student, studentEmail, is a String
;; Template:
;; (define (make-student a-student)
;;         (make-student-studentName a-student)
;;         (make-student-studentEmail a-student))
(define-struct Student (studentName studentEmail))

;; Examples of Student:
(define rob (make-Student "Rob" "rbrodin@wpi.edu"))
(define bor (make-Student "bor" "nidorbr@wpi.edu"))

;; Problem 2.

;; a ListOfStudent is one of:
;; empty
;; (cons student ListOfStudent)
;; interp.
;; ListOfStudent (a list of students) represents a list of Students.
;; Template:
;; (define (ListOfStudent a-student1 a-student2 ...)
;;         (cons a-student1 (cons a-student2 ... empty))

;; Examples of ListOfStudent:
(define studentList1 (cons bor (cons rob empty)))
(define studentList2 (cons rob (cons bor empty)))

;; Problem 3.

;; a BST is one of
;;   false
;;   (make-coursenode Number String String ListOfStudent BST BST)
;; interp. false means no BST, or an empty BST
;;         The course number, course-id, is a Number
;;         The course name, title, is a String
;;         The course instructor, instructor, is a String
;;         The students in a class, students, is a ListOfStudent
;;         The left search tree, left, is a BST
;;         The right search tree, right, is a BST
;; Invariant:
;; If a course number is less than the parent node's, the child coursenode goes in left
;; If a course number is greater than the parent node's, the child coursenode goes in the right
;; Template:
;; (define (BST coursenode)
;;    (cond[(false? coursenode) ....]
;;         [else
;;            (... (coursenode key))]))

;; Problem 5.
(define-struct coursenode (course-id title instructor students left right))

;; Problem 4.

;; The following binary tree is based off of the CS courses leveling. 
(define courseTree (make-coursenode 1.1102 "Accelerated Intro to Program Design" "Beck" studentList1
                                    (make-coursenode 1.1101 "Intro to Program Design" "Engling" studentList2 false false)
                                    (make-coursenode 1.2102 "Object Oriented" "Cuneo" studentList1 false
                                                     (make-coursenode 1.2103 "Accelerated Object Oriented" "N/A" studentList1 false
                                                                      (make-coursenode 1.2104 "Could be a class?" "N/A" studentList1 false false)))))
;; Problem 6

;; any-taught-by?: BST String -> Boolean
;; any-taught-by? takes a BST, and a name of an instructor (a String), and returns true if any of the courses in the BST
;; are taught by that instructor.
;; Stub: (define (any-taught-by? BST String) true)
;; Template:
;;(define (any-taught-by? a-tree)
;;    (cond [(boolean? a-tree) (... )];base case
;;          [(coursenode?  a-tree) (if (...)
;;                                     true
;;                                  (any-taught-by? a-tree-child instructor))]))
(define (any-taught-by? a-tree instructorName)
  (cond [(boolean? a-tree) false];base case, a possible future issue might come from this returning false
        [(coursenode? a-tree) (if(string=? instructorName (coursenode-instructor a-tree))
                                 true
                                 (or (any-taught-by? (coursenode-left a-tree) instructorName) (any-taught-by? (coursenode-right a-tree) instructorName)))]))

;; The following check-expects are to test to make sure the function any-taught-by? works.
(check-expect (any-taught-by? courseTree "Engling") true)
(check-expect (any-taught-by? courseTree "engling") false)
(check-expect (any-taught-by? courseTree "N/A") true)
 
;; Problem 7

;; The following check-expects check all conditions where remove-student-in-list might fail. 
(check-expect (remove-student-in-list "rbrodin@wpi.edu"
                                      (cons (make-Student "Tom Brodin" "tbrodin@wpi.edu") (cons (make-Student "Rob Brodin" "rbrodin@wpi.edu") empty)))
              (cons (make-Student "Tom Brodin" "tbrodin@wpi.edu") empty))
(check-expect (remove-student-in-list "tbrodin@wpi.edu"
                                      (cons (make-Student "Tom Brodin" "tbrodin@wpi.edu") (cons (make-Student "Rob Brodin" "rbrodin@wpi.edu") empty)))
              (cons (make-Student "Rob Brodin" "rbrodin@wpi.edu") empty))
(check-expect (remove-student-in-list "tbrodin@wpi.edu"
                                      (cons (make-Student "J Brodin" "jbrodin@wpi.edu") (cons (make-Student "Tom Brodin" "tbrodin@wpi.edu") (cons (make-Student "Rob Brodin" "rbrodin@wpi.edu") empty))))
              (cons (make-Student "J Brodin" "jbrodin@wpi.edu") (cons (make-Student "Rob Brodin" "rbrodin@wpi.edu") empty)))
(check-expect (remove-student-in-list "hbrodin@wpi.edu"
                                      (cons (make-Student "J Brodin" "jbrodin@wpi.edu") (cons (make-Student "Tom Brodin" "tbrodin@wpi.edu") (cons (make-Student "Rob Brodin" "rbrodin@wpi.edu") empty))))
              (cons (make-Student "J Brodin" "jbrodin@wpi.edu") (cons (make-Student "Tom Brodin" "tbrodin@wpi.edu") (cons (make-Student "Rob Brodin" "rbrodin@wpi.edu") empty))))

;; remove-student-in-list: String ListOfStudents -> ListOfStudents
;; remove-student-in-list iterates through a list of students and checks if an email matches up with a student, if it does, that student is removed from the list. The rest of the list is then returned.
;; If the email is not in the list, the entire list is returned.
;; This helper function will be used in drop-student.
;; Stub: (define (remove-student-in-list String ListOfStudents) ListOfStudents)
;; Template:
;;(define (remove-student-in-list studentEmail ListOfStudents)
;;  (cond[(empty? ListOfStudents) empty]
;;       [(cons? ListOfStudents)
;;        (if =?)
;;           (remove-student-in-list studentEmail (rest ListOfStudents))
;;           (cons (first ListOfStudents) (remove-student-in-list studentEmail (rest ListOfStudents))))]))
(define (remove-student-in-list studentEmail ListOfStudents)
  (cond[(empty? ListOfStudents) empty]
       [(cons? ListOfStudents)
        (if (string=? (Student-studentEmail (first ListOfStudents)) studentEmail)
            (remove-student-in-list studentEmail (rest ListOfStudents))
            (cons (first ListOfStudents) (remove-student-in-list studentEmail (rest ListOfStudents))))]))


;; The following check-expects check if the course-id is equal to the number given. 
(check-expect (check-student-enrollment (make-coursenode 1.1101 "CS1101" "Engling" (cons (make-Student "Rob Brodin" "rbrodin@wpi.edu") empty) empty empty) 1.1101) true) 
(check-expect (check-student-enrollment (make-coursenode 1.1100 "CS1101" "Engling" (cons (make-Student "Rob Brodin" "rbrodin@wpi.edu") empty) empty empty) 1.1101) false) 

;; check-student-enrollment: CourseNode Natural -> Boolean
;; check-student-enrollment takes a CourseNode, checks if the class matches the course, and if it does, the function returns true.
;; Otherwise, the function returns false.
;; Stub: (define (check-student-enrollment CourseNode String) true)
;; Template:
;;(define (check-student-enrollment a-CourseNode a-courseNumber
;;  (= (coursenode-number a-CourseNode) a-courseNumber))
(define (check-student-enrollment a-CourseNode a-courseNumber)
  (= (coursenode-course-id a-CourseNode) a-courseNumber))

;; drop-student: BST Natural String -> BST
;; drop-student consumes a BST, a course number, and an email address of a student, and produces a binary search tree.
;; The binary search tree that is produced removes the given student from the given class, and returns a new child node
;; Exluding the student from the class list.
;; Stub: (define (drop-student BST Natural String) BST)
;; Template:
;;(define (drop-student a-parent courseNumber emailAddress)
;;  ( (coursenode-course-id   a-parent)
;;     (coursenode-students   a-parent)
;;       (drop-student-list (parent-children a-parent))))
;; Course id, can check if the id is larger, and keep doing that.
(define (drop-student a-parent courseNumber emailAddress)
  (if(false? a-parent) false
  (if (check-student-enrollment a-parent courseNumber)
      ;; if the coursenumber matches the parent course, remove the student from the list. It is not necessary to check the invariant at this spot.
      (make-coursenode (coursenode-course-id a-parent) (coursenode-title a-parent)
                       (coursenode-instructor a-parent) (remove-student-in-list emailAddress (coursenode-students a-parent)) (coursenode-left a-parent) (coursenode-right a-parent))
      (if (< courseNumber (coursenode-course-id a-parent)) ;; Checking if the course number for the class being searched for is less than the parent's course id. If it is, ignore the right leaf, otherwise, ignore the left leaf.
          (make-coursenode (coursenode-course-id a-parent) (coursenode-title a-parent) (coursenode-instructor a-parent)
                           (coursenode-students a-parent) (drop-student (coursenode-left a-parent) courseNumber emailAddress) (coursenode-right a-parent))
          (make-coursenode (coursenode-course-id a-parent) (coursenode-title a-parent) (coursenode-instructor a-parent)
                           (coursenode-students a-parent) (coursenode-left a-parent) (drop-student (coursenode-right a-parent) courseNumber emailAddress))
          ))))


;; The following check-expects test the cases where one name is replaced, and one where an email does not exist in a class. Both check-expects run correctly.
(define dropStudentList1 (cons (make-Student "Rob Brodin" "rbrodin@wpi.edu") (cons (make-Student "Andrew Strauss" "astrauss@wpi.edu") (cons (make-Student "Tom Brodin" "tbrodin@wpi.edu") empty))))
(define dropStudentList2 (cons (make-Student "Bob Rrodin" "brbrodin@wpi.edu") (cons (make-Student "Sndrew Atrauss" "sastrauss@wpi.edu") (cons (make-Student "Bom TBrodin" "btbrodin@wpi.edu") empty))))

(define dropStudentBST (make-coursenode 1.1102 "Accelerated Intro to Program Design" "Beck" dropStudentList1
                                    (make-coursenode 1.1101 "Intro to Program Design" "Engling" dropStudentList2 false false)
                                    (make-coursenode 1.2103 "Accelerated Object Oriented" "Beck"  dropStudentList1
                                                     (make-coursenode 1.2102 "Object Oriented" "Cuneo" dropStudentList1 false false)
                                                                      (make-coursenode 1.2104 "Could be a class?" "N/A" dropStudentList1 false false))))

(check-expect (drop-student dropStudentBST 1.1101 "emailthatdoesntexist@wpi.edu") dropStudentBST)

(check-expect (drop-student dropStudentBST 1.2104 "astrauss@wpi.edu") (make-coursenode 1.1102 "Accelerated Intro to Program Design" "Beck" dropStudentList1
                                    (make-coursenode 1.1101 "Intro to Program Design" "Engling" dropStudentList2 false false)
                                    (make-coursenode 1.2103 "Accelerated Object Oriented" "Beck"  dropStudentList1
                                                     (make-coursenode 1.2102 "Object Oriented" "Cuneo" dropStudentList1 false false)
                                                                      (make-coursenode 1.2104 "Could be a class?" "N/A" (cons (make-Student "Rob Brodin" "rbrodin@wpi.edu") (cons (make-Student "Tom Brodin" "tbrodin@wpi.edu") empty)) false false))))

;; Problem 8

;; There is only one check-expect necessary to test the functionality of list-titles-in-order-by-coursenum
(check-expect (list-titles-in-order-by-coursenum dropStudentBST)
              (list  "Intro to Program Design" "Accelerated Intro to Program Design" "Object Oriented" "Accelerated Object Oriented" "Could be a class?"))

;; list-titles-in-order-by-coursenum: BST -> ListOfStrings
;; list-titles-in-order-by-coursenum consumes a binary search tree and produces a list of titles of the courses, in order by ascending course number.
;; Stub: (define (list-titles-in-order-coursenum BST) ListOfStrings)
;; Template:
;; (define (list-titles-in-order-by-coursenum a-BST)
;;  (cond [(empty? a-BST) empty]   ;; Base Case
;;        [else
;;         (append (first child a-BST)      ;; First condition
;;              (list-titles-in-order-by-coursenum (rest child a-BST)))]))
(define (list-titles-in-order-by-coursenum a-parent)
  (cond[(false? a-parent) empty]
       [else
        (append (list-titles-in-order-by-coursenum (coursenode-left a-parent)) (cons (coursenode-title a-parent) empty) (list-titles-in-order-by-coursenum (coursenode-right a-parent)))]))

;; This problem is somewhat difficult because of the sorting part.

;; Problem 9

;; The following check-expects test different cases of adding classes, and where those specific classes should be added. 
(check-expect (add-course dropStudentBST 1.1103 "The forgotten intro class" "*shrug*")
              (make-coursenode 1.1102 "Accelerated Intro to Program Design" "Beck" dropStudentList1
                                    (make-coursenode 1.1101 "Intro to Program Design" "Engling" dropStudentList2 false false)
                                    (make-coursenode 1.2103 "Accelerated Object Oriented" "Beck"  dropStudentList1 
                                                     (make-coursenode 1.2102 "Object Oriented" "Cuneo" dropStudentList1 (make-coursenode 1.1103 "The forgotten intro class" "*shrug*" empty false false) false)
                                                                      (make-coursenode 1.2104 "Could be a class?" "N/A" dropStudentList1 false false))))

(check-expect (add-course dropStudentBST 1.000 "The forgotten introduction class" "*shrug*")
              (make-coursenode 1.1102 "Accelerated Intro to Program Design" "Beck" dropStudentList1
                                    (make-coursenode 1.1101 "Intro to Program Design" "Engling" dropStudentList2 (make-coursenode 1.000 "The forgotten introduction class" "*shrug*" empty false false) false)
                                    (make-coursenode 1.2103 "Accelerated Object Oriented" "Beck"  dropStudentList1 
                                                     (make-coursenode 1.2102 "Object Oriented" "Cuneo" dropStudentList1 false false)
                                                                      (make-coursenode 1.2104 "Could be a class?" "N/A" dropStudentList1 false false))))

(check-expect (add-course dropStudentBST 1.2106 "The second forgotten introduction class" "insert_professor")
              (make-coursenode 1.1102 "Accelerated Intro to Program Design" "Beck" dropStudentList1
                                    (make-coursenode 1.1101 "Intro to Program Design" "Engling" dropStudentList2 false false)
                                    (make-coursenode 1.2103 "Accelerated Object Oriented" "Beck"  dropStudentList1 
                                                     (make-coursenode 1.2102 "Object Oriented" "Cuneo" dropStudentList1 false false)
                                                                      (make-coursenode 1.2104 "Could be a class?" "N/A" dropStudentList1 false (make-coursenode  1.2106 "The second forgotten introduction class" "insert_professor" empty false false)))))
;; add-course: BST Natural String String -> BST
;; add-course takes a BST, a course number, a course title, and the name of an instructor.
;; A new binary search tree is created with a new course at the appropriate end of the tree (based on the invariant)
;; Stub: (define (add-course BST Natural String String) dropStudentBST)
;; Template:
;;(define (add-course a-parent courseNumber courseTitle courseInstructor)
;;  ( (coursenode-course-id   a-parent)
;;     (coursenode-students   a-parent)
;;     (coursenode-title  a-parent)
;;     (coursenode-instructor   a-parent)
;;       (add-course (parent-children a-parent))))
(define (add-course a-parent courseNumber courseTitle courseInstructor)
  (cond[(false? a-parent) (make-coursenode courseNumber courseTitle courseInstructor empty false false)]
       [(< courseNumber (coursenode-course-id a-parent))
          (make-coursenode (coursenode-course-id a-parent) (coursenode-title a-parent)
                           (coursenode-instructor a-parent) (coursenode-students a-parent) (add-course (coursenode-left a-parent) courseNumber courseTitle courseInstructor) (coursenode-right a-parent))]
       [(> courseNumber (coursenode-course-id a-parent))
          (make-coursenode (coursenode-course-id a-parent) (coursenode-title a-parent)
                           (coursenode-instructor a-parent) (coursenode-students a-parent) (coursenode-left a-parent) (add-course (coursenode-right a-parent) courseNumber courseTitle courseInstructor))]))

  
