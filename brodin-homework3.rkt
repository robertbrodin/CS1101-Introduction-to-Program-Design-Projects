;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname brodin-homework3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
 ;; Robert Brodin - Lab 3/Homework 3
;; A03 - Engling

;; Problem 1

;; menu-item is a (make-menu-item String Natural Natural String) where:
;; interp.
;; The type of food, foodType, is a String
;; The number of calories for that item (singular), calorieNum, is a Natural
;; The quantity of that item for the specific order, quantity, is a Natural
;; Any special orders that the order has, specialOrder, is a String
;; Template (Problem 2):
;; (define (menu-item a-menu-item)
;;         (menu-item-foodType a-menu-item)
;;         (menu-item-calorieNum a-menu-item)
;;         (menu-item-price a-menu-item)
;;         (menu-item-quantity a-menu-item)
;;         (menu-item-specialOrder a-menu-item))
(define-struct menu-item (foodType calorieNum price quantity specialOrder))

;; Examples of menu-item:
;; Ten normal salads, with no special orders:
(define coke (make-menu-item "coke" 10 100 10 ""))
;; One vegan pizza, hold the cheese.
(define pizza (make-menu-item "pizza" 180 10 1 "vegan"))

;; Problem 1

;; an Order is one of:
;; empty
;; (cons menu-item Order)
;; interp: Order (a list of menu-items) represents a list of menu-items.
;; Order is not defined as a struct, but rather, an itemization. -> was told this by a lab assistant.

;; Problem 1

;; coupon is a (make-coupon menu-item Integer)
;; The item that the coupon will be applicable on is, foodType, a String
;; The percent discount on the item is, percentDiscount, an Integer
;; Template (Problem 2):
;; (define (coupon a-coupon)
;;         (coupon-foodType a-coupon)
;;         (coupon-percentDiscount a-coupon))
(define-struct coupon (foodType percentDiscount))

;; Examples of coupon:
(define coupon-one (make-coupon "coke" 0.0625))
(define coupon-two (make-coupon "pizza" 0.99))

;; Problem 3

;; The variables are defined to be used in the check-expects for count-drinks.
(define orderTest (cons coke (cons pizza (cons (make-menu-item "milk" 10 1 10 "") empty))))
(define orderTest2 (cons coke (cons (make-menu-item "coke" 500 1  5 "") (cons (make-menu-item "milk" 10 1 10 "") empty))))
(define orderTest3 (cons (make-menu-item "cake" 500 1 5 "")  empty))

;; Three check-expects are defined to test whether the count-drinks function works.
;; The first check-expect is made up of 10 cokes, and 10 milks, so the function should return 20.
(check-expect (count-drinks orderTest) 20)
;; The second check-expect is made up of 10 cokes, another 5 cokes, and 10 milks, so the function should return 25.
(check-expect (count-drinks orderTest2) 25)
;; The third check-expect is made up of zero orders than has a coke, coffee, tea, or milk. Therefore, the function should return zero.
(check-expect (count-drinks orderTest3) 0)

;; count-drinks: Order -> Natural
;; count-drinks takes an order (a list of menu-items), and for each item that has a drink (which counts as, "coke", "coffee", "tea", or "milk)
;; the count is incremented by one.
;; Stub: (define (count-drinks order) 0)
;; Template: 
;; (define (count-drinks order)
;;  (cond [(empty? order) (...)]   ;; Base Case
;;        [else
;;         (... (first order)      ;; First condition
;;              (count-drinks (rest order)))]))
(define (count-drinks order) (cond
                               [(empty? order) 0]
                               [(or (equal? (menu-item-foodType (first order)) "milk")
                                    (equal? (menu-item-foodType (first order)) "cofee")
                                    (equal? (menu-item-foodType (first order)) "tea")
                                    (equal? (menu-item-foodType (first order)) "coke"))
                                (+ (* 1 (menu-item-quantity (first order))) (count-drinks (rest order)))]
                               [else (+ 0 (count-drinks (rest order)))]))

;; Problem 4

;; Testing the higer-calorie? function. Relatively self explanatory if you view the function purpose.
(check-expect (higher-calorie? 10 9) true)
(check-expect (higher-calorie? 10 11) false)
(check-expect (higher-calorie? 10 10) false)

;; higher-calorie?: Natural Natural -> Boolean
;; higher-calorie? if the first natural is greater than (NOTE: not greater than or equal to) the second natural, return true. Otherwise, return false;
;; Stub: (define (higher-calorie? num1 num2) true)
;; Template:
;;(define (higher-calorie? num1 num2)
;;  (num1 > num2))
(define (higher-calorie? num1 num2) (> num1 num2))

;; Will be comparing all of these orders with check-expects to a set number of calories, which in the case of the check-expects, will be 200.
(define orderTest4 (cons coke (cons pizza (cons (make-menu-item "milk" 201 1 10 "") empty))))
(define orderTest5 (cons coke (cons pizza (cons (make-menu-item "milk" 200 1 10 "") empty))))
(define orderTest6 (cons coke (cons pizza (cons (make-menu-item "milk" 199 1 10 "") empty))))

;; The first check-expect should return true, because one of the items has a calorieNum of over 200.
(check-expect (any-high-calorie-items? orderTest4 200) true)
;; The second check-expect should return false, because even though 200 is equal to 200, it is not greater than. Meaning it just meets the parameters for the function to work.
(check-expect (any-high-calorie-items? orderTest5 200) false)
;; The third check-expect should return false, because none of the items have a number of calories greater than 200.
(check-expect (any-high-calorie-items? orderTest6 200) false)

;; any-high-calorie-items?: Order Natural -> Boolean
;; any-high-calorie-items? takes an Order and a Natural, if an item in the order returns a number higher than the specified natural, the function returns true.
;; Stub: (define (any-high-calorie-items) true)
;; Template (Problem 2):
;; (define (any-high-calorie-items? a-order a-natural)
;;  (cond [(empty? order) false]
;;        [(cons?  order) (if (helper-function (first order) a-natural)
;;                           true   ;helper on first in list
;;                           (any-high-calorie-items? (rest order) a-natural))])) ;natural recusion
(define (any-high-calorie-items? order maxCalorie)
  (cond [(empty? order) false]
        [(cons?  order) (if (higher-calorie? (menu-item-calorieNum (first order)) maxCalorie)
                            true   ;if higher-calorie? returns true, it means that an item in the order's number of calories is greater than maxCalorie (ignoring number of items)
                            (any-high-calorie-items? (rest order) maxCalorie))])) ;natural recusion


;; Problem 5

;; The following check-expects are to test the check-quatity helper function.
(check-expect (check-quantity (make-menu-item "milk" 201 1 10 "")) true)
(check-expect (check-quantity (make-menu-item "milk" 201 1 1 "")) false)
(check-expect (check-quantity (make-menu-item "milk" 201 1 0 "")) false)

;; check-quantity: menu-item -> Boolean
;; check-quantity takes a menu-item, and if the quantity of the item is greater than one, the function returns true, otherwise, the function returns false.
;; Stub: (define (check-quantity menu-item) true)
;; Template:
;; (define (check-quantity a-menu-item)
;;         (> (quantity a-menu-item) 1))
(define (check-quantity a-menu-item)
  (> (menu-item-quantity a-menu-item) 1))

;; The following check-expects are used to check in varying ranges of complexity what the list-multiples function will return.
;; The check-expect should return empty, since the quantity is zero.
(check-expect (list-multiples (cons (make-menu-item "milk" 199 1 0 "") empty)) empty)
;; The check-expect should return empty, since the quantity is not greater than one.
(check-expect (list-multiples (cons (make-menu-item "milk" 199 1 1 "") empty)) empty)
;; The check-expect will return the milk item, because the quantity is greater than one.
(check-expect (list-multiples (cons (make-menu-item "milk" 199 1 2 "") empty)) (cons (make-menu-item "milk" 199 1 2 "") empty))
;; The next two check-expects are to test the function with multiple list items. If the function works properly with two, then it should work properly with any number greater than two.
;; The differences between these two check-expects is that the quantity of one is 2, and the quantity of one is 1. The first check-expect will return two menu-items, the second will return one menu-item.
(check-expect (list-multiples (cons (make-menu-item "milk" 199 1 2 "") (cons (make-menu-item "milk" 199 1 2 "") empty))) (cons (make-menu-item "milk" 199 1 2 "") (cons (make-menu-item "milk" 199 1 2 "") empty)))
(check-expect (list-multiples (cons (make-menu-item "milk" 199 1 2 "") (cons (make-menu-item "milk" 199 1 1 "") empty))) (cons (make-menu-item "milk" 199 1 2 "") empty))


;; list-multiples: Order -> Order
;; list-multiples consumes and order and produces a lit of all menu items where the quantity of the item is greater than one.
;; Stub: (define (list-multiples order) order)
;; Template:
;;(define (list-multiples a-order) ;culler: keep&chuck  builds list  two recursive calls
;;  (cond [(empty? a-order) empty]
;;        [(cons? a-order) (if (helper-function (first a-order))
;;                          (cons (first a-order) (helper-function (rest a-order)))
;;                          (list-multiples (rest a-order)))]))
(define (list-multiples a-order) ;culler: keep&chuck  builds list  two recursive calls
  (cond [(empty? a-order) empty]
        [(cons? a-order) (if (check-quantity (first a-order))
                             (cons (first a-order) (list-multiples (rest a-order)))
                             (list-multiples (rest a-order)))]))

;; Problem 6

;; Checking the contains-special-order? function. The first check-expect is true because the string is empty, and the second is false because the string is not empty.
(check-expect (contains-special-order? (make-menu-item "milk" 199 1 2 "")) false)
(check-expect (contains-special-order? (make-menu-item "milk" 199 1 2 "special order")) true)

;; contains-special-order?: menu-item -> Boolean
;; contains-special-order? takes a menu-item and returns false if that menu item does not have a special order. Otherwise, the function returns true.
;; Stub: (define (contains-special-order? menu-item) true)
;; Template:
;;(define (contains-special-order? a-menu-item)
;;  (string=? "" (menu-item-specialOrder a-menu-item))
(define (contains-special-order? a-menu-item)
  (not(string=? "" (menu-item-specialOrder a-menu-item))))

;; The following check-expects test the make-special-order-string function.
;; The first check-expect should return: foodtype: special-instruction, which is "milk: hold the cow juice".
(check-expect (make-special-order-string (make-menu-item "milk" 199 1 2 "hold the cow juice")) "milk: hold the cow juice")
;; The second check-expect should return: "milk: ", because there are no special instructions. This case should not appear in the function, but is still tested to cover all bases.
(check-expect (make-special-order-string (make-menu-item "milk" 199 1 2 "")) "milk: ")

;; make-special-order-string: menu-item -> String
;; make-special-order-string takes a menu-item and returns a String in the format: "foodtype: special-instruction".
;; Stub: (define (make-special-order-string menu-item) "")
;; Template:
;;(define (make-special-order-string a-menu-item)
;; (string-append foodtype ":" specialOrder))
(define (make-special-order-string a-menu-item)
  (string-append (menu-item-foodType a-menu-item) ": " (menu-item-specialOrder a-menu-item)))

;; The following check-expects are designed to test if the function returns the correct ListOfStrings.

;; The first check-expect should return empty, because there are no special-orders specified.
(check-expect (special-orders (cons (make-menu-item "milk" 199 1 2 "") empty)) empty)
;; The first check-expect should return a list with "milk: no pulp", because there is one order, and it is milk with the special order, "no pulp".
(check-expect (special-orders (cons (make-menu-item "milk" 199 1 2 "no pulp") empty)) (cons "milk: no pulp" empty))
;; The first check-expect should return a list with "milk: no pulp", because there is one order with a special order, and it is milk with the special order, "no pulp".
(check-expect (special-orders (cons (make-menu-item "milk" 199 1 2 "no pulp") (cons (make-menu-item "milk" 199 1 2 "") empty))) (cons "milk: no pulp" empty))
;; The first check-expect should return a list with "milk: no pulp", and "pizza: no sauce, no cheese", because there are two orders with a special orders.
(check-expect (special-orders (cons (make-menu-item "milk" 199 1 2 "no pulp") (cons (make-menu-item "pizza" 199 1 2 "no sauce, no cheese") empty))) (cons "milk: no pulp" (cons "pizza: no sauce, no cheese" empty)))

;; special-orders: Order -> ListOfStrings
;; special-orders takes an Order, and returns a list of strings of orders that have special instructions. The returned ListOfStrings will contain items in the format: "foodtype: special-instruction"
;; Stub: (define (special-orders order) ListOfStrings)
;; Template:
;;(define (special-orders a-order) ;culler: keep&chuck  builds list  two recursive calls
;;  (cond [(empty? a-order) empty]
;;        [(cons? a-order) (if (helper-function (first a-order)
;;                         (cons (first a-order) (special-orders (rest a-order)))
;;                          (special-orders (rest a-order)))]))
(define (special-orders a-order) ;culler: keep&chuck  builds list  two recursive calls
  (cond [(empty? a-order) empty]
        [(cons? a-order) (if (contains-special-order? (first a-order))
                             (cons (make-special-order-string (first a-order)) (special-orders (rest a-order)))
                             (special-orders (rest a-order)))]))

;; Problem 7

;; The following check-expects are tested to see if a coupon is equal, one case, and if a coupon is not equal, another caxse.
(check-expect (coupon-match?  (make-menu-item "coke" 199 1 2 "") (make-coupon "coke" 0.0625)) true)
(check-expect (coupon-match? (make-menu-item "cake" 199 1 2 "") (make-coupon "coke" 0.0625)) false)


;; coupon-match?: menu-item coupon -> Boolean
;; coupon-match? takes a menu-item, and a coupon, and compares the type of foods. If they are eqaul, the function returns true, otherwise, the function returns false.
;; Stub: (define (coupon-match? menu-item coupon) true)
;; Template:
;; (define (coupon-match? a-menu-item a-coupon)
;;  (coupon-food-type = a-coupon-foodtype))
(define (coupon-match? a-menu-item a-coupon)
  (string=? (coupon-foodType a-coupon) (menu-item-foodType a-menu-item)))


;; The following variables and check-expects are used to check if the correct discount is applied to the the menu item.
(define coupon-three (make-coupon "coke" 0.01))
(define coupon-four (make-coupon "pizza" 0.99))

;; Two tests seem adequate due to the repetitive nature of this function. If it works more than once it is sound (generally not the best but in this case it is simple math).
(check-expect (add-discount (make-menu-item "coke" 199 100 2 "") coupon-three) (make-menu-item "coke" 199 99 2 ""))
(check-expect (add-discount (make-menu-item "pizza" 199 9001 2 "") coupon-four) (make-menu-item "pizza" 199 90.01 2 "")) ;; Calculated using a calculator.

;; add-discount: menu-item coupon -> menu-item
;; add-discount takes a menu-item, and a coupon, and applies the percent discount specified in the coupon to the menu item.
;; Note: Nothing else is changed in the menu item, only the unit price.
;; Stub: (define (add-discount menu-item coupon) menu-item)
;; Template:
;;(define (add-discount a-menu-item a-coupon)
;;  (make-menu-item ... (* (a-coupon-percentDiscount a-coupon) (menu-item-price a-menu-item))))
(define (add-discount a-menu-item a-coupon)
  (make-menu-item (menu-item-foodType a-menu-item) (menu-item-calorieNum a-menu-item) (- (menu-item-price a-menu-item) (* (coupon-percentDiscount a-coupon) (menu-item-price a-menu-item))) (menu-item-quantity a-menu-item) (menu-item-specialOrder a-menu-item)))

;; The following check expects are to test the functionality of the apply-coupon function.
;; The first two check-expects are tested with only one item in the order, and should either return the same menu item (as in the first check-expect), or the menu item should be the same, other the the price.
(check-expect (apply-coupon (cons (make-menu-item "coke" 199 1 2 "") empty)  (make-coupon "cake" 0.01)) (cons (make-menu-item "coke" 199 1 2 "") empty)) 
(check-expect (apply-coupon (cons (make-menu-item "cake" 199 100 2 "") empty)  (make-coupon "cake" 0.01)) (cons (make-menu-item "cake" 199 99 2 "") empty))

;; The second two check-expects are to test a list with multiple items. One will return an order with no changes, one will return one with one change, and one will return one with two changes.
(check-expect (apply-coupon (cons (make-menu-item "cake" 199 100 2 "") (cons (make-menu-item "cake" 199 100 2 "") empty))  (make-coupon "coke" 0.01)) (cons (make-menu-item "cake" 199 100 2 "") (cons (make-menu-item "cake" 199 100 2 "") empty)))
(check-expect (apply-coupon (cons (make-menu-item "cake" 199 100 2 "") (cons (make-menu-item "coke" 199 100 2 "") empty))  (make-coupon "cake" 0.01)) (cons (make-menu-item "cake" 199 99 2 "") (cons (make-menu-item "coke" 199 100 2 "") empty)))
(check-expect (apply-coupon (cons (make-menu-item "cake" 199 100 2 "") (cons (make-menu-item "cake" 199 100 2 "") empty))  (make-coupon "cake" 0.01)) (cons (make-menu-item "cake" 199 99 2 "") (cons (make-menu-item "cake" 199 99 2 "") empty)))

;; apply-coupon: Order Coupon -> Order
;; apply-coupon takes an order and a coupon, and returns an order with coupons applied to the menu-items that match the specific coupon.
;; For example, a coupon for pizza would work on pizza, and would not work on salad.
;; The coupon will also be applied to the unit price, not the total price.
;; Stub: (define apply-coupon order coupon) order)
;; Template:
;;(define (apply-coupon a-order a-coupon) 
;;  (cond [(empty? a-order) empty]
;;        [(cons? a-order) (if (helper-function-condition (first a-order))
;;                          (cons (new-menu-item-helper (first a-order)) (apply-coupon (rest a-order) a-coupon))
;;                          (apply-coupon (rest a-order) a-coupon))]))
(define (apply-coupon a-order a-coupon) 
  (cond [(empty? a-order) empty]
        [(cons? a-order) (if (coupon-match? (first a-order) a-coupon)
                          (cons (add-discount (first a-order) a-coupon) (apply-coupon (rest a-order) a-coupon))
                          (cons (first a-order) (apply-coupon (rest a-order) a-coupon)))]))

;; Problem 8

;; The following check expect is checking the functionality of the calculate-cost helper function. Two cases are tested, one with zero discount, and one with an arbirtary discount.
(check-expect (calculate-cost (make-menu-item "cake" 199 100 1 "") 0.01) 99)
(check-expect (calculate-cost (make-menu-item "cake" 199 100 1 "") 0) 100)
(check-expect (calculate-cost (make-menu-item "cake" 1990 100 10 "") 0.01) 990)
(check-expect (calculate-cost (make-menu-item "cake" 199 100 10 "") 0) 1000)

;; calculate-cost: menu-item Natural -> Natural
;; Calculates the cost with a discount (0 if not a senior) of a menu-item. The function takes into account quantity.
;; Stub: (define (calculate-cost menu-item natural) 0)
;; Template:
;; (define (calculate-cost a-menu-item discount)
;;  (- cost (* cost discount)))
(define (calculate-cost a-menu-item discount)
  (- (* (menu-item-quantity a-menu-item) (menu-item-price a-menu-item)) (* (menu-item-quantity a-menu-item) (menu-item-price a-menu-item) discount)))

;; Senior discount is defined as 10% (default).
(define SENIOR-DISCOUNT 0.1)

;; The following check-expects are to test the functionality of the order-cost function.
;; The first two check-expects test the function with only one menu-item, without a senior discount and with a senior discount.
(check-expect (order-cost (cons (make-menu-item "cake" 199 50 10 "") empty) false) 500)
(check-expect (order-cost (cons (make-menu-item "cake" 199 50 10 "") empty) true) 450)
;; The second two check-expects test the function with multiple menu-items, without a senior discount and with a senior discount.
(check-expect (order-cost (cons (make-menu-item "cake" 199 50 10 "") (cons (make-menu-item "cake" 199 50 10 "") empty)) false) 1000) 
(check-expect (order-cost (cons (make-menu-item "cake" 199 50 10 "") (cons (make-menu-item "cake" 199 50 10 "") empty)) true) 900) 

;; order-cost: Order Boolean -> Natural
;; order-cost takes an Order and a Boolean (true if the customer is a senior citizen), and returns a natural which is the total cost of the order.
;; Stub: (define (order-cost order boolean) 0)
;; Template:
;; (define (order-cost a-order is-senior)
;;  (cond [(empty? a-order) 0]
;;        [(cons? a-order) (if (helper-function-condition (first a-order)
;;                         (+ (helper-cost-function (first a-order)) (order-cost (rest a-order)))
;;                          (order-cost (rest a-order)))]))
 (define (order-cost a-order is-senior)
  (cond [(empty? a-order) 0]
        [(cons? a-order) (if is-senior
                         (+ (calculate-cost (first a-order) SENIOR-DISCOUNT) (order-cost (rest a-order) is-senior))
                          (+ (calculate-cost (first a-order) 0) (order-cost (rest a-order) is-senior)))]))


;; Problem 9


;; The following check-expect outputs a list of orders with a discount on the cake, and not the tea.
(check-expect (apply-coupon (cons (make-menu-item "cake" 199 100 2 "") (cons (make-menu-item "tea" 90 100 10 "") empty)) (make-coupon "cake" 0.01))
              (cons (make-menu-item "cake" 199 99 2 "") (cons (make-menu-item "tea" 90 100 10 "") empty)))
;; That Order is: (cons (make-menu-item "cake" 199 99 2 "") (cons (make-menu-item "tea" 90 100 10 "") empty)) 

;; The following check-expect involves a nested function, first calculating the discount on the cake, and then taking the senior discount into account.
;; Calculation: 
;; First menu-item: 99 * 2 = 198, senior discount = 10%, so 198 - (198 * .1) = 178.2 
;; Second menu-item: 100 * 10 = 1000, senior discount = 10%, so 1000 - (1000 * .1) = 900
;; Therefore, the total cost should be 1078.2
(check-expect (order-cost (apply-coupon (cons (make-menu-item "cake" 199 100 2 "") (cons (make-menu-item "tea" 90 100 10 "") empty)) (make-coupon "cake" 0.01)) true) 1078.2)


