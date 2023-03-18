# hoop

```lisp
;; Print some numbers.
 (loop for i from 1 to 3
       do (print i))
; >>  1
; >>  2
; >>  3
; =>  NIL
```

```lisp
(hoop ((i :from 1 :to 3))
      nil
  (print i))
```

```lisp
;; Print every third number.
 (loop for i from 10 downto 1 by 3
       do (print i))
; >>  10 
; >>  7 
; >>  4 
; >>  1 
; =>  NIL
```

```lisp
(hoop ((i :from 10 :to 1 :by -3))
      nil
  (print i))
```

```lisp
;; Step incrementally from the default starting value.
 (loop for i below 3
       do (print i))
; >>  0
; >>  1
; >>  2
; =>  NIL
```

```lisp
(hoop ((i :from 0 :before 3))
      nil
  (print i))
```


6.1.2.1.2.1 Examples of for-as-in-list subclause

```lisp
;; Print every item in a list.
 (loop for item in '(1 2 3) do (print item))
>>  1
>>  2
>>  3
=>  NIL
```

```lisp
(hoop ((item :in-list '(1 2 3)))
      nil
  (print item))
```

```lisp
;; Print every other item in a list.
 (loop for item in '(1 2 3 4 5) by #'cddr
       do (print item))
>>  1
>>  3
>>  5
=>  NIL
```

```lisp
(hoop ((item :in-list '(1 2 3 4 5) by #'cddr))
      nil
  (print item))
```

```lisp
;; Destructure a list, and sum the x values using fixnum arithmetic.
 (loop for (item . x) of-type (t . fixnum) in '((A . 1) (B . 2) (C . 3))
       unless (eq item 'B) sum x)
=>  4
```

```lisp
(hoop (((item . x) :in-list '((A . 1) (B . 2) (C . 3)))
       (j := 0))
      j
  (unless (eq item 'B)
    (incf j x)))
```

6.1.2.1.3.1 Examples of for-as-on-list subclause

;; Collect successive tails of a list.
 (loop for sublist on '(a b c d)
       collect sublist)
=>  ((A B C D) (B C D) (C D) (D))

```lisp
(hoop ((sublist :on-list '(a b c d))
       (c :accumulate))
      c
  (c sublist))
```

;; Print a list by using destructuring with the loop keyword ON.
 (loop for (item) on '(1 2 3)
       do (print item))
>>  1 
>>  2 
>>  3 
=>  NIL


```lisp
(hoop (((item) :on-list '(1 2 3)))
      nil
  (print item))
```
