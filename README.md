# hoop

## Step Clause

```lisp
(:step simple-var &key (from 0) to before (by 1))
```

* `:from` specifies the value from which stepping begins.
* `:to` marks the end value for stepping.
* `:before` marks the end value for stepping like `to` but iteration is
  terminated before simple-var reaches the value.
* `:by` marks the increment or decrement. If a negative value is used
  then simple-var will be decremented.

### Basic Examples

```
CL-USER> (hoop ((:step i :from 1 :to 3))
           (print i))

1 
2 
3 
NIL
CL-USER> (hoop ((:step i :from 10 :to 1 :by -3))
           (print i))

10 
7 
4 
1 
NIL
CL-USER> (hoop ((:step i :before 3 :by -1))
           (print i))
NIL
CL-USER> (hoop ((:step i :before 3))
           (print i))

0 
1 
2 
NIL
```

## List Item Clause

```lisp
(:each-item d-var-spec &key in on (by #'cdr))
```

Iterates over the contents of a list specifies by either `:in` or `:on`.

* `:in` specifies the list form to iterate over. d-var-spec is applied
  to the CAR of each list CONS.
* `:by` specifies the function to advance the iteration.

If d-var-spec is a CONS then destructuring happens in the same way as
LOOP. Unlike LOOP this destructuring is done via symbol macros thereby
making it possible to use SETF to update the list values.

## Examples

```
CL-USER> (hoop ((:each-item i :in '(1 2 3)))
           (print i))

1 
2 
3 
NIL
CL-USER> (hoop ((:each-item i :in '(1 2 3 4 5) :by #'cddr))
           (print i))

1 
3 
5 
NIL
CL-USER> (hoop ((:each-item (i . x) :in '((A . 1) (B . 2) (C . 3)))
                (:sum j))
           (unless (eq i 'B)
             (j x)))
4
CL-USER> (defparameter a (list 1 2 3 4 5))
A
CL-USER> (hoop ((:each-item i :in a))
           (setf i (list i (if (oddp i) :odd :even))))
NIL
CL-USER> a
((1 :ODD) (2 :EVEN) (3 :ODD) (4 :EVEN) (5 :ODD))
```

## List Sublist Clause

```lisp
(:each-sublist d-var-spec &key in (by #'cdr))
```

Iterates over the contents of a list specifies by either `:in` or `:on`.

* `:in` specifies the list form to iterate over. d-var-spec is applied
  to each list CONS.
* `:by` specifies the function to advance the iteration.

If d-var-spec is a CONS then destructuring happens in the same way as
LOOP. Unlike LOOP this destructuring is done via symbol macros thereby
making it possible to use SETF to update the list values.

## Examples

```
CL-USER> (hoop ((:each-sublist sublist :in '(a b c d))
                (:collect j))
           (j sublist))
((A B C D) (B C D) (C D) (D))
CL-USER> (hoop ((:each-sublist (i) :in '(1 2 3)))
           (print i))

1 
2 
3 
NIL
```

# Generate Clause

## Examples

```lisp
(:generate d-var-spec &key using then)
```



```
CL-USER> (hoop ((:generate i :using 1 :then (+ i 10))
                (:step j :from 1 :to 5)
                (:collect k))
           (k i))
(1 11 21 31 41)
CL-USER> (hoop ((:each-item i :in '(:fu :bar :wibble))
                (:with j := '(:bar 2 :quux 743 :fu 27))
                (:generate (indicator value nil)
                 :using (get-properties j (list i))))
           (when indicator
             (format t "Found ~s with a value of ~s~%"
                     indicator value)))
Found :FU with a value of 27
Found :BAR with a value of 2
```