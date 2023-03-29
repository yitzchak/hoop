# HOOP

HOOP is an iteration facility for Common Lisp that is conceptually
similar to the builtin LOOP facility. There are some significant
differences though:

1. HOOP places all of the iteration clauses in the first argument
   position like LET.

2. Accumulation clauses are delected in the "binding" group, but
   called in the body via functioned declared with FLET. The currect
   value of the accumulation can be accessed with a variable of the
   same name.

3. Various termination clauses can be placed in the "binding" group or
   HOOP-FINISH, HOOP-NEXT, or RETURN can be called in the body.

4. There are no unconditional, conditional or other selectable
   clauses. These are left as ordinary Lisp forms in a body as in LET.

5. There is no parsing of the body or of the binding forms. The
   binding and iteration forms are typically passed to the appropriate
   MAKE-INSTANCE for the clause type. What appears to be keywords are
   usually just key/value pairs.

6. There is a parallel binding version (HOOP) and a serial binding
   version (HOOP*). One can also switch between parallel and serial
   with the `:parallel` or `:serial` clauses.

HOOP is still experimental. It does not have type declarations or much
type checking yet. It can do everything else LOOP can do and
somethings that LOOP cannot. Generally there is a simple translation
between LOOP and HOOP. For example using LOOP to collect generate `'(1
3 5 7 9)` could be done with:

```lisp
(loop for i from 1 to 10
      when (oddp i)
        collect i)
```

Using HOOP this can be done with:

```
(hoop* ((:step i :from 1 :to 10)
        (:collect c))
  (when (oddp i)
    (c i)))
```

The overall syntax of HOOP with serial bindings is

```lisp
(hoop* (clause*)
  body)
```

The various clauses are described below.

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
(:each-item d-var-spec &key in (by #'cdr))
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

```lisp
(:generate d-var-spec &key using then)
```

Initializes d-var-spec by setting it to the result of evaluating
`using` on the first iteration, then setting it to the result of
evaluating `then` on the second and subsequent iterations. If `:then`
is omitted, the construct uses `using` on the second and subsequent
iterations.

If d-var-spec is a list then `multiple-value-setq` will be used to
assign to the names in d-var-spec. Each item in this list will be
treated as a separate d-var-spec.

## Examples

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

# Each Element Clause

```lisp
(:each-elt d-var-spec &key in by start end)
```

Uses `elt` to iterate over the elements of a sequence given by the
`:in` key. The range of indicies and the stepping between successive
indicies can be controlled with the `:start`, `:end` and `:by` keys.

## Example

```
CL-USER> (hoop ((:each-elt i :in "abcd"))
           (print i))

#\a 
#\b 
#\c 
#\d 
NIL
```

# Each Key Value Clause

```lisp
(:each-key-value (d-var-spec d-var-spec) &key in)
```

Iterates over a hash table supplied by the `:in` key.

# Examples

```
CL-USER> (defparameter a (make-hash-table))
A
CL-USER> (setf (gethash :fu a) 1)
1 (1 bit, #x1, #o1, #b1)
CL-USER> (setf (gethash :bar a) 2)
2 (2 bits, #x2, #o2, #b10)
CL-USER> (hoop ((:each-key-value (k v) :in a))
           (format t "~s ~s~%" k v))
:FU 1
:BAR 2
NIL
CL-USER> (hoop ((:each-key-value (k nil) :in a))
           (print k))

:FU 
:BAR 
NIL
```

# Each Symbol Clause

```lisp
(:each-symbol simple-var &key in symbol-types package status)
```

# Examples

```
CL-USER> (make-package "FU")
#<PACKAGE "FU">
CL-USER> (intern "BAR" 'FU)
FU::BAR
NIL
CL-USER> (export 'fu::bar 'fu)
T
CL-USER> (intern "QUUX" 'FU)
FU::QUUX
NIL
CL-USER> (hoop ((:each-symbol s :in 'fu
                 :package p :status stat
                 :symbol-types (:external :internal)))
           (format t "~s ~s ~s~%" s p stat))
FU::QUUX #<PACKAGE "FU"> :INTERNAL
FU:BAR #<PACKAGE "FU"> :EXTERNAL
NIL
```

# With Clause

```lisp
(:with simple-var &key =)
```
