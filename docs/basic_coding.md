# Basic Coding

## Basic rules

### Boolean values

**false** is signalized by `nil`.
Anything other than `nil` signalizes **True**.
For readability reasons, `t` is also provided.

### nil

Besides signalizing false values, `nil` also represents the empty list `()`.

### Variables

In EusLisp, global variables names are conventionally surrounded by `*`, such as in `*robot*`.
This notation allows clear understanding of each variable's scope, but is not obligatory.

Local variables can be introduced with `let`. Such variables cannot be accessed from outside the `let` statement.

```
(let (a)
  (setq a 1))
a --> unbound variable error
```

### Comments

Line comments start with `;`.
Block comments are surrounded by `#|` and `|#`.
Regarding line comments:
- `;` is indented at fixed depth, many times at column 40.
- `;;` is indented alongside code.
- `;;;` is not indented.

## Lists

In EusLisp linear lists are used, and there is no array.
Objects of different types can be stored in the same list.

### Creating lists

Lists can be created with the function `list`
```
(setq *list* (list 0 1 2 3))
```

or with the `'` notation.

```
(setq  *list* '(0 1 2 3))
```


### Refer to elements

Elements of a certain index can be accessed using `elt` or `nth`.
```
(elt *list* 0)
;; 0
(nth 0 *list*)
;; 0
```

The first element can be referred with `car`.
```
(car *list*)
;; 0
```

`cdr` gives the list of all but the first element.
```
(cdr *list*)
;; (1 2 3)
```

The last element can be referred with the following.
```
(car (last *list*))
;; 3
```

The length of the list is given by `length`
```
(length *list*)
;; 4
```

### Editing lists

Lists can be joined with `append`.
```
(setq *newlist* (append *list* (list 4 5 6)))
;; *newlist* is (0 1 2 3 4 5 6)
;; *list* remains unaltered
```

An element can be added with `push` (destructive) or `cons` (not destructive).

```
(push -1 *list*)
;; (-1 0 1 2 3)
```

`pop` takes out the first element of the list destructively.
```
(pop *list*)
;; -1
;; *list* becomes (0 1 2 3)
```


## Conditional clauses

### when

`when` is executed when the condition is `non-nil`.

```
(when condition
  (print "true"))
```

### unless

`unless` is the opposite of `when`, being executed when the condition is `nil`.

```
(unless condition
  (print "false"))
```

### cond

`cond` is used for if-else-if like statements.

```
(setq *val* 0)  ;; Try out with different values

(cond
 ((= *val* 0)
  (print "val = 0"))
 ((= *val* 1)
  (print "val = 1"))
 (t
  (print "default")))
```

In `cond`, the first element of each s-expression is evaluated in order. If it is `non-nil` the rest of the s-expression is executed and `cond` exits; if it is `nil` the next s-expression condition is evaluated.
In the above, the `t` condition is executed when all of the above conditions do not apply (i.e. \*val\* is different from 0 and 1).

### if

`if` works like an if-else clause. If the condition is `non-nil` the first s-expression is executed, else the second one is executed.

```
(if condition
    (print "true")
  (print "false"))
```

Because `if` only deals with single s-expressions, `progn` is used for evaluating multiple expressions in the same clause.
```
(if condition
    (progn
      (print "this")
      (print " is ")
      (print "true"))
  (print "false"))
```

## Iteration

### dotimes

Similar to `for` statement.

```
(dotimes (i 10)
  (print i))
```

In the above, `print` is executed for `i` values from `0` to `9`.


### dolist

Similar to `foreach`.

```
(dolist (i (list 0 1 2 3))
  (print i))
```

In the above, `i` takes the value of each element in the list, from the start.

### while

Loops while the condition is `non-nil`.

```
(setq i 0)
(while (< i 10)
  (print i)
  (setq i (+ 1 i)))
```

The above is similar to the `dotimes` example above.
(However, here `i` is a global variable)


### do-until-key

`do-until-key` is a special iteration form. Instead of evaluating a certain condition, it is executed until the `Enter` key is pressed.

```
(do-until-key
 (print "press ENTER to stop"))
```


## Other control clauses

### return

`return` is used to exit loops such as `dotimes`, `dolist` and `while`.

```
(dotimes (i 10)
  (when (= i 5) (return))
  (print i))
```

### return-from

`return-from` is used to exit a block with the given name, such as ones signaled by a function or method.
Details will be given afterwards.


## Functions and Classes

### Functions

Functions are defined with `defun`.

```
(defun plus (a b)
  (+ a b))
```

And called as follows.

```
(plus 1 2)
```

The above is evaluates as `3`.
**Functions return the last evaluated value.**

Forced exit is done by `return-from`.

```
(defun plus-minus (a b c)
  (let (d e)
    (setq d (- a b))
    (when (< d 0)
      (print "a < b, abort.")
      (return-from plus-minus nil))
    (setq e (- (+ a b) c))
    e)  ;; let returns e
  )
```

In the above, try out the differences between
```
(plus-minus 1 2 3)
```
and
```
(plus-minus 2 1 3)
```

### optional, key

It is possible to give default values for function arguments as follows.

```
(defun negate (a &optional (b 0))
  (- b a))
```

In the above, it is possible to use both 
```
(negate 10)
```
and
```
(negate 10 5)
```

Similarly, definition using key arguments is as follows.
```
(defun negate (a &key (b 0))
  (- b a))
```
Which makes possible to use both
```
(negate 10)
```
and
```
(negate 10 :b 5)
```

Differently from `&optional`, it is necessary to give key arguments by using `:` followed by the variable name.

### Classes

Classes and member functions can be defined as follows.

```
(defclass myclass
  :super propertied-object
  :slots (myname myage))

(defmethod myclass
  (:init
   (name age)
   (setq myname name
         myage age)
   self)

  (:myname
   (&optional (newname nil))
   (when newname (setq myname newname))
   myname)

  (:myage
   (&optional (newage nil))
   (when newage (setq myage newage))
   (unless (= myage 17)
     (print "something wrong.")
     (return-from :myage 17))
   17)

  )  ;; defmethod
```

In `defclass`, `:super` indicates the parent class, which will be inherited.
Here, the class `propertied-object` is used.
Member variables are signalized by `:slots`.

Member functions are defined with `defmethod`, and have syntax similar to normal functions.
`:init` is the constructor, which returns `self` i.e. the instance itself.
Methods are defined with `:` followed by the method name.

Class objects can be created with the following.

```
(setq *me* (instance myclass :init "John Smith" 30))
```

Methods are called by `send`.

```
(send *me* :myname)
```

The above returns `"John Smith"`.

```
(send *me* :myname "Alan Smithee")
```
The above replaces the member variable `myname` with `"Alan Smithee"`.
This way, it is possible to use the same method for `set` and `get` purposes.
