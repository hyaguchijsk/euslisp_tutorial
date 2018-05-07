# Advanced Coding

## Function symbol

It is possible to obtain and execute symbols of previously defined functions.
Function symbols can be obtained with `#'`, being correspondent to function pointers in C.



## Anonymous function (lambda)

Using the function symbol, it is possible to create and apply functions at the same time.

Regular function definition looks like:

```
(defun funcname (args) (do-something))
```

Anonymous functions can be defined with:

```
#'(lambda (args) (do-something))
```

## sort

`sort` is a function that sorts a list according to an evaluation function, which is given using the function symbol.

For example, try to execute the following:

```
(setq *ls* (list 0 3 1 2))
(print (sort *ls* #'<))
(print (sort *ls* #'>))
```

The first sort uses the function `<`, sorting in crescent order.
The second sort uses the function `>`, sorting in decreasing order.

Please note that `sort` is destructive, causing the given variable to be changed.


## mapcar

`mapcar` applies given function symbol to all elements of a list.

In the following example, a lambda function that returns twice the input is given to double the value of all elements from the list.

```
(mapcar #'(lambda (x) (* 2 x)) (list 0 3 1 2))
;; (0 6 2 4)
```


## funcall

`funcall` executes the given function symbol.

The next example shows a function that takes two values and a comparison function symbol `func`. If the result of executing `func` with the two values is non-nil it returns the first, else it returns the second value.
```
(defun numcheck (a b func) (if (funcall func a b) a b))
```

Try out and check the following results.

```
(numcheck 0 1 #'<)  ;; 0
(numcheck 0 1 #'>)  ;; 1
```
