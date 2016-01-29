# Basic Coding

## 基本ルール

### 真偽値

真偽値は`t`と`nil`が使われますが，
** 真は`nil`でないもの ** であるため，
なにか値が返ってくれば真，
`nil`が返ってくれば偽，という記述ができます．


### nil

`nil`は，様々な意味を持ちます．
真偽値の偽として使われるほか，空のリストとしても作用します．


### グローバル変数

euslispにおけるグローバル変数の命名規則は，
`*`と`*`で囲まれた変数名を取ります．
例えば，`*robot*`のようにします．
ただし，強制ではないため，
あやまってローカル変数宣言を忘れた場合にも
グローバル変数として認識されます．

### コメント

euslispにおけるコメントは，
`;`で始まる行と，`#|`,`|#`で囲まれたブロックとなります．
この内，`;`は3種類あり，
`;`はインデントの深さが固定，
`;;`はコードに応じたインデント，
`;;;`はインデントされないコメント行になります．


## リスト

euslispには配列はなく，線形リストを用います．
異なる型のオブジェクトを同一のリストに保持できます．

### 作り方

```
(setq *list* (list 0 1 2 3))
```

もしくは

```
(setq  *list* `(0 1 2 3))
```

とします．

### 要素の取り出し

リストの長さを取得
```
(length *list*)
;; 4
```

インデックス指定を行う場合
```
(elt *list* 0)
;; 0
```

最初の要素を取り出す場合
```
(car *list*)
;; 0
```

最初の要素を除いたリストを取り出す場合
```
(cdr *list*)
;; (1 2 3)
```

最後の要素を取り出す場合
```
(car (last *list*))
;; 3
```


### 編集方法

リストの結合
```
(setq *newlist* (append *list* (list 4 5 6)))
;; *newlist* は (0 1 2 3 4 5 6)
;; *list*は変化せず
```

リストへの要素追加(破壊的)
```
(push -1 *list*)
;; (-1 0 1 2 3)
```

リストからの要素取り出し(破壊的)
```
(pop *list*)
;; -1が 返ってくる
;; *list* は (0 1 2 3)
```


## 条件分岐

### when

`when`は，条件式が`nil`でない場合に
中身が実行されます．

```
(when condition
  (print "true"))
```

### unless

`unless`は`when`の逆です．
条件式が`nil`の場合に
中身が実行されます．

```
(unless condition
  (print "false"))
```

### cond

`cond`は，複数の条件分岐を続けて書くことができます．

```
(setq *val* 0)  ;; 適宜変えて実行してください

(cond
 ((= *val* 0)
  (print "val = 0"))
 ((= *val* 1)
  (print "val = 1"))
 (t
  (print "default")))
```

`cond`では上から順番に評価が行われます．
S式の先頭の条件が`nil`でない場合，そのS式の中身が実行され，
`nil`の場合は次のS式の条件が評価されます．
最後に`t`を置くことで，
すべての条件に当てはまらなかった場合を表現しています．


### if

euslispの`if`は条件式が真であるときは最初のS式が，
偽であるときは次のS式が実行されます．

```
(if condition
    (print "true")
  (print "false"))
```

この例では`condition`が`nil`でない場合にtrue，
`nil`の場合にfalseが表示されます．
`if`は単一のS式を実行するもののため，
複数のS式を実行させる場合は`(progn )`で囲うなどの工夫が必要です．
また`elseif`にあたるものがないため，複雑な制御構造を書けません．
(C言語で言うところの三項演算子に近いものです．)
そこで，条件分岐は主に前述の3つを使います．


## 繰り返し

### dotimes

いわゆる`for`文です．

```
(dotimes (i 10)
  (print i))
```

このようにすると，
`i`が`0`から`9`まで変化しながら実行されます．


### dolist

いわゆる`foreach`文です．

```
(dolist (i (list 0 1 2 3))
  (print i))
```

とすると，
`i`にはリストの先頭から順番に値が入ります．


### while

条件式が`nil`でない間実行されます．

```
(setq i 0)
(while (< i 10)
  (print i)
  (setq i (+ 1 i)))
```

とすると，`dotimes`の例とほぼ等価になります．
(この例では，`i`はグローバル変数となります．)


### do-until-key

`do-until-key`は特殊な繰返し文で，
条件式を持たず，キー入力(enter)を割り込みとして受けると終了します．

```
(do-until-key
 (print "press ENTER to stop"))
```

とすると，Enterキーが押されるまで実行され続けます．


## その他の制御構造

### return

`dotimes`,`dolist`,`while`から抜けるときは，
`return`を使います．

```
(dotimes (i 10)
  (when (= i 5) (return nil))
  (print i))
```

### return-from

`return-from`は，ある名前で示されたブロック，
関数やクラスメソッドから抜けるときに使われます．
詳しくは後述します．


## 関数・クラス

### 関数

関数定義は`defun`で行います．

```
(defun plus (a b)
  (+ a b))
```

として，

```
(plus 1 2)
```

とすると`3`が返ってきます．
**関数は最後に評価された値を返します．**


ローカル変数は`let`を使って定義します．

```
(defun minus (a b)
  (let (c)
    (setq c (- a b))))
```

とすると，`let`外から`c`にアクセスすることはできません．
また，`let`の中で複数のローカル変数を定義するときは，
`()`の中に続けて記述してください．

一方，強制的に関数を抜ける際には，前述の`return-from`を使います．

```
(defun plus-minus (a b c)
  (let (d e)
    (setq d (- a b))
    (when (< d 0)
      (print "a < b, abort.")
      (return-from plus-minus nil))
    (setq e (- (+ a b) c))
    e)  ;; letの返り値がe
  )
```
として，
```
(plus-minus 1 2 3)
```
と
```
(plus-minus 2 1 3)
```
の違いを確かめてみてください．


### optional, key

関数の引数にデフォルト値を与えることができます．

```
(defun negate (a &optional (b 0))
  (- b a))
```
とすると，
```
(negate 10)
```
と
```
(negate 10 5)
```
のどちらも使えます．

```
(defun negate (a &key (b 0))
  (- b a))
```
とすると，

```
(negate 10)
```
と
```
(negate 10 :b 5)
```
が使えます．

どちらの場合も後ろの引数を省略できますが，
`&optional`の場合は単純に引数を追加することで，
`&key`の場合は引数の名前に`:`を追加して指定することで
引数を与えることができます．


### クラス

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

クラス定義は，`defclass`を用います．
`:super`に継承する元のクラスを指定します．
ここでは`propertied-object`というクラスを使っています．
`:slots`はメンバ変数です．

メソッド定義は`defmethod`で行います．
書式は基本的に関数と同じです．
`:init`がコンストラクタとなります．
コンストラクタは最後に`self`を返します．
その他のメソッドは`:`を先頭につけて定義していきます．


クラスオブジェクトの作り方です．
```
(setq *me* (instance myclass :init "John Smith" 30))
```
とすると，オブジェクトが生成されます．

このオブジェクトのメソッドをよんでみましょう．
```
(send *me* :myname)
```
とすると，先程与えた`"John Smith"`が返ってきます．

```
(send *me* :myname "Alan Smithee")
```
とすると，メンバ変数`myname`に`"Alan Smithee"`が代入されます．
このように，セッターとゲッターを同じメソッドで共有できます．
