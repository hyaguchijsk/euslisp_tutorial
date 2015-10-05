# EusLisp tutorial

## About

EusLispは，
ロボットプログラミングのための様々な機能を持ったLispライク処理系です．

このチュートリアルは，
他の言語ではある程度経験があるが，
EusLispについては全くの初心者に向けて記述しています．

言語仕様や一歩進んだ使い方について，より詳しく知りたい時は，
[本家のドキュメント][euslisp-doc]を参照してください．

ロボット工学に関する知識はロボット工学の教科書を参照してください．

[euslisp-doc]: http://euslisp-docs.readthedocs.org/en/latest/


### EusLispの特徴

- ロボットプログラミングに特化した開発環境です．
- その根底には充実した三次元幾何ライブラリがあります．
- OpenHRP,ROSとのブリッジを持ち，それらに対応した多数のロボットが動かせます．


## Install

ROSがインストールされていることが前提となります．
[ROS/Installation]を参考にインストールしてください．
また，[wstool],[catkin-tools]もあわせてインストールすると便利です．

```bash
sudo apt-get install python-wstool python-catkin-tools
```

インストール終了後，catkinのワークスペースを作ってください．
例えば以下のようにします．

```bash
mkdir ~/catkin_ws
cd ~/catkin_ws
wstool init src
catkin init
catkin build
source ~/catkin_ws/devel/setup.bash
```

### ROSをインストールしてあり，手動でインストールする場合

`ros-<ROS_DISTRO>-roseus`というパッケージが提供されています．
indigoの場合，以下のようにインストールできます．

```bash
sudo apt-get install ros-indigo-roseus
```

また，本チュートリアルで紹介するロボットモデルを利用するためには，
[rtmros_common]のインストールを行なってください．

```bash
sudo apt-get install ros-indigo-hrpsys-ros-bridge ros-indigo-euscollada ros-indigo-pr2eus
```

および，catkinのワークスペースに[rtmros_tutorials]をダウンロードしてください．
```bash
cd <catkin_ws>/src
wstool set rtm-ros-robotics/rtmros_tutorials https://github.com/start-jsk/rtmros_tutorials.git --git
wstool update rtm-ros-robotics/rtmros_tutorials
cd rtm-ros-robotics/rtmros_tutorials/hrpsys_ros_bridge_tutorials
catkin bt
```
[ROS/Installation]: http://wiki.ros.org/ROS/Installation
[wstool]: http://wiki.ros.org/wstool
[catkin-tools]: https://catkin-tools.readthedocs.org/en/latest/
[rtmros_common]: http://wiki.ros.org/rtmros_common/Tutorials/WorkingWithEusLisp
[rtmros_tutorials]: https://github.com/start-jsk/rtmros_tutorials


### インストーラを利用して最初からインストールする場合

[jsk_common]を利用することで，
ros, hrpsys, euslispを含む
様々なロボットプログラミングツールをインストールできます．

[jsk_common]: https://github.com/jsk-ros-pkg/jsk_common


## Basic Usage

### インタプリタ起動

euslispはインタプリタを用いて動作させることが基本です．
** rosをインストールした環境であれば，通常はroseusを使いましょう． **

- 最低限の機能を起動

```bash
eusgl
```

- irteus拡張 + GUIを読み込んで起動

```bash
irteusgl
```

- irteusgl + rosインタフェースを読み込んで起動

```bash
roseus
```

euslispのインタプリタはreadlineに対応していないため

- 履歴をたどることができない
- カーソルキーが使えない

という問題点があります．
これらを解決するため，emacs shellの利用を推奨しています．
emacsを起動し，`M-x shell`とすると，emacs shellが利用できます．
emacs shellの上では，`M-p`で履歴をたどれるほか，
カーソルキーで移動できる，
バッファに実行結果をためておけるなど，多数の利点があります．


### インタプリタの使い方

euslispインタプリタは
** 大小文字の区別なし，一番外側の`()`はなくても良い **
という仕様になっています．

例えば

```
(+ 1 2)
```

と

```
+ 1 2
```

は同じです．ただし，入れ子になる場合は`()`を書きましょう．

```
+ (- 3 2) 1
```

また，変数への代入は`setq`を使います．

```
(setq a (+ 1 2))
```

前回の実行結果を参照するには`*`を使いましょう．

```
(+ 1 2)
```

としたあとに，

```
(setq a *)
```

とすると，前回実行結果の`3`が代入されます．
同様に`**`で前々回，`***`で前々々回の結果を参照できます．


終了方法は`exit`です．

```
(exit)
```


### プログラムの書き方

プログラムは拡張子`.l`のファイルに記述していきます．
euslispはインタプリタに読ませるスクリプトとしてこのファイルを解釈します．
コンパイルやエントリーポイント(いわゆる`main`関数)はなく，
ファイルの中身をそのままインタプリタ上でタイプすることと同じです．

例えば，`test.l`というファイルにプログラムを記述した場合，

```bash
roseus test.l
```

としてインタプリタごと起動するか，
インタプリタを先に起動しておいて，その上で
```
(load "test.l")
```

としましょう．
