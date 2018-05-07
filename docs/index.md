# EusLisp tutorial

## About

EusLisp is a Lisp dialect designed for robot programming.

This tutorial is aimed to ones that already have some experience with other programming languages, but are completely new to EusLisp.

Ones that want to learn more about the inner processes or have more detailed explanations should refer to the [original documentation][euslisp-doc].

For more information on robotic engineering, please refer to an appropriate textbook.

[euslisp-doc]: http://euslisp-docs.readthedocs.org/en/latest/


### EusLisp main features

- Language specialized in robot programming.
- Solid 3D modeling library.
- Is applicable to any robot supported by ROS and/or OpenHRP.

## Install

To install ROS, please follow instructions on the following link: [ROS/Installation].
It is recommended to also install [wstool] and [catkin-tools].

```bash
sudo apt-get install python-wstool python-catkin-tools
```

When finished with installation, create a catkin workspace like in the following.

```bash
mkdir ~/catkin_ws
cd ~/catkin_ws
wstool init src
catkin init
catkin build
source ~/catkin_ws/devel/setup.bash
```

### Manual installation from ROS

Packages like `ros-<ROS_DISTRO>-roseus` are provided. For instance, in indigo EusLisp can be installed as follows.

```bash
sudo apt-get install ros-indigo-roseus
```

In order to run the robot model used by this tutorial, the install of [rtmros_common] is also necessary.

```bash
sudo apt-get install ros-indigo-hrpsys-ros-bridge ros-indigo-euscollada ros-indigo-pr2eus
```

Also download [rtmros_tutorials] into the catkin workspace previously created.

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


### Installer for ROS, HRPSYS and EusLisp

[jsk_common] makes possible to install a variety of tools for robot programming, including ROS, hrpsys and EusLisp.

[jsk_common]: https://github.com/jsk-ros-pkg/jsk_common


## Basic Usage

### Launching the Interpreter

EusLisp is mainly used from the interpreter.
** With ROS installed, the common is to use "roseus". **

- Minimal usage

```bash
eusgl
```

- Launch with irteus expansion + GUI

```bash
irteusgl
```

- Launch with irteusgl + ROS interface

```bash
roseus
```

Since the EusLisp interpreter does not support readline, it is not able to:

- Navigate through the history
- Use cursor keys.

In order to avoid this problem, it is recommended to use emacs shell.
After starting emacs, doing `M-x shell` will launch emacs shell.
In emacs shell, the history can be navigated with `M-p`, the cursor keys can be used, results can be accumulated in the buffer, besides many other benefits.


### Using the Interpreter

In EusLisp interpreter,
** there is no difference between capital and lowercase letters, and the outer parenthesis `()` can be omitted. **

For example:

```
(+ 1 2)
```

Is equal to:

```
+ 1 2
```

However, is is necessary to use inner parenthesis.

```
+ (- 3 2) 1
```

Variables can be set with `setq`.

```
(setq a (+ 1 2))
```

`*` can be used to access the previous result.

For example in the following `*` is substituted with the previous result, that is, `3`.

```
(+ 1 2)
(setq a *)
```

`**` and `***` work similarly, accessing the 2nd last result and 3rd last result.


Session can be finished with `exit` or `Ctrl+D`.

```
(exit)
```


### Writing programs

EusLisp programs conventionally use the extension `.l`.
Loading one of these files in the interpreter is equivalent to typing all of its contents. No compile is performed when loading, and there is no entry point (main functions).

A file `test.l` can be loaded by starting the interpreter with the file name as arguments

```bash
roseus test.l
```

or by calling the load function from the interpreter.

```
(load "test.l")
```
