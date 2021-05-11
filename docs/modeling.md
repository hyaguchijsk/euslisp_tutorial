# Modeling

Let's create a model on irteus expansion.


## Geometrical model (bodyset)

`bodyset` is the lowest class able to gather multiple `body` instances into a single object.
Using this, the object described at [geo_coding](geo_coding.md) can be rewritten as follows.

```
(setq *stick* (make-cylinder 10 100))
(send *stick* :set-color :red)
(setq *body* (make-cube 50 100 50))
(send *body* :translate (float-vector 0 0 100))
(send *body* :set-color :yellow)
(send *stick* :assoc *body*)
(setq *hammer*
      (instance bodyset :init (make-cascoords)
                :bodies (list *stick* *body*)))
(objects (list *hammer*))
```

## Multiple link model (cascaded-link)

`cascaded-link` is the base for describing a multiple link model.
In the `samplerobot` example (refer [robot_coding](robot_coding.md)), objects given by `(send *sr* :rarm)` are of type `cascaded-link`.

`cascaded-link` adds joint angle information from `joint` class to the link information from `bodyset-link` class.

### bodyset-link

`bodyset-link` is the lowest class able to describe multiple link structures with joints.
Since it is a child of `bodyset` class, it can be created the same way.

### joint

`joint` is the lowest class able to represent joints, as the name points out.
Main types are `rotational-joint` and `linear-joint`.
During `:init`, it is possible to join links by setting `:parent-link` and `:child-link`.
Please note that the reference is the `child-link` coordinates.

### Creating multiple link models

Create a class inheriting `cascaded-link` and model each link.

At initialization (`:init`), the following steps are taken.

- create a `bodyset-link` objects
- create `joint` objects between `bodyset-link` instances
- `:assoc` links in order, starting from `self` and the root link
- assign the list of `bodyset-link` objects to the member variable `links`
- assign the list of `joint` objects to the member variable `joint-list`
- at last, call `:init-ending`

For example, let's create a hand that moves the previously created hammer.

```
(defclass hammer-hand
  :super cascaded-link
  :slots (hammer hand j0))
(defmethod hammer-hand
  (:init
   (&rest args)
   (send-super* :init args)
   (let (hammer-stick hammer-body hammer-stick2 hand-co)
     ;; create bodyset-links
     (setq hammer-stick (make-cylinder 10 100))
     (send hammer-stick :set-color :red)
     (setq hammer-body (make-cube 50 100 50))
     (send hammer-body :translate (float-vector 0 0 100))
     (send hammer-body :set-color :yellow)
     (send hammer-stick :assoc hammer-body)
     (setq hammer
           (instance bodyset-link :init (make-cascoords)
                     :bodies (list hammer-stick hammer-body)))

     (setq hammer-stick2 (make-cylinder 20 20))
     (send hammer-stick2 :set-color :green)
     (send hammer-stick2 :rotate (deg2rad 90) :y)
     (send hammer-stick2 :translate (float-vector 0 0 -10))
     (setq hand
           (instance bodyset-link :init (make-cascoords)
                     :bodies (list hammer-stick2)))

     ;; create joints
     (setq j0
           (instance rotational-joint :init
                     :parent-link hand
                     :child-link hammer
                     :axis :x))

     ;; :assoc from base
     (send self :assoc hand)
     (send hand :assoc hammer)

     ;; links and joint-list are declared as cascaded-link
     (setq links (list hand hammer))
     (setq joint-list (list j0))

     ;; do not forget!
     (send self :init-ending)
     )
   self)
  ;; joint accessor method
  (:hand (&rest args) (forward-message-to j0 args))
  )

(setq *hammer-hand*
      (instance hammer-hand :init))
(objects (list *hammer-hand*))
```
![modeling_hammerhand_00](figure/modeling_hammerhand_00.jpg)


Joint angle can be set with the following.
```
(send *hammer-hand* :hand :joint-angle 30)
```

![modeling_hammerhand_01](figure/modeling_hammerhand_01.jpg)


## robot-model class

Is is possible to define robots with multiple link models using `robot-model` class.

## scene-model class

`scene-model` is the basic class for modeling the surroundings.
This is done by creating a class that inherits `scene-model` and giving objects models and spots to `:objects`.

For example, a room model can be created with the following.

```
(defclass myroom-scene
  :super scene-model
  :slots ())
(defmethod myroom-scene
  (:init
   (&rest args &key (name "myroom"))
   (let (objs wall0 wall1 wall2 wall3 floorpanel walls
              table bed bed-base blaket pillow
              tablespot bedspot)
     (setq wall0 (make-cube 5300 150 2000)
           wall1 (make-cube 150 3000 2000)
           wall2 (make-cube 5300 150 2000)
           wall3 (make-cube 150 3000 2000)
           floorpanel (make-cube 5300 3300 100))
     (send floorpanel :translate (float-vector 0 0 -50))
     (send wall0 :translate (float-vector 0 1575 1000))
     (send wall1 :translate (float-vector 2575 0 1000))
     (send wall2 :translate (float-vector 0 -1575 1000))
     (send wall3 :translate (float-vector -2575 0 1000))
     (send floorpanel :set-color :brown)
     (send wall0 :set-color :gray)
     (send wall1 :set-color :gray)
     (send wall2 :set-color :gray)
     (send wall3 :set-color :gray)
     (send floorpanel :assoc wall0)
     (send floorpanel :assoc wall1)
     (send floorpanel :assoc wall2)
     (send floorpanel :assoc wall3)
     (setq walls (instance bodyset :init (make-cascoords)
                           :name "wall"
                           :bodies (list floorpanel wall0 wall1 wall2 wall3)))

     (setq table (make-cube 800 1800 700 :name "table"))
     (send table :translate (float-vector -2100 600 350))
     (send table :set-color :white)

     (setq bedbase (make-cube 2500 1400 350)
           blanket (make-cube 2100 1400 100)
           pillow (make-cube 300 500 100))
     (send bedbase :translate (float-vector 0 0 175))
     (send blanket :translate (float-vector -200 0 400))
     (send pillow :translate (float-vector 1050 0 400))
     (send bedbase :set-color :white)
     (send blanket :set-color :red)
     (send pillow :set-color :white)
     (send bedbase :assoc blanket)
     (send bedbase :assoc pillow)
     (setq bed (instance bodyset :init (make-cascoords)
                         :name "bed"
                         :bodies (list bedbase blanket pillow)))
     (send bed :translate (float-vector 1250 -800 0))

     (setq tablespot (make-cascoords :name "table-spot")
           bedspot (make-cascoords :name "bed-spot"))
     (send tablespot :translate (float-vector -1400 600 0))
     (send tablespot :rotate pi :z)
     (send bedspot :translate (float-vector 1250 100 0))
     (send bedspot :rotate -pi/2 :z)


     (setq objs (list walls table bed tablespot bedspot))
     (send-super :init :name name :objects objs)
     )
   self)
  )

(setq *myroom* (instance myroom-scene :init))
(objects (list *myroom*))
```

![modeling_myroom_00](figure/modeling_myroom_00.jpg)


The greatest benefit of `scene-model` is that it is able to access objects and spots by name.
In this case:

```
(send *myroom* :object "bed")
```

gives the bed object, and 

```
(send *myroom* :spot "bed-spot")
```

gives the spot near the bed.
