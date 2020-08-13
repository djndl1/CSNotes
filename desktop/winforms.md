# Basic Concepts

- form: a slate

- control: a discrete UI element that displays data or accepts data input, added to forms and responds to user actions. 

The action generates and event, and an event handler reacts to the event.

# Controls

## Arranging controls

- snap lines

- anchoring: automatic layout control

- docking: stick any control on the edge of its container.

- splitting

## a user control as a container

## Application Settings

app.config stores only default user settings and applicaton settings. Altered settings are typically stored in `Document and Settings`.

## Resources

Various multimedia resource and strings. String resources are embedded in `Resources.resx`. Other resources are copied into the `Resources` directory. Resources are exposed as strongly typed properties of the Resource class.

## Multithreaded UI

`BackgroundWorker`

# Forms

## Show Forms

1. `Form.Show()`: modeless show. Does not establish an implicit owner-owned relationship. The owner can be explicitly set.  The modeless owned form always apears on top of the owner form.

2. `Form.ShowDialog()`: modal. The new form -> owned form, the form that opened it -> owner form. There is no builtin way for the owned form to call back to or query the form that opened it.

## Lifetime

```
+--------------+
|Construction  |  InitializeComponent() auto-generated
+------|-------+  Add customization in the constructor
       |
       v
+------|-------+                                +--------------------+
|    Load      | Load event for final           |    Deactivate      |
+------|-------+ initialization                 +---------|----------+
       |                                                  ^
       v                                                  |
+--------------+ Activated event                +---------|---------+
|  Activated   |                                |    FormClosed     | for serializing the properties of
+--------------+                                +---------|---------+ a form to be used when it's reopened
       |                                                  ^
       v                                                  |
+------|-------+                                +---------|---------+
|    Shown     +------------------------------->+    FormClosing    |    Handling finalization
+-------|------+                                +---------|---------+
        |    Shown event                                  ^
        |                                                 |
        |                                                 |
        |                                                 |
        |     +------------+        +--------------+      |
        +---->+ Deactivate +-------->  Activated   +------+
              +------------+        +--------------+

               Deactivate event         When resuming
               when swtiching away
```

## Size, Location

`Size` (`struct Size` and `Location` properties (in `struct Point`).  `PointF` is for drawing.

`StartPosition`, 

Resize, SizeChanged, ResizeBegin, ResizeEnd events;

Normal windows are drawn lowest z-order to highest.
