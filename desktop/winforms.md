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

`Size` represents the size of the entire window.`ClientSize` is the area for which the form is responsible. `.SetClientSizeCore()`

`.DesktopBounds` rectangle relative to the desktop for top-level windows. `.Bounds`: a rectangle of the form relative to the screen. `ClientRectangle`: a rectangle describing the client area of the form.

To translate these different points and rectangles, use `PointtoScreen`, `PointToClient`, `RectangleToScreen`, `RectangleToPoint`.

- `.StartPosition`: initial location of a form

`.MinimumSize`, `.MaximumSize` properties for a form. `WindowState` for maximizing, minimizing or normalizing the window.

Resize, SizeChanged, ResizeBegin, ResizeEnd events;

Normal windows are drawn lowest z-order to highest.


## Form Adornments

About titlebars, minimizebox, maximizebox, icon, helpbbutton.

## Form Transparency

mostly a parlor trick.


## Menus

- `MenuStrip`

- `ContextMenuStrip`: like the one that pops out when you click a text box.

## Toolbar

- `ToolStrip`

## Statusbar

`Statustrip`: items include `ToolStripStatusLabel`, `ToolStripProgressBar`, `ToolStripStatusLabel`, `ToolStripSplitButton`

## Multiple Document Interface (MDI)

MDI App contains a set of related window in a single frame. An MDI item has a parent `.IsMdiContainer` and a child `.MdiParent`.

## Visual Inheritance

for reuse

# Dialogs

A form that pops up in response to a user request for service. Modeless dialogs don't stop users from interacting with the rest of the app. Most XxxDialog components in WinForms support only modal activation using `ShowDialog`.

Some common dialogs:

- `ColorDialog`: pick a color exposed by the color property of type Systme.Drawing.Color.

- `FolderBrowserDialog`: pick a folder

- `FontDialog`

- `OpenFileDialog`

- `SaveFileDialog`

- `PageSetupDialog`, `PrintDialog`, `PrintPreviewDialog`.
