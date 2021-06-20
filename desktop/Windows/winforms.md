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

A typical modal dialog is more likely ot hide both the minimize and the maximize boxes, show the help button, hide the icon and not show up in the task bar.

`Form.Modal` determines if it's modal. 

It is generally considered bad practice for anything except the main form to directly update the data when the user presses OK or Apply.

`.ShowDialog` returns a value that specifies the condition under which the form was closed, allowing the form client code to respond accordingly. To return a value other than Cancel from `ShowDialog`, set the `.DialogResult` property before closing the form (which will automatically close the dialog). It is possible to assign certain buttons as predefined buttons such as `Cancel` or `OK` so that the return result is set automatically.

For modeless forms, setting `DialogResult` doesn't automatically close it. Modeless forms acting as dialogs usually have `Apply` and `Close` buttons so data entered into the form can be used before th modeless form even goes away. Use events to expose the `Apply` button pressing.

Every contorl has `CausesValidation` set to true by default. Set this to false for  `Cancel` or `Close` buttons.

# Layout

## Fixed Layout

- `SnapToGrid`: snapping to a predefined grid whose dimensions can be set in the same location 

- `SnapLines`


- `Location` and `Size` 

- `Padding`: the internal distance in pixels from the edge of a form or control's client area that child controls can't intrude on. It's implemented on the base `Control`.

- `Margin`: the spacing between adjacent controls. All controls implement the `Margin` property except for `Form`.

- `z-order` (the highest is zero): all controls in the same container are logically ordered in a vertical stack, defined by z-orders and is implicitly determined by the order in which controls are added to their container. Controls are rendered in last-to-first order but the z-order is calculated in first-to-last order.

- _tab order_: the sequence in which a user cn navigate through them at runtime. `TabStop`, `TabIndex`. A control's tab index is relative to other controls within the container. 

## Dynamic Layout

- _anchoring_: preserving the distance between the edge of a control and the adjacent edge of a control's container. A control is resize if the user has selected two opposing edges.

it's common to anchor a modal dialog's OK and Cancel buttons to only the buttom and right edges.

### Docking

Identify the edge that a control is required to stick itself to.

Docking is done in reverse z-order priority.

`DockStyle.Fill`: set or unset to move a control around 

`ToolStrip` allows users to dynamically redock them at runtime.

### Autoscaling and Automatic Resizing

`AutoSize`, `AutoSizeMode`: so that a container can be resized to show the hidden parts of the resized or moved control. Automatic resizing is triggered when the edges of a resizing or repositioning control come close enough in proximity to the host container's right and bottom edges that heir margins overlap.

`AutoScale` set to true: not only increases the size of all fonts displayed on the form and its title bar, but also increases the form's overall size as well as increases control sizes and changes locations to maintain the same proportional sizing and spacing. The scale factor is not necessarily based on font (also on dpi). `AutoScaleDimension` is the default dimension.

## Container Controls

### Splitting 

- `SplitContainer`: a bar that seperates two controls.

### Grouping

Blank canvases from a layout POV.

- `GroupBox`

- `Panel`: a group box without labels and frame, looks like a _subform_.

- `TabPages`: with a tab at the top

### Flow Layout

Client controls are Consistently ordered collapsing and expanding as their container resizes, and the layout is being re-arranged.
 
- `FLowLayoutPanel`: `.FlowDirection`. The rendering order is the same as the order in which the controls are added.

### TableLayoutPanel

a tabular layout built on columns, rows and cells.

### Optimization

- `SuspendLayout()`

- `ResumeLayout()`

Placed in `InitializeComponent` for any form that has at least one control. The SuspendLayout and ResumeLayout methods are used in tandem to suppress multiple Layout events while you adjust multiple attributes of the control.

# Drawing `System.Drawing`

primitives: colors, brushes (filling the interior of a shpae), pens (drawing the edge of a shape), fonts.

The `System.Drawing.Graphics` class provides the abtract surface on which things are drawn. 

Windows asks a form to redraw newly uncovered content via the `Paint` event (`PaintEventArgs`)

`Form.Invalidate()` invalidate the rendering ans requests redraw. `Form.Update()` sends the Paint event and `Form.Refresh()` do both at once. By default only the changed area is redrawn, which sometimes leads to strange behavior. A form has several drawing style, `ResizeRedraw` causes windows to redraw the entire client area whenver the form is resized.

## `System.Drawing.Color`

RGB and opacity

Some of the colors already have a well-known name. The `KnownColor` enumeration has 33 values describing the current colors assigned to various parts of the Windows UI. Use them through `SystemColors`

## `System.Drawing.Brush`

- SolidBrush: a color used to fill in the shape being drawn.

- TextureBrush: constructed with an image such as a bitmap. By default it is used repreated to tile the space inside the shape being drawn.

- Hatch Brushes: fill space using one of several builtin two-color patterns, where the two colors are used to draw the foreground and the background of the pattern.

- Linear gradient brushes: 

```csharp
Rectangle area = new Rectangle(x, y, width, height);
Color startColor = Color.DarkBlue;
Color endColor = Color.White;
using (Brush brush = new LinearGradientBrush(area, startColor, endColor, 45.0f))
{
    //
}
```

- path gradient brushes

## `System.Drawing.Pen`

used to frame shapes. A pen has _width_, _color or _brush_, _start_ and _end_ cap styles and a _dash_ style/pattern (-.-.-).


## Shape, ...

TODO

# Drawing Text

typeface: Arial, font style: Bold

point: 1/72 inch no matter what device it's drawn on.

DPI: dot per inch

design units: 

`System.Drawing.Font`; `System.Drawing.FontFamily`. Also there is the `System.Drawing.Text.GenericFontFamilies` enum.

`System.Drawing.SystemFonts`: same for all specific Windows UI items.

`.DrawString()`, `System.Drawing.StringFormat`

TODO

# Components

a class of `System.ComponentModel.IComponenet`. Components are reusable, cofigurable classes. they do not provide native support for user interaction via a form. Intrisic support for UI interaction is provided by controls.

Custom components are a great way to package reusable Windows Form code that typically doesn't have a UI or handle user input. 

Creating a custom component is much like creating a standard class, except that components come with built-in resource management and are the cornerstone of the Windows Forms design-time experience.

e.g. Timer, `OpenFileDialog`, `ErrorProvider`, `Tooltip`

## Custom Components

TODO

# Controls

Reusable class whose main purpose is to interact with users on behalf of a container (a form or a container control). A control is derived from `System.ConponenetModel.Component`.

`Application.EnableVisualStyles()` enables themed rendering.

Tool strips offer additional rendering modes that can override the current theme using special rendering classes.

Some controls (_owner-drwan controls_) provides events that allow a control's onwer to take over the drawing from the control. The `ControlPaint` helper class which has static members for drawing common control.

The most popular form of UI reuse for a control is simple containment. `System.Windows.Forms.UserControl` is a way to contain a set of other controls for reuse as a set, producing a kind of subform.

# Resource

When a file is marked as an Embedded Resource, it becomes embedded in the assembly’s set of manifest resources. The manifest of an assembly is composed of a set of metadata that describes part of the assembly. Part of that metadata is the name and data associated with each embedded resource. Manifest resources serve as the needed foundation for strongly typed resources. Manifest resources are embedded with no type information.

## Strongly Typed Resources

Application resouces files `.resx` meploy ResX to persist resource type information. Building the project causes the `.resx` data to be embedded as _nested resources_, which are resources grouped into a named container `.resources`.

Either read the `.resx` directly (`ResXResourceReader`) or use the compiled `.resouces` files (`ResourceReader`). However, neither supports random access. Resource Manager is recommended.

`ResXFileCodeGenerator` automatically generates a storngly typed resource classes, does pretty much the same work as a programmer would do.

There are also default project resources and automatically associated resources

## Resource Localization

.NET supports application-sepcific localization via culture-specific resource assemblies deployed in satellite assemblies (separate assembies that can be found near the location of the main assembly). The resource embedded in the main assembly are considered _culture neutral_. Culture-specific resources are embedded into a project on a per-form basis, with each form being responsible for one or more sets of culture- and language-specific localized data sets

# Applications

`System.Windows.Forms.Application` provides various static methods and properties to manage an application.

## App Lifetime

To fully initialize a WinForms app and start it routing WinFroms events, invoke `Application.Run`

- without arguments if other means have already been used to show an initial UI; the application runs until explicity told to stop (call `Application.Exit()`), even when all its forms are closed. Typically, this is done only when the app needs a secondary UI thread.

- on the main form. It shows the main form and doesn't return until the main form closes.

- on an `ApplicationContext`, useful if a custom context is needed. `ApplicationContext` detects main form closure and exits the application as appropriate. Some of the methods (`OnMainFormClosed`) can be overriden to modify the application behavior. 

## Application Events

During the lifetime of an application, several key application events `Idle`, `ThreadExit` (UI thread exits), `ApplicationExit` (when the last UI thread goes away) are fired by the `Application` object. These are commonly added inside the `Main` function.

To replace the WinForms unhandled-exception dialog, use an exception handler to `Application.ThreadException`.

## Single-Instance Application

# Settings

Most real-world applications rely on information forom the environment around them.

## Compile-Time Settings

- _Assembly Information_: some of them are available from the `Application` class.

## Run-Time Settings

The `Application` class exposes several run-time environment settings. More environment settings re in `System.Environment`. Runtime access to shell settings is exposed by `System.Windows.Form.SystemInformation`; sysinfo about the screen is in `Systme.Windows.Forms.Screen`. `Microsoft.Win32.SystemEvents`

## Application, User and Roaming-User settings

_setting_: name, type, value and scope (whether a setting is an application or user seting)

_setting file_: `Settings.setings`

_application configuration file_: `app.config`. The `ConfigurationManager` class and the classes in `System.Configuration` wra the config file into a settings-oriented abstraction.

- _user configuration file_: `user.config`. Unmodified user settings are copied fro mthe dfeaults locatd in `app.exe.config`

## Strongly Typed Settings

Every setting file added to a project is used to generate an additonal strongly typed settings class. `ApplicationSettingsbase` 

# Data Bindings

## DataGridView

A grid in which data is displayed and represents a specific collection of data. The grid determines the properties of the collection automatically using `System.Reflection`.

### Columns and Rows

If `AutoGenerateColumns` is set to `true`, the columns for the grid are generated whenever the DataSource or DataMember property value changes. Autogeneration can be disabled and the columns can be assigned manually.

Cells and bands are elements of a grid, inherited from `DataGridViewElement`

- `DataGridViewBand`: A linear collection of cells, the base class

- `DataGridViewColumn`: `DataPropertyName` binds the member object's property to a column. Column cells can be buttons, check boxes, comboboxes, clickable links or text boxes, depending on the the actually used derived class.

- `DataGridViewRow`: 

- `DataGridViewCell` 


# Various Controls

## ToolStrip

Various bars with items on it.

### Menubar

- `MenuStrip`

- `ContextMenuStrip`: like the one that pops out when you click a text box.

## Toolbar

- `ToolStrip`

## Statusbar

`Statustrip`: items include `ToolStripStatusLabel`, `ToolStripProgressBar`, `ToolStripStatusLabel`, `ToolStripSplitButton`
