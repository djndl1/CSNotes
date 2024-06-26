# History

Abstact Windows Toolkit (AWT) was the first GUI toolkit bundled with Java. The basic AWT library deals with user interface elements by delegating their creation and behavior to the native GUI toolkit on each target platform.

Built on top of AWT, by painting user interface elements onto blank windows, Swing was introduced. we say “Swing” when we mean the “painted” user interface classes, and we say “AWT” when we mean the underlying mechanisms of the windowing toolkit, such as event handling.

In 2007, Sun Microsystems introduced an entirely different user interface toolkit, called JavaFX, as a competitor to Flash and replace  ugly Swing. It ran on the Java VM but had its own programming language, called JavaFX Script. The language was optimized for programming animations and fancy effects. Programmers complained about the need to learn a new language, and they stayed away in droves. In 2011, Oracle released a new version, JavaFX 2.0, that had a Java API and no longer needed a separate programming language. Starting with Java 7 update 6, JavaFX has been bundled with the JDK and JRE.


# Frames

A top-level window is called a _frame_ in Java. `JFrame` in Swing extends `Frame` in AWT. The JFrame is one of the few Swing components that is not painted on a canvas. Thus, the decorations (buttons, title bar, icons, and so on) are drawn by the user’s windowing system, not by Swing.

Most Swing component classes start with a "J" while many classes without "J" as the initial belong to AWT.

The Swing classes are placed in the `javax.swing` package, where `javax` indicates a Java _extension_ package, not a core package, although it is now present in every Java implementation since version 1.2. By default, a frame has a size of $0 \times 0$ pixels.

All Swing components must be configured from the event dispatch thread, the thread of control that passes events such as mouse clicks and keystrokes to the user interface components. Simply constructing a frame does not automatically display it. Frames start their life invisible. That gives the programmer the chance to add components into the frame before showing it for the first time. To show the frame, the main method calls the `setVisible` method of the frame. After scheduling the initialization statements, the main method exits. Note that exiting main does not terminate the program—just the main thread. The event dispatch thread keeps the program alive until it is terminated, either by closing the frame or by calling the System.exit method. Components have properties that can be manipulated through a pair of getter and setter

# Display Information in a Component

Frames are designed to be containers for components, such as a menu bar and other use interface elements. Any componenets added to a frame are automatically placed into the content pane.

`JComponent` is the base calss for all Swing components except top-level containers.

```java
class MyComponent extends JComponent
{
   public void paintComponent(Graphics g)
   {
      code for drawing
   }
   
   public Dimension getPreferredSize()
   { 
      return new Dimension(DEFAULT_WIDTH, DEFAULT_HEIGHT); 
   }
}
```

where `Graphics` remembers a collection of settings for drawing images and text. Each time a window needs to be redrawn, no matter what the reason, the event handler notifies the component. This causes the paintComponent methods of all components to be executed automatically.

Override the getPreferredSize method and return an object of the Dimension class with the preferred width and height.

Methods such as `paintComponent` automatically receive an object of the `Graphics2D` class. The Java 2D library provides a set of shape classes and supplies two versions of each shape class, one with `float` coordiantes and the other with `double` coordinates, both of which are static inner class packaged into an abstract class:

```java
Rectangle2D.Float
Rectangle2D.Double
```

Simply use the `Double` shape classes to avoid dealing with `float` values altogether. Only use the `Double` version to save memory

```
+----------+                    +----------+
|  Point2D |                    |          |
+-----|----+                    |   Shape  |
      ^                +- - - ->+          |
      |                |        +------|---+
      |                                ^
+-----|----+           |               |
|x  Point x|       +---|-----+   +-----|-------+
+----------+       |  Line2D |   | Rectangular |
                   +---------+   |    Shape    |
                                 +------^------+
                                        |
                                        |
                             +----------|-----------+
                             |                      |
                      +------|------+      +--------|-------+
                      |  Ellipse2D  |      |  Rectangular2D |
                      +-------------+      +------|---------+
                                                  ^
                                                  |
                                           +------|-----+
                                           |xRectangle x|
                                           +------------+
```

The `setPaint()` method of the Graphics2D class lets you select a color that is used for all subsequent drawing operations on the graphics context. `.fill()` fill the interior of closed shapes. A custom color can be created by creating a `Color` object. To set the background color use the `setBackground()` of the `Component` class. There is also a `setForeground()` method.

`GraphicsEnvironment.getAvailableFontFamilyNames()` finds out which fonts are available on a particular computer. The AWT defines five logical font names:

- `SansSerif`

- `Serif`

- `Monospaced`

-  `Dialog`

- `DialogInput`

which are always mapped to some fonts that actually exist on the client machine.

To use a font, create a `Font` object. Use the `deriveFont()` method to get a font of the desired size.

# Event Handling

In AWT, _event sources_ (such as buttons or scrollbars) have methods that allow to register _event listeners_ (that implements a listener interface), objects that carry out the desired response to the event. When an event listener is notified about an event, information about the event is encapsulated in an event object. In Java, all event objects ultimately derive from the class `java.util.EventObject`. The event source sends out event objects to all registered listeners, which take respective actions to the event.

```java
ActionListener listener = . . .;
var button = new JButton("OK");
button.addActionListener(listener);
```

where `listener` inplements

```java
class MyListener implements ActionListener
{
   . . .
   public void actionPerformed(ActionEvent event)
   {
      // reaction to button click goes here
      . . .
   }
   
}
```

Each of the AWT listener interfaces that have more than one method comes with a companion adapter class that implements all the methods in the interface but does nothing with them. You extend the adapter class to specify the desired reactions to some, but not all, of the event types in the interface.

the `Action` interface, which extends `ActionListener`  is an object that encapsulates a description of the command (as a text string and an optional icon) and parameters that are necessary to carry out the command (such as the requested color).

```java
void actionPerformed(ActionEvent event)
// enable or disable the action
void setEnabled(boolean b)
boolean isEnabled()
// store and retrieve arbirary name/value pairs
void putValue(String key, Object value)
Object getValue(String key)
// do something if the listener changes
void addPropertyChangeListener(PropertyChangeListener listener)
void removePropertyChangeListener(PropertyChangeListener listener)
```

There is a class `AbstractAction` that implements all methods except for actionPerformed.

```java
public class ColorAction extends AbstractAction
{
   public ColorAction(String name, Icon icon, Color c)
   {
      putValue(Action.NAME, name);
      putValue(Action.SMALL_ICON, icon);
      putValue("color", c);
      putValue(Action.SHORT_DESCRIPTION, "Set panel color to " + name.toLowerCase());
   }
   public void actionPerformed(ActionEvent event)
   {
      Color c = (Color) getValue("color");
      buttonPanel.setBackground(c);
   }
}
```

A `KeyStroke` object is the source of a key stroke event. There are three input maps:

- `WHEN_FOCUSED`: this component has keyboard focus;

- `WHEN_ANCESTOR_OF_FOCUSED_COMPONENT`: this component contains the component that has keyboard focus;

- `WHEN_IN_FOCUSED_WINDOW`: this component is contained in the same window as the component has keyboard focus;

Each component has three input maps and one action map.

```java
InputMap imap = panel.getInputMap(JComponent.WHEN_FOCUSED);
imap.put(KeyStroke.getKeyStroke("ctrl Y"), "panel.yellow");
ActionMap amap = panel.getActionMap();
amap.put("panel.yellow", yellowAction);
```

When the user clicks a mouse button, three listener methods are called: `mousePad` when the mouse is first pressed, `mouseReleased` when the mouse is released and `mouseClicked`. The `getX` and `getY` methods on `MouseEvent` gives the coordinates of the mouse pointer when the mouse was clicked. The `getClickCount` distinguishes between single, double and triple clicks. 

```java
public void mousePressed(MouseEvent event)
{
   current = find(event.getPoint());
   if (current == null) // not inside a square
      add(event.getPoint());
}
public void mouseClicked(MouseEvent event)
{
   current = find(event.getPoint());
   if (current != null && event.getClickCount() >= 2)
      remove(current);
}
```


There are separate `MouseListener` and `MouseMotionListener` dealing with mouse motion instead of mouse clicks. The cursor can be changed.

```java
public void mouseMoved(MouseEvent event)
{
   if (find(event.getPoint()) == null)
      setCursor(Cursor.getDefaultCursor());
   else
      setCursor(Cursor.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR));
}
```

The inheritance of the AWT events

```bash
                             +--------+
                             | Event  |
                             | Object |
                             +----|---+
                                  ^
                                  |
                                  |
                             +---------+
                             |AWT Event|
                             +---|-----+
                                 ^
                                 |
                                 |
    +-----------|----------------|------------+
    |           |                |            |
+---|---+  +----|-----+  +-------|--+   +-----|---+
| Action|  |Adjustment|  |Component |   |  Item   |
| Event |  |  Event   |  |  Event   |   |  Event  |
+-------+  +----------+  +-----^----+   +---------+
                               |
                 +-------------|----------|-----------+
                 |             |          |           |
                 |             |          |           |
            +----|----+   +----|---+  +---|---+  +----|---+
            |  Focus  |   |  Input |  | Paint |  | Window |
            |  Event  |   |  Event |  | Event |  |  Event |
            +---------+   +----|---+  +-------+  +--------+
                               ^
                         +-----|-----+
                         |           |
                     +---|---+   +---|---+
                     |  Key  |   | Mouse |
                     | Event |   | Event |
                     +-------+   +----^--+
                                      |
                                      |
                                      |
                                 +----|-----+
                                 |MouseWheel|
                                 |  Event   |
                                 +----------+

```

The event objects encapsulate information about the event that the event source communicates to its listeners. The AWT makes a useful distinction between low-level and semantic events. Low-level events are those events that make semantic events possible (dragging the mouse vs. scrolling the scrollbar). 

Common used event classes in `java.awt.event`

- `ActionEvent` (for a button click, a menu selection, selecting a list item, or Enter typed in a text field)

- `AdjustmentEvent` (the user adjusted a scrollbar)

- `ItemEvent` (the user made a selection from a set of checkbox or list items)
 
Five low-level event classes are commonly used:

- `KeyEvent` (a key was pressed or released)

- `MouseEvent` (the mouse button was pressed, released, moved, or dragged)

- `MouseWheelEvent` (the mouse wheel was rotated)

- `FocusEvent` (a component got focus or lost focus)

- `WindowEvent` (the window state changed)

# Preferences API

TODO

# Swing

TODO
