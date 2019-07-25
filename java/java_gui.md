# History

Abstact Windows Toolkit (AWT) was the first GUI toolkit bundled with Java. The basic AWT library deals with user interface elements by delegating their creation and behavior to the native GUI toolkit on each target platform.

Built on top of AWT, by painting user interface elements onto blank windows, Swing was introduced. we say “Swing” when we mean the “painted” user interface classes, and we say “AWT” when we mean the underlying mechanisms of the windowing toolkit, such as event handling.

In 2007, Sun Microsystems introduced an entirely different user interface toolkit, called JavaFX, as a competitor to Flash and replace  ugly Swing. It ran on the Java VM but had its own programming language, called JavaFX Script. The language was optimized for programming animations and fancy effects. Programmers complained about the need to learn a new language, and they stayed away in droves. In 2011, Oracle released a new version, JavaFX 2.0, that had a Java API and no longer needed a separate programming language. Starting with Java 7 update 6, JavaFX has been bundled with the JDK and JRE.


# Frames

A top-level window is called a _frame_ in Java. `JFrame` in Swing extends `Frame` in AWT. The JFrame is one of the few Swing components that is not painted on a canvas. Thus, the decorations (buttons, title bar, icons, and so on) are drawn by the user’s windowing system, not by Swing.

Most Swing component classes start with a "J" while many classes without "J" as the initial belong to AWT.

The Swing classes are placed in the `javax.swing` package, where `javax` indicates a Java _extension_ package, not a core package, although it is now present in every Java implementation since version 1.2. By default, a frame has a size of $0 \times 0$ pixels.

All Swing components must be configured from the event dispatch thread, the thread of control that passes events such as mouse clicks and keystrokes to the user interface components. Simply constructing a frame does not automatically display it. Frames start their life invisible. That gives the programmer the chance to add components into the frame before showing it for the first time. To show the frame, the main method calls the `setVisible` method of the frame. After scheduling the initialization statements, the main method exits. Note that exiting main does not terminate the program—just the main thread. The event dispatch thread keeps the program alive until it is terminated, either by closing the frame or by calling the System.exit method. Components have properties that can be manipulated through a pair of getter and setter
