# Android Overview

Android's default user interface is mainly based on direct manipulation, using touch inputs that loosely correspond to real-world action. Android devices boot to the homescreen, the primary navigation and information "hub" on Android devices, typically made up of app icons and widgets. Along the top of the screen is a status bar, showing information about the device and its connectivity. An All Apps screen lists all installed applications, with the ability for users to drag an app from the list onto the home screen. A Recents screen lets users switch between recently used apps.

Applications ("apps"), which extend the functionality of devices, are written using the Android software development kit (SDK) and, often, the Java programming language. Java may be combined with C/C++, together with a choice of non-default runtimes that allow better C++ support.

Since Android devices are usually battery-powered, Android is designed to manage processes to keep power consumption at a minimum. When an application is not in use the system suspends its operation so that, while available for immediate use rather than closed, it does not use battery power or CPU resources.

Android's kernel is based on the Linux kernel's long-term support (LTS) branches. Android's variant of the Linux kernel has further architectural changes that are implemented by Google outside the typical Linux kernel development cycle, such as the inclusion of components like device trees, ashmem, ION, and different out of memory (OOM) handling. The flash storage on Android devices is split into several partitions, such as `/system` for the operating system itself, and `/data` for user data and application installations.

On top of the Linux kernel, there are the middleware, libraries and APIs written in C, and application software running on an application framework which includes Java-compatible libraries. Android uses Android Runtime (ART) as its runtime environment (introduced in version 4.4), which uses ahead-of-time (AOT) compilation to entirely compile the application bytecode into machine code upon the installation of an application.

![Android Stack](https://developer.android.com/guide/platform/images/android-stack_2x.png)

The hardware abstraction layer provides standard interfaces that expose device hardware capabilities to the higher-level Java API framework.

For devices running Android version 5.0 (API level 21) or higher, each app runs in its own process and with its own instance of the Android Runtime (ART). ART is written to run multiple virtual machines on low-memory devices by executing DEX files, a bytecode format designed specially for Android that's optimized for minimal memory footprint. Build toolchains, such as Jack, compile Java sources into DEX bytecode, which can run on the Android platform.

In addition to a set of standard Java development libraries (providing support for such general purpose tasks as string handling, networking and file manipulation), the Android development environment also includes the Android Libraries, a set of Java-based libraries that are specific to Android development. The entire feature-set of the Android OS is available to you through APIs written in the Java language. These APIs form the building blocks you need to create Android apps by simplifying the reuse of core, modular system components and services, which include the following:

- View System: build an app's UI

- Resource Manager: providing access to non-code resources such as localized strings, graphics, and layout files

- Notification Manager: enables all apps to display custom alerts in the status bar

- Activity Manager: manages the lifecycle of apps and provides a common navigation back stack

- Content Provider: enable apps to access data from other apps, such as the Contacts app, or to share their own data

The Android runtime core libraries are Java-based and provide the primary APIs for developers writing Android applications. It is important to note, however, that the core libraries do not perform much of the actual work and are, in fact, essentially Java “wrappers” around a set of C/C++ based libraries. In practice, the typical Android application developer will access these libraries solely through the Java based Android core library APIs.


More at [Android Architecture](https://source.android.com/devices/architecture)

. Android allows for different resources to be provided for different devices.

# Model-View-Controller

A model object holds the application's data and business logic. Model classes are typically designed to model the things the app is concerned with.

View objects know how to draw themselves on the screen and how to respond to user input.

Controller objects tie the view and model objects together. They contain application logic. In Android, a controller is typically a subclass of `Activity`, `Fragment` or `Service`.


# Application Fundamentals

Android apps can be written using Kotlin, Java, and C++ languages. Code along with any data and resources are compiled into an APK (Android package) file. 

Each Android app lives in its own security sandbox. Each app is a different user. The system assigns each app a unique Linux user ID. The system sets permissions for all the files in an app so that only the user ID assigned to that app can access them. Each process has its own virtual machine. Every app runs in its own Linux process. The Android System implements the _principle of least privilege_.

App components are the essential building block of an Android app:

- Activities: a single screen with a user interface;

- Services: runs in the background to perform long-running operations or to perform work for remove processes. They may or may not be noticed by the user. Another component can start the service and let it run or bind to it in order to interact with it.

- Broadcast receivers: enable the system to deliver events to the app outside of a regular user flow, allowing the app to respond to system-wide broadcast announcements. Well-defined entry into an app. Both the system and apps can initiate a broadcast.

- Content providers: manages a shared set of app data that can be stored in the file system, in a SQLite database, on the web, or on any other persistent storage locatioon that the app can access. To the system, a contet provider is an entry pint into an app for publicshing named data items, identified by a URI scheme.

Any app can start another app's component. When the system starts a component, it starts the process for that app if it's not already running and instantiates the classes needed for the component. 

## Activating Components

Activities, services and broadcast receivers are activated by an asynchronous messages called an _intent_. Intents bind individual components to each other at runtime, working as messengers that request an action from other components.

For activities and services, an intent defines the action to perform. For broadcast receivers, the intent simply defines the announcement being broadcast.

Content providers are activated when targeted by a request from a `ContentResolver`. The content resolver handles all direct transactions with the content provider.

### Intent 

TODO

## The manifest file

The system know the components of an app by inspecting its `AndroidManifest.xml`. All the components of the app must be declared in it. The manifest file identifies any user permission the app requires, declares the minimum API level required, declares hardware and software features required, declares API libraries other than the Android framework APIs the app needed to be linked against.

Activities, services, and content providers that you include in your source but do not declare in the manifest are not visible to the system and, consequently, can never run.

## Android Build Process

The Android tools take the resources, code, and the `AndroidManifest.xml` (which contains metadata about the application) and turn them into an `.apk` file. `aapt` (Android Asset Packaging Tool) compiles layout file resources into a more cmpact format and packges into the `.apk` file. It is possible to create a view class programmatically in the activity instead of defining them in XML.

## View System

The user interface for an Android app is built as a hierarchy of _layouts_ and _widgets_. The layouts are `ViewGroup`, containers that control how their child views are positioned on the screen. Widgets are `View` objects, UI components.


A reference to an inflated widget is obtained by calling `Activity.findViewById()`. A click event is handled by a `View.OnClickListener`.

The `Context` parameter is typically an instance of `Activity` (which is a subclass of Context).

- `FrameLayout`: simplest `ViewGroup` and does not arrange its children in any particular manner. Child views are arranged according to their `android:layout_gravity` attributes.

## Activities

The Android system initiates code in an `Activity` instantce by invoking specific callback methods that correspond to specific stages of its lifecycle.

The mobile-app experience requires that the app doesn't always begin in the same place. An activity provides the window in which the app draws its UI. Each activity can start another activity in order to perform different actions. An activity provides the window in which the app draws its UI. Although activities work together to form a cohesive user experience in an app, each activity is only loosely bound to the other activities; there are usually minimal dependencies among the activities in an app.

Activities serve as the entry point for a user's interaction with an app, and are also central to how a user navigates within an app (as with the Back button) or between apps (as with the Recents button). Android apps are built as combination of components that can be invoked individually. When one app invokes another, the calling app invokes an activity in the other app, rather than the app as an atomic whole.

### Life Cycle

 The Activity instances in an app transition through different states in their lifecycle. The Activity class provides a number of callbacks that allow the activity to know that a state has changed and take certain actions. Each callback perform specific work that's appropriate to a given change of state.

```
                                      +-----------+
                                      | Launched  |
                                      +-----|-----+
                                            |
                                      +-----v-----+
        +---------------------------->+ onCreate()|
        |                             +-----|-----+
        |                                   |
        |                             +-----v-----+                       +---------------+
        |                             | onStart() +<----------------------+  onRestart()  |
        | User navigates              +-----|-----+                       +--------|------+
        | to the activity                   |                                      ^
        |                             +-----v-----+                                |
        |                             |onResume() +<-------------+                 |
        |                             +-----|-----+              |                 |
        |                                   |                    |                 |
+-------|----------+                 +--------------+            |                 |
|  App process     |                 |  Activity    |            | User returns    |
|     killed       |                 |  running     |            | to the activity |
|                  |                 |              |            |                 |
+-------|----------+                 +------|-------+            |                 |
        ^                                   |                    |                 |
        |                                   |Another activity    |                 |
        |                                   |in foreground       |                 |
        | Apps with higher priority    +----v-----+              |                 |  User navigates
        +------------------------------+onPause() +--------------+                 |  to the activity
        | need memory                  +----|-----+                                |
        |                                   | No longer visible                    |
        |                                   |                                      |
        |                            +------v-------+                              |
        +----------------------------+  onStop()    +------------------------------+
                                     +------|-------+
                                            | finishing or destroyed by the system
                                     +------v-------+
                                     | onDestroy()  |
                                     +------|-------+
                                            |
                                            |
                                     +------v-------+
                                     |  shutdown    |
                                     +--------------+

```

```
      +---------------+
      |  Nonexistent  |
      +--|---------|--+
         |         ^
         |         |
onCreate |         |onDestroy
         |         |
      +--v---------|--+
      |   Stopped     |   Entire lifetime in memory
      +--|---------|--+             +
         |         ^                |
         |         |                |
onStart  |         |onStop          |
         v         |                |
      +--|---------|--+             |
      |   Paused      |             |  Visible lifetime
      +--|--------|---+             |  to user
         |        ^                 |          +
         |        |                 |          |
onResume |        |onPause          |          |
         |        |                 |          |  Foreground lifetime
      +--v--------|---+             |          |           +
      |   Resumed     |             |          |           |
      +---------------+             |          |           |
                                  <-+        <-+         <-+

```

Rotating the device changes the _device configuration_, a set of characteristics that describe the current state of an individual device, including screen orientation, screen density, screen size, keyboard type, dock mode, language and more. Typically, applications provide alternatives resources to match device configurations. When a runtime configuration change occurs, there may be resources that are a better match for the new configuration. Android destroys the activity, looks for the activity with those resources.

- `onCreate()` (must implement): perform startup logic that should happen only once for the entire life of the activity. Typically, it inflates widgets, gets references to inflated widgets, sets listeners on widgets to handle user interaction, connecting to external model data.

# APIs

## Logging

The `android.util.Log` class sends log messages to a shared system-level log. Verbose logs should never compile into an app except during development. Debug logs are compiled in but stripped at runtime, while error, warning, and info logs are always kept.

The message format is

```bash
date time PID-TID/package priority/tag: message
```

## Support Library

With the release of Android 9.0 (API level 28) there is a new version of the support library called `AndroidX` which is part of Jetpack. The AndroidX library contains the existing support library and also includes the latest Jetpack components. 

# Resource

An app requires resources that are separate from the source code. Using app resources makes it easy to update various characteristics of an app without modifying code. Every resource is given a unique integer ID. Different resources are provided for different device configuration.

TODO

A layout is a resource. A resource is a piece of the application that is not code, things like image files, audio files and XML files.
