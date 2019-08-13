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

Android apps are built as combination of components that can be invoked individually. Android allows for different resources to be provided for different devices.
