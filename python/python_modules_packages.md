# Modules

Python has a way to put definitons in a file and use them in a script or in an interactive instance of the interpreter. Such a file is called a module.

Within a module, the module's name is available as the value of the global variable `__name__`.

A module can contain executable statements as well as function definitions. These statements are intended to initialize the module. They are executed only the first time the module name is encountered in an `import` statement.

Each module has its own private symbol table, which is used as the global symbol table by all functions defined in the module.

A module executed as a script can be used to provide a convenient user interface or for testing purpose.

The search path `sys.path` is initialized from the current path and `PYTHONPATH` and some default locations.

`dir()` finds out which names a module defines.

# Packages

Python comes with a packaging framwork called `Disutils`, a build tool, an installation tool, a package metadata format and other things. It integrates with Python Package Index (PyPI). All of these center around the `setup` script, traditionally called `setup.py`.
