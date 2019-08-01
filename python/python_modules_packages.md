# The import system

The `import` statement combines two operations; it searches for the named module, then it binds the results of that search to a name in the local scope. The built-in `__import__()` function searches the path and return the module to perform the name binding. Other mechanisms for invoking the import system (such as `importlib.import_module()`) may choose to bypass `__import__()` and use their own solutions to implement import semantics.

The `importlib` module provides a rich API for interacting with the import system.

The first place checked during import search is `sys.modules`. This mapping serves as a cache of all modules that have been previously imported, including the intermediate paths.  The search path `sys.path` is initialized from the current path and `PYTHONPATH` and some default locations. If the named module is not in `sys.modules`, then Python's import protocol is invoked to find and load the module. Python searches `sys.meta_path`, which contains a list of meta path finder objects. Python has one importer locating built-in modules, another for frozen modules, a third search an import path (a list of locations that may name file system or zip files) for modules. 


```python
In [10]: sys.meta_path
Out[10]: 
[_frozen_importlib.BuiltinImporter,
 _frozen_importlib.FrozenImporter,
 _frozen_importlib_external.PathFinder,
 <six._SixMetaPathImporter at 0x7f96fce4aa90>]
```

# Modules

Python has a way to put definitons in a file and use them in a script or in an interactive instance of the interpreter. Such a file is called a module.

Within a module, the module's name is available as the value of the global variable `__name__`.

A module can contain executable statements as well as function definitions. These statements are intended to initialize the module. They are executed only the first time the module name is encountered in an `import` statement.

Each module has its own private symbol table, which is used as the global symbol table by all functions defined in the module.

A module executed as a script can be used to provide a convenient user interface or for testing purpose.



`dir()` finds out which names a module defines. To reload a module, use `importlib.reload()`

# Packages

Packages are a way of structuring Python’s module namespace by using “dotted module names”. All packages are modules, but not all modules are packages. Any module that contains a `__path__` attribute is considered a package.

Python defines two types of packages, __regular packages__ and __namespace packages__. A regular package is typically implemented as a directory containing an `__init__.py` file. When a regular package is imported, this `__init__.py` file is implicitly executed, and the objects it defines are bound to names in the package’s namespace.

A namespace package is a composite of various portions, where each portion contributes a subpackage to the parent package. Portions may reside in different locations on the file system. Portions may also be found in zip files, on the network, or anywhere else that Python searches during import.

`__all__` in a package's `__init__.py` specifies a list of of submodules that should also be imported when `from package import *` is encountered. It is also possible to only import certain submodules. Note that relative imports are based on the name of the current module. Since the name of the main module is always `__main__`, modules intended for use as the main module of a Python application must always use absolute imports.

Python comes with a packaging framwork called `Disutils`, a build tool, an installation tool, a package metadata format and other things. It integrates with Python Package Index (PyPI). All of these center around the `setup` script, traditionally called `setup.py`.
