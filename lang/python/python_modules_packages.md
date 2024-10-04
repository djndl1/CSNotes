# Modules

A Python file consisting of definitions, statements for initialization for modularity.
A module can contain executable statements, function definitions, type definitions. These statements are intended to initialize the module. They are executed only the first time the module name is encountered in an `import` statement.

A module executed as a script can be used to provide a convenient user interface or for testing purpose.

The module's name is available as the value of the global variable `__name__`.
Each module forms its own namespace: it has its own private symbol table, which is used as the global symbol table by all functions defined in the module.

`dir()` finds out which names a module defines. To reload a module, use `importlib.reload()`

# Packages

Packages are a way of structuring Python’s module namespace by using “dotted module names”. All packages are modules, but not all modules are packages. Any module that contains a `__path__` attribute is considered a package.

Python defines two types of packages, __regular packages__ and __namespace packages__. A regular package is typically implemented as a directory containing an `__init__.py` file. When a regular package is imported, this `__init__.py` file is implicitly executed, and the objects it defines are bound to names in the package’s namespace.

A namespace package is a composite of various portions, where each portion contributes a subpackage to the parent package. Portions may reside in different locations on the file system. Portions may also be found in zip files, on the network, or anywhere else that Python searches during import.

`__all__` in a package's `__init__.py` specifies a list of of submodules that should also be imported when `from package import *` is encountered. It is also possible to only import certain submodules. Note that relative imports are based on the name of the current module. Since the name of the main module is always `__main__`, modules intended for use as the main module of a Python application must always use absolute imports.

Python comes with a packaging framwork called `Disutils`, a build tool, an installation tool, a package metadata format and other things. It integrates with Python Package Index (PyPI). All of these center around the `setup` script, traditionally called `setup.py`.

# The import system mechanism

The `import` statement combines two operations; it searches for the named module, then it binds the results of that search to a name in the local scope. The built-in `__import__()` function searches the path and return the module to perform the name binding. Other mechanisms for invoking the import system (such as `importlib.import_module()`) may choose to bypass `__import__()` and use their own solutions to implement import semantics.

The `importlib` module provides a rich API for interacting with the import system.

The import machinery fills in some attributes on each module during loading, based on the module's spec, before the loader executes the module.

- `__name__`: fully-qualified name of the module

- `__package__`: empty for top-level module, or the parent package's name for submodules.

Each module is imported only once per session. To reimport a module, use `importlib.reload()`

## Search Paths

- `sys.builtin_module_names`

- `sys.path`: initialized from the script's directory, `PYTHONPATH` and the installation-dependent default (`site-packages`)

# Startup

- `python scriptname`: =scriptname= may be directory/zipfile containing a `__main__.py` module file, a `.py` file and that file will be the `__main__` module.
   - `sys.argv[0]` is the script name given
   
- `-m modulename`: search the module in `sys.path` and execute it as a script
  - if a package name is given, its =__main__= module is executed
  - not for builtin modules written in C as there are no module files.
  - `sys.argv[0]` is the full path to the module file.
  - Many standard modules contain code that can be executed as a script.
  - the zipfile case includes `.whl`, one can run `python pip.whl/pip` to use `pip`
  
- `-`: read from stdin

- `-c commands`: one or more statements.
  - `sys.argv[0]` is `-c`
