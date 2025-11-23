# Modules

A Python script file consisting of definitions, statements for initialization to achieve modularity.

A module can contain 

- executable statements

- function definitions

- type definitions

These statements are intended to initialize the module. They are executed only the first time the module name is encountered in an `import` statement.

A module executed as a script can be used to provide a convenient user interface or for testing purpose. When a module is executed as a script, its name becomes `__main__`. That's why we have 
  
```python
if __name__ == '__main__':
    main()
```  

The module's name is available as the value of the global variable `__name__`.

Each module forms its own namespace: it has its own private symbol table, which is used as the global symbol table by all functions defined in the module.

`dir()` finds out which names a module defines.

# The import mechanism

The `import` statement combines two operations; it searches for the named module, then it binds the results of that search to a name in the local scope. The built-in `__import__()` function searches the path and return the module to perform the name binding. Other mechanisms for invoking the import system (such as `importlib.import_module()`) may choose to bypass `__import__()` and use their own solutions to implement import semantics.

The `importlib` module provides a rich API for interacting with the import system.

The import machinery fills in some attributes on each module during loading, based on the module's spec, before the loader executes the module.

- `__name__`: fully-qualified name of the module

- `__package__`: empty for top-level module, or the parent package's name for submodules.

Each module is imported only once per session. To reimport a module, use `importlib.reload()`

## Search Paths

- `sys.builtin_module_names`

- `sys.path`: initialized from the script's directory ,`PYTHONPATH` and the installation-dependent default (`site-packages`)
  - the symlinks are resolved before adding
  - `site-packages` can be used for packages that are likely to depend on the Python version.

## `import` syntax

- multiline import with parentheses

    ```python
    from Module import (A, B, C, D,
        E, F)
    ```
  
- Relative imports: as `import A`  now defaults to absolute `sys.path`, relative import is a must, one dot per level; 
  - only `from <> import ...` is allowed, `import ..A` is illegal
  because `..A` is not a legal prefix in an expression.
  - `__name__` is used in relative imports to find a module (not purely filesystem hierarchy). If the module name provides no package information or set to `__main__`, then it is assumed to be a top-level module.
  - Note that relative imports are based on the name of the current module. Since the name of the main module is always `__main__`, modules intended for use as the main module of a Python application must always use absolute imports.
  ```python
  from .CurrentDirectoryModule import A
  from ..ParentDirectoryModule import B
  from ...GrantparentDirectoryModule import C
  from . import D # current directory package
  ```

- Only absolute-importing a name in package that is a module or subpackage is supported. For a function or a class, it is not.
  Each name except the last one must be package and the last item can only be a module or a package.

  ```python
  import Package.Module # ok 
  import Package.Module1.Module2 # ok
  from Package import Module1 # ok
  from Package.Module import Class # ok
  
  import Package.Module.Class # error
  ```

# Packages

A module of related modules organized together. All packages are modules, but not all modules are packages. Any module that contains a `__path__` attribute is considered a package. Importing a name under a package imports any module/package above the name in the hierarchy.

## Basic Concepts

- _distribution_: separately installable sets of Python modules

- _vendor package_: pacakged by OS packaging meechanism

- _portion_: a set of files in a single directory (possibly a zip file) that contribute to a namespace package

Python defines two types of packages, __regular packages__ and __namespace packages__ (since 3.3)

- regular package is typically implemented as a directory containing an `__init__.py` file. When a regular package is imported, this `__init__.py` file is implicitly executed, and the objects it defines are bound to names in the package's namespace.
  - `__init__.py` may be empty or contains initialization code.
  - Regular packages are self-contained: all code reside in the same directory hierarchy.
  - regular packages are constructed statically (a fixed path) and thus can be loaded faster than a dynamic namespace package.

- namespace package: a package whose main goal is to provide a namespace for multiple directories belonging to the same package without an `__init__.py`.

## Namespace Package (PEP 420)

A mechanism for splitting a single package across multiple directories (under `sys.path`) so that a package is not restricted to one directory hierarchy but still in the same namespace, or under the same directory without causing file conflicts (`__init__.py`).

Also, this enables dynamic path computation and portion search that is not restricted to the `__path__` constructed the first time the package is imported. Additional portions of the package added later to `sys.path` can be found if they are imported. `__path__` may change with the import of new portions due to its dynamic nature.

The concept has been implemented before with various slightly-incompatible mechanisms. The import mechinery itself will construct the list of directories that make up that package without manually manipulating `__path__`. The import machinery looks for any directory matching the package name without an `__init__.py` and adds it to `__path__`, and when the search is finished, a namespace package of the name is _dynamically_ created. The package has no `__file__` attribute since it it not a directory or a `__init__.py` but dynamically created.

This dynamic nature also allows extension to an existing package without modifying the original package as long as the package is implemented as a namespace package.

## Search Order

Directories (and thus packages) precedes any modules defined in `importlib.machinery.all_suffixes()`:
a package precedes a module with the same name during importing.

## `__path__`: the One Distinction between Packages and Modules

If a module has a `__path__`, then it is a package, by default containing the directory name of the package.

The `__path__` list specifies the directories that are searched for submodules of the package, parallel to the global `sys.path`. 
This may be useful if submodules under certain directories are preferred by prepending these directories to `__path__`.

## `__all__` & import all

`from package import *` does not search the entire directory and its subdirectories. It merely searches `__all__` in a package's `__init__.py`, a list of names of the submodules that should also be imported when `from package import *` is encountered. It is also possible to only import certain submodules. If `__all__` is not defined, then no submodules except what is defined in the package level is imported (e.g. those defined in `__init__.py`)

# Invoking Interpreter 

- `python scriptname`: `scriptname` may be directory/zipfile containing a `__main__.py` module file, a `.py` file and that file will be the `__main__` module.
   - `sys.argv[0]` is the script name given
   
- `-m modulename`: search the module in `sys.path` and execute it as a script
  - if a package name is given, its `__main__` module is executed
  - not for builtin modules written in C as there are no module files.
  - `sys.argv[0]` is the full path to the module file.
  - Many standard modules contain code that can be executed as a script.
  - the zipfile case includes `.whl`, one can run `python pip.whl/pip` to use `pip`
  
- `-`: read from stdin

- `-c commands`: one or more statements.
  - `sys.argv[0]` is `-c`

# Package Distribution & Installation

## Pip (Preferred installer)

```shell
python -m pip
```

## distutils (Builtin, Deprecated in Python 3.10 and Removed in 3.12) 

`setup.py` is the center of all activity of building, distributing and installing modules using Distutils.

```shell
# first build
python setup.py build [--build-base=my-build-dir] 

# or install it directly into an instalation directory
python setup.py install  
# one may specify an alternative installation directory using --user --home- --prefix --exec-prefix --install-base
```

## [Setuptools](https://setuptools.pypa.io/en/latest/index.html) (the Modern Successor to distutils)

Not in the standard library but typically included in a Python distribution.
Typically, this shoud be used with a frontend (build)[https://pypi.org/project/build/] 
and `build` will download and install `setuptools`.

with `build` as the frontend, Setuptools uses `pyproject.toml` instead of `setup.py`.
`pyproject.toml` requires the specification of a certain backend (here `setuptools`).
`setup.py` is still supported but not recommended.
