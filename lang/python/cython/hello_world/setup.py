from setuptools import setup
from Cython.Build import cythonize

setup(name = 'Hello From Cython',
      ext_modules= cythonize("*.pyx", language_level=3, annotate=True))