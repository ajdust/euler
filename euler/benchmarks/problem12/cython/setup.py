# run with pytohn3 setup.py build_ext --inplace

from distutils.core import setup
from distutils.extension import Extension
from Cython.Build import cythonize

extensions = [
    Extension(
        "facben",
        ["facben.pyx"],
        language="c++",
        extra_compile_args=["-std=c++14"],
        extra_link_args=["-std=c++14"]
    )
]

setup(
    name = "facben",
    ext_modules = cythonize(extensions, annotate=True),
)