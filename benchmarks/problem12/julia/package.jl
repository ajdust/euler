# Skip JIT compilation
# https://github.com/JuliaLang/PackageCompiler.jl
using PackageCompiler

build_executable(
	"main.jl",
	snoopfile = "call_functions.jl"
)