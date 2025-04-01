# Building

Since Turner is header-only library, there isn't anything to build per-se,
unless you want to build the tests and examples. That said, here's how one
would build the repository using CMake.

To build tests, you'll need [Catch2](https://github.com/catchorg/Catch2) installed.

```bash
# Clone the repository
git clone https://github.com/punk-floyd/turner
cd turner

# Configure the project
cmake -DCMAKE_BUILD_TYPE=Release ..

# Build the repository
cmake --build .

# Run the tests (if Catch2 was found)
ctest
```

## Integrating Turner into an Existing Project

### Option 1: Pilfer Turner Header Files

This is the easiest method.

MDTODO : Document me

### Option 2: Just Point CMake to Turner's `inc/` directory

One would use this method if they didn't want any kind of serious
relationship with Turner. If commitments scare you and you just want to take
it slow and see where things go, then this is your option.

Assuming you have some target library or executable `foo` that you want to
be able to access Turner, add the following to your `CMakeLists.txt` file.

```cmake
target_include_directories(foo PRIVATE "path/to/turner/inc/directory")
```

Then, in your C++ source/header file(s):

```c++
#include <turner/json.h>
```

### Option 3: Add Turner as a Submodule

MDTODO : Document me