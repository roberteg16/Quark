include("${CMAKE_CURRENT_LIST_DIR}/Common.cmake")

# Enable Debug build
set(CMAKE_BUILD_TYPE          "Debug"                CACHE STRING "" FORCE)

set(LLVM_OPTIMIZED_TABLEGEN   ON                     CACHE BOOL "" FORCE)
