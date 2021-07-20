set(QUARK_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(QUARK_BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR})

include_directories(BEFORE
  "${QUARK_SOURCE_DIR}/include"
  "${QUARK_BINARY_DIR}/include"
)

add_subdirectory(lib)
add_subdirectory(tools)
add_subdirectory(test)
