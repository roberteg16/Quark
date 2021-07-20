# Test runner infrastructure for Parallelware Analyzer. This configures the
# Parallelware test trees for use by Lit, and delegates to LLVM's lit test
# handlers.

configure_lit_site_cfg(
  ${CMAKE_CURRENT_SOURCE_DIR}/lit.site.cfg.py.in
  ${CMAKE_CURRENT_BINARY_DIR}/lit.site.cfg.py
  MAIN_CONFIG
  ${CMAKE_CURRENT_SOURCE_DIR}/lit.cfg.py
  )
configure_lit_site_cfg(
  ${CMAKE_CURRENT_SOURCE_DIR}/Unit/lit.site.cfg.py.in
  ${CMAKE_CURRENT_BINARY_DIR}/Unit/lit.site.cfg.py
  MAIN_CONFIG
  ${CMAKE_CURRENT_SOURCE_DIR}/Unit/lit.cfg.py
  )

option(QUARK_TEST_USE_VG "Run Quark tests under Valgrind" OFF)
if(QUARK_TEST_USE_VG)
  set(QUARK_TEST_EXTRA_ARGS ${QUARK_TEST_EXTRA_ARGS} "--vg")
endif ()

# X-Unit results
set(QUARK_TEST_EXTRA_ARGS ${QUARK_TEST_EXTRA_ARGS} "--xunit-xml-output=check-quark.xml")
# Filter
set(QUARK_TEST_EXTRA_ARGS ${QUARK_TEST_EXTRA_ARGS} "--filter=\$\${QUARK_TEST_FILTER}")

list(APPEND QUARK_TEST_DEPS
  llvm-config
  FileCheck count not
  llvm-symbolizer

  quark
)

set(QUARK_TEST_PARAMS
  quark_site_config=${CMAKE_CURRENT_BINARY_DIR}/lit.site.cfg
)

add_custom_target(quark-test-depends DEPENDS ${QUARK_TEST_DEPS})
set_target_properties(quark-test-depends PROPERTIES FOLDER "Quark tests")

add_lit_testsuite(check-quark "Running the Quark regression tests"
  ${CMAKE_CURRENT_BINARY_DIR}
  # LIT ${LLVM_LIT}
  PARAMS ${QUARK_TEST_PARAMS}
  DEPENDS ${QUARK_TEST_DEPS}
  ARGS ${QUARK_TEST_EXTRA_ARGS}
)
set_target_properties(check-quark PROPERTIES FOLDER "Quark Tests")