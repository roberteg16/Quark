# Only interested in X86
set(LLVM_TARGETS_TO_BUILD      "X86"            CACHE STRING "" FORCE)

# Enable Quark project
set(LLVM_TOOL_QUARK_BUILD      ON               CACHE BOOL   "" FORCE)
# Append quark as an external project of LLVM
set(LLVM_EXTERNAL_PROJECTS    "quark"           CACHE STRING "" FORCE)
# Minimal required LLVM projects to build quark
set(LLVM_ENABLE_PROJECTS      "quark"           CACHE STRING "" FORCE)