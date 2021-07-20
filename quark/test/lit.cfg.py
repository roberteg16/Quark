# -*- Python -*-

# Configuration file for the 'lit' test runner.

import os

import lit.formats
from lit.llvm import llvm_config

# name: The name of this test suite.
config.name = 'Quark'

# testFormat: The test format to use to interpret tests.
config.test_format = lit.formats.ShTest(not llvm_config.use_lit_shell)

# suffixes: A list of file extensions to treat as test files.
config.suffixes = ['.ll', '.qk', '.test', '.txt']

# excludes: A list of directories to exclude from the testsuite. The 'Inputs'
# subdirectories contain auxiliary inputs for various tests in their parent
# directories.
config.excludes = ['Inputs', 'CMakeLists.txt', 'README.txt', 'LICENSE.txt']

# test_source_root: The root path where tests are located.
config.test_source_root = os.path.dirname(__file__)

# test_exec_root: The root path where tests should be run.
config.test_exec_root = os.path.join(config.quark_obj_root, 'test')

# Tweak the PATH to include the tools dir.
llvm_config.with_environment('PATH', config.llvm_tools_dir, append_path=True)

# Propagate some variables from the host environment.
llvm_config.with_system_environment(
    ['HOME', 'INCLUDE', 'LIB', 'TMP', 'TEMP', 'ASAN_SYMBOLIZER_PATH', 'MSAN_SYMBOLIZER_PATH'])

# Don't reuse the user config for testing purposes
llvm_config.with_environment('XDG_DATA_HOME', config.quark_obj_root)
llvm_config.with_environment('XDG_CONFIG_HOME', config.quark_obj_root)

llvm_config.use_default_substitutions()