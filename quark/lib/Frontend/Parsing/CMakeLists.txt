# Find Bison
find_package(BISON 3.5.1 REQUIRED)

# Find Flex
find_package(FLEX 2.5.35 REQUIRED)

BISON_TARGET(QuarkParser
             QuarkGrammar.y
             ${CMAKE_CURRENT_BINARY_DIR}/QuarkParser.cpp)

FLEX_TARGET(QuarkLexer
             QuarkLex.l
             ${CMAKE_CURRENT_BINARY_DIR}/QuarkLexer.cpp)

ADD_FLEX_BISON_DEPENDENCY(QuarkLexer QuarkParser)

include_directories(${CMAKE_CURRENT_BINARY_DIR})

llvm_add_library(QuarkParsing
  LexContext.cpp
  SourceModule.cpp
  ParserUtils.cpp
  ${BISON_QuarkParser_OUTPUTS}
  ${FLEX_QuarkLexer_OUTPUTS}

  LINK_LIBS
  QuarkAST

  LINK_COMPONENTS
  Support
)

target_compile_options (QuarkParsing PRIVATE -fexceptions)
