#ifndef QUARK_TOOL_QUARK_FLAGS_H
#define QUARK_TOOL_QUARK_FLAGS_H

namespace quark {

enum ID {
  OPT_INVALID = 0, // This is not an option ID.
#define OPTION(PREFIX, NAME, ID, KIND, GROUP, ALIAS, ALIASARGS, FLAGS, PARAM,  \
               HELPTEXT, METAVAR, VALUES)                                      \
  OPT_##ID,
#include <Options.inc>
  LastOption
#undef OPTION
};

} // namespace quark

#endif // QUARK_TOOL_QUARK_FLAGS_H