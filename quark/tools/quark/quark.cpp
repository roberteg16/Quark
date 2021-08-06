#include <quark/Frontend/CodeGen/CodeGen.h>
#include <quark/Frontend/CodeGen/QuarkContext.h>
#include <quark/Frontend/Parsing/LexContext.h>
#include <quark/Frontend/Parsing/SourceModule.h>
#include <quark/Frontend/Sema/SemaAnalyzer.h>

#include <llvm/ADT/SetVector.h>
#include <llvm/Option/ArgList.h>
#include <llvm/Option/OptSpecifier.h>
#include <llvm/Option/OptTable.h>
#include <llvm/Option/Option.h>
#include <llvm/Support/FormatVariadic.h>
#include <llvm/Support/InitLLVM.h>

#include "../../build/tools/quark/lib/Frontend/Parsing/QuarkParser.hpp"
#include "Flags.h"

using namespace quark;

namespace {

using namespace llvm::opt;
using namespace quark;

#define PREFIX(NAME, VALUE) static const char *const NAME[] = VALUE;
#include <Options.inc>
#undef PREFIX

static const OptTable::Info InfoTable[] = {
#define OPTION(PREFIX, NAME, ID, KIND, GROUP, ALIAS, ALIASARGS, FLAGS, PARAM,  \
               HELPTEXT, METAVAR, VALUES)                                      \
  {PREFIX, NAME,  HELPTEXT,    METAVAR,     OPT_##ID,  Option::KIND##Class,    \
   PARAM,  FLAGS, OPT_##GROUP, OPT_##ALIAS, ALIASARGS, VALUES},
#include <Options.inc>
#undef OPTION
};

class QuarkOptTable : public OptTable {
public:
  QuarkOptTable() : OptTable(InfoTable) {}
};

} // namespace

using namespace quark;

/// Format helper to avoid line breaks
template <typename... Args>
static void Fprint(llvm::raw_ostream &out, Args &&...args) {
  llvm::formatv(std::forward<Args>(args)...).format(out);
}

/// Get the first alias found for the given option, or None otherwise
static llvm::Optional<llvm::opt::Option>
GetFirstAlias(const llvm::opt::OptTable &table, llvm::opt::Option o) {
  for (unsigned i = 0; i <= table.getNumOptions(); ++i) {
    llvm::opt::Option alias = table.getOption(i);
    if (!alias.isValid() || alias.getID() == o.getID()) {
      continue;
    }

    llvm::opt::Option aliasOf = alias.getAlias();
    if (aliasOf.isValid() && aliasOf.getID() == o.getID()) {
      return alias;
    }
  }
  return llvm::None;
}

/// Build the first column of the help output
static llvm::SmallString<80> BuildArg(llvm::opt::Option o,
                                      const llvm::opt::OptTable &table) {
  llvm::SmallString<80> result;
  llvm::raw_svector_ostream out(result);

  // Find a short alias to print it before
  if (llvm::Optional<llvm::opt::Option> alias = GetFirstAlias(table, o)) {
    Fprint(out, "{0} [ {1} ]", alias->getPrefixedName(), o.getPrefixedName());
  } else {
    Fprint(out, "{0}", o.getPrefixedName());
  }

  // Print the metavar if exists
  if (const char *metavar = table.getOptionMetaVar(o.getID())) {
    Fprint(out, " {0}", metavar);
  }

  return result;
}

/// Return true if the option is invalid, is a group, doesn't have prefix or is
/// an alias of other option
static bool InvalidOption(llvm::opt::Option o) {
  return !o.isValid() || o.getKind() == llvm::opt::Option::GroupClass ||
         o.getPrefix().empty() || o.getUnaliasedOption().getID() != o.getID();
}

static llvm::SetVector<unsigned>
CollectAllGroups(const llvm::opt::OptTable &table) {
  // Collect all used groups
  llvm::SetVector<unsigned> groups;
  for (unsigned i = 0; i <= table.getNumOptions(); ++i) {
    llvm::opt::Option o = table.getOption(i);
    if (InvalidOption(o)) {
      continue;
    }

    llvm::opt::Option grp = o.getGroup();
    if (grp.isValid()) {
      groups.insert(grp.getID());
    }
  }

  return groups;
}

static llvm::SmallVector<llvm::StringRef, 8> WordWrap(llvm::StringRef text,
                                                      unsigned col) {
  llvm::SmallVector<llvm::StringRef, 8> result;

  do {
    result.emplace_back(text.begin(), 0);

    llvm::StringRef line;
    std::tie(line, text) = text.split('\n');
    do {
      llvm::StringRef word;
      std::tie(word, line) = line.split(' ');

      unsigned length = word.end() - result.back().begin();
      if (length <= col) {
        result.back() = llvm::StringRef(result.back().begin(), length);
      } else {
        result.push_back(word);
      }
    } while (line.size());
  } while (text.size());

  return result;
}

static void PrintGroupHelp(llvm::opt::InputArgList &args, QuarkOptTable &table,
                           llvm::raw_ostream &out,
                           llvm::opt::OptSpecifier groupID) {
  out << table.getOptionHelpText(groupID) << ":\n";

  for (unsigned i = 0; i <= table.getNumOptions(); ++i) {
    llvm::opt::Option o = table.getOption(i);
    if (InvalidOption(o)) {
      continue;
    }
    // Skip options of other groups
    if (!o.matches(groupID)) {
      continue;
    }

    llvm::StringRef helpText;
    if (const char *text = table.getOptionHelpText(o.getID())) {
      helpText = text;
    }

    llvm::SmallString<80> arg = BuildArg(o, table);
    for (llvm::StringRef line : WordWrap(helpText, 47)) {
      // Break the line if both columns don't fit together
      if (arg.size() > 30) {
        Fprint(out, "  {0}\n", arg);
        arg.clear();
      }
      Fprint(out, "  {0,-30} {1}\n", arg, line);
      arg.clear();
    }
  }
}

static void PrintHelp(llvm::opt::InputArgList &args, QuarkOptTable &table,
                      llvm::raw_ostream &out, llvm::StringRef exec) {
  // Preamble
  out << llvm::formatv("Syntax: {0} [options] <input>\n", exec);

  // Print arguments grouped by group
  for (unsigned group : CollectAllGroups(table)) {
    out << '\n';
    PrintGroupHelp(args, table, out, group);
  }
}

static bool HandleInmediateArgs(llvm::opt::InputArgList &args,
                                QuarkOptTable &table, llvm::raw_ostream &out,
                                llvm::StringRef exec,
                                llvm::opt::OptSpecifier helpF,
                                llvm::opt::OptSpecifier versionF) {
  if (args.hasArg(helpF)) {
    PrintHelp(args, table, out, exec);
    return true;
  }

  if (args.hasArg(versionF)) {
    return true;
  }

  return false;
}

extern FILE *yyin; // NOLINT

int main(int argc, const char *argv[]) {
  llvm::InitLLVM llvm(argc, argv);

  auto options = std::make_unique<QuarkOptTable>();

  auto compilerArgs = llvm::makeArrayRef(argv + 1, argv + argc);
  unsigned missingArgIndex, missingArgCount;
  llvm::opt::InputArgList args =
      options->ParseArgs(compilerArgs, missingArgIndex, missingArgCount);

  llvm::raw_ostream &out = llvm::outs();

  if (HandleInmediateArgs(args, *options, out, argv[0], OPT_help,
                          OPT_version)) {
    return 0;
  }

  llvm::opt::Arg *input = args.getLastArg(OPT_INPUT);
  if (!input) {
    out << "No inputs found\n";
    return -1;
  }

  std::string fileName(input->getSpelling());

  quark::LexContext ctx(fileName);
  quark::SourceModule sm;

  quark::QuarkParser quarkParser(ctx, sm);

  llvm::StringRef file = input->getSpelling();
  yyin = fopen(file.data(), "r");
  if (quarkParser.parse()) {
    return 1;
  }

  SemaAnalyzer sema;
  if (!sema.analyze(sm)) {
    out << "Semantic analysis failed\n";
    return -1;
  }

  if (args.hasArg(OPT_emit_ast)) {
    sm.print(out);
    return 0;
  }

  QuarkContext &quarkCtx = QuarkContext::buildContext();
  std::unique_ptr<llvm::Module> mod = CodeGen(file, sm, quarkCtx).generate();

  if (args.hasArg(OPT_emit_ir)) {
    mod->print(out, nullptr);
    return 0;
  }

  std::error_code ec;
  llvm::raw_fd_ostream rfo("ir.ll", ec);
  mod->print(rfo, nullptr);

  return 0;
}
