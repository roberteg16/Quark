// Include the common option parsing interfaces.
include "llvm/Option/OptParser.td"

def General_Group : OptionGroup<"<general group>">, HelpText<"Options">;

def version : Flag<["--"], "version">, Group<General_Group>,
  HelpText<"print version">;
def v : Flag<["-"], "v">, Group<General_Group>, Alias<version>;
def help : Flag<["--"], "help">, Group<General_Group>, HelpText<"print help">;
def h : Flag<["-"], "h">, Group<General_Group>, Alias<help>;

def emit_ast: Flag<["--"], "emit-ast">, Group<General_Group>,
              HelpText<"emits Quark's AST">;
def emit_ir: Flag<["--"], "emit-ir">, Group<General_Group>,
             HelpText<"emits LLVM IR">;
