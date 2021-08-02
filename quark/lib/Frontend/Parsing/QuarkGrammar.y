%require "3.5.1"
%language "C++"

%skeleton "lalr1.cc"

/// Ask bison to add location tracking for us
/// %locations
/// Ask bison to generate position/location code into separated file
/// %define api.location.file "SourceLoc.h"

/// Add prefix to all symbols
/// %define api.symbol.prefix {S_}
/// Add prefix to all tokens
%define api.token.prefix {TK_}

/// Automove all semantic values of actions
/// %define api.value.automove

/// Define objects inside namespace quark
%define api.namespace {quark}

/// Tell bison to change parser name to QuarkParser
%define api.parser.class {QuarkParser}

/// Ask bison to use variants insted of union (safer and easier to use)
%define api.value.type variant
/// When used along with '%define api.value.type variant' Bison modifies
/// what it expects for yylex and generates several handy constructors for each
/// token
%define api.token.constructor

/// If used along with '%define api.value.type variant', RTTI is needed, so
/// do not disable it
%define parse.assert
%define parse.error verbose

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                          REQUIRD CODE STARTS HERE                          //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

%code requires
{
#include <quark/Frontend/AST/Decl.h>
#include <quark/Frontend/AST/Expr.h>
#include <quark/Frontend/AST/Stmt.h>
#include <quark/Frontend/AST/Type.h>
#include <quark/Frontend/Parsing/LexContext.h>
#include <quark/Frontend/Parsing/SourceModule.h>
#include <quark/Frontend/Parsing/ParserUtils.h>

#include <llvm/ADT/SmallString.h>
#include <llvm/Support/Debug.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/Twine.h>
#include <llvm/Support/Casting.h>

} // %code requires

%param { LexContext &ctx }
%param { SourceModule &sm }

%code{
  quark::QuarkParser::symbol_type yylex (quark::LexContext&,
                                         quark::SourceModule &sm);

  void quark::QuarkParser::error(const std::string &error) {
  llvm::outs() << "Error: " << error << "\n";
}
}

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                        TOKEN DEFINITION STARTS HERE                        //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

%token  END 0
%token  MODULE EXPORT IMPORT
%token  FOR WHILE PAR_FOR
%token  IF ELSIF ELSE
%token  RET DEFER TYPE FN PRINT
%token  ALLOC DEALLOC
%token  VAR
%token  MUT

%token PLUS "+" MINUS "-" SLASH "/" ASTERISK "*" AMPERSAND "&" MOD "%" EQUAL "="
%token PIPEPIPE "||" LESS "<" GREAT ">" AMPERSANDAMPERSAND "&&" EXCLAMATION "!"
%token EQUALEQUAL "==" EXCLAMATIONEQUAL "!=" GREATEQUAL ">=" LESSEQUAL "<="

%token INV_ARROW "<-" ARROW "->" DOUBLEDOTEQUAL ":=" DOUBLEDOT ":" DOT "."
%token SEMICOLON ";" COMMA ","
%token TRUE "true" FALSE "false"
%token LEFTKEY "{" RIGHTKEY "}"
%token LEFTBRACE "[" RIGHTBRACE "]"
%token LEFTPARENTHESIS "(" RIGHTPARENTHESIS ")"

%token INTEGER REAL STRING CHAR ID

// Associativity and precendece (lower to greater)
%right "="
%left "||"
%left "&&"
%left "==" "!="
%left ">" "<" ">=" "<="
%left "+" "-"
%left "*" "/" "%"
%right "&" "!"
%left "(" ")" "[" "]" "." "->"
%left "<-"

// Dangling ELSE
%right ELSIF ELSE

// Token types
%type<long long> INTEGER;
%type<long double> REAL;
%type<bool> TRUE FALSE;
%type<llvm::SmallString<40>> STRING;
%type<llvm::SmallString<10>> ID Export_module Module_name Doted_name;
%type<char> CHAR;
%type<std::unique_ptr<Expr>> ConstNumber Literal Term Expr Func_call Method_call;
%type<std::unique_ptr<VarRefExpr>> Var_access;
%type<std::unique_ptr<Expr>> Member_access;
%type<llvm::SmallVector<std::unique_ptr<Expr>, 4>> List_of_expr List_of_expr1;
%type<llvm::SmallVector<llvm::SmallString<10>, 10>> List_of_imports;
%type<std::deque<std::unique_ptr<Decl>>> List_of_decls;
%type<std::unique_ptr<Decl>> Declaration;
%type<std::unique_ptr<FuncDecl>> Declaration_func;
%type<std::unique_ptr<TypeDecl>> Declaration_type;
%type<std::unique_ptr<VarDecl>> Reciver Var_decl;
%type<llvm::SmallVector<std::unique_ptr<TypeFieldDecl>, 4>> List_of_field_decl;
%type<std::unique_ptr<TypeFieldDecl>> Field_decl;
%type<llvm::SmallVector<std::unique_ptr<VarDecl>, 4>> List_of_params List_of_params1;
%type<std::unique_ptr<Stmt>> Stmt Else_stmt;
%type<std::unique_ptr<VarDeclStmt>> Local_var_decl;
%type<std::unique_ptr<BlockStmt>> Block;
%type<std::vector<std::unique_ptr<Stmt>>> List_of_stmt;
%type<llvm::SmallVector<IfStmt::CondAndStmt, 4>> List_of_elsif;
%type<IfStmt::CondAndStmt> Elsif_stmt;
%type<llvm::SmallVector<TypeAccess, 4>> List_of_accesses;
%type<TypeAccess> Subtype_access;
%type<std::vector<unsigned>> List_of_static_arrays;
%type<std::vector<std::unique_ptr<Expr>>> List_of_dinamic_arrays;
%type<unsigned> List_of_pointers;
%type<std::unique_ptr<Type>> Type_declaration;
%type<std::unique_ptr<Expr>> Var_access_array_subscript;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                           GRAMMAR STARTS HERE                              //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
%%
%start Start;

Start: Export_module List_of_imports List_of_decls END {
                                                          sm.fill(ctx, std::move($1), std::move($2), std::move($3));
                                                       };

List_of_imports:  List_of_imports IMPORT Module_name {
                                                        $1.push_back($3);
                                                        $$ = std::move($1);
                                                     }
               |  %empty                             {};

Export_module:  EXPORT MODULE Module_name { $$ = std::move($3); }
             |  %empty                    { } ;

Module_name: ID Doted_name {
                              $$ += $1;
                              $$ += $2;
                            };

Doted_name: Doted_name "." ID {
                                $$ += $1;
                                $$ += '.';
                                $$ += $3;
                              }
          | %empty            { };

List_of_decls: List_of_decls Declaration {
                                            $1.push_back(std::move($2));
                                            $$ = std::move($1);
                                          }
             | %empty                    {};

Declaration: Declaration_type {
                                ctx.addTypeDecl(*$1);
                                $$ = std::move($1);
                              }
           | Declaration_func {
                                ctx.addFunctionDecl(*$1);
                                $$ = std::move($1);
                              };

Var_decl: ID Type_declaration   {
                                  ctx.checkNonExistenceOfVarCurrentLevel($1);
                                  $$ = std::make_unique<VarDecl>($1, std::move($2));
                                };

Declaration_type: TYPE ID "{" List_of_field_decl "}"  {
                                                        ctx.checkNonExistenceOfType($2);
                                                        $$ = std::make_unique<TypeDecl>($2, std::move($4));
                                                      };

Field_decl: ID Type_declaration   { $$ = std::make_unique<TypeFieldDecl>($1, std::move($2)); };

List_of_field_decl: List_of_field_decl Field_decl {
                                                    $1.push_back(std::move($2));
                                                    $$ = std::move($1);
                                                  }
                  | %empty                        {};

Declaration_func: FN { ctx.enterScope(); } Reciver ID ":" List_of_params "->" Type_declaration {
                                                                                    ctx.enterFunction($4);
                                                                                    auto &listOfParams = $6;
                                                                                    for (auto &param : listOfParams) {
                                                                                      ctx.addVar(*param);
                                                                                    }
                                                                                    auto &reciver = $3;
                                                                                    if (reciver) {
                                                                                      ctx.addVar(*reciver);
                                                                                    }
                                                                                  } "{" List_of_stmt "}" {
                                                                                                          $$ = ctx.exitFunction(std::move($6), std::move($8),
                                                                                                                                std::move($11), std::move($3));
                                                                                                          ctx.exitScope();
                                                                                                         };

Reciver: "(" ID "*" ID ")"  {
                          auto varDecl = std::make_unique<VarDecl>(std::move($2), std::make_unique<PtrType>(ctx.getType($4)->clone()));
                          varDecl->setKind(VarDeclKind::RecieverVar);
                          $$ = std::move(varDecl);
                        }
       |  %empty        {};

List_of_params: List_of_params1 { $$ = std::move($1); }
              | %empty          { };
List_of_params1:  List_of_params1 "," Var_decl {
                                                $1.push_back(std::move($3));
                                                $1.back()->setKind(VarDeclKind::ParamVar);
                                                $$ = std::move($1);
                                               }
               | Var_decl                      {
                                                $$.push_back(std::move($1));
                                                $$.back()->setKind(VarDeclKind::ParamVar);
                                               };

Block: "{" { ctx.enterScope(); } List_of_stmt { ctx.exitScope(); } "}" { $$ = std::make_unique<BlockStmt>(std::move($3)); };

List_of_stmt: List_of_stmt Stmt {
                                  $1.push_back(std::move($2));
                                  $$ = std::move($1);
                                }
            | %empty            { };

Stmt: FOR "(" { ctx.enterScope(); } Local_var_decl ";" Expr ";" Expr ")" Stmt { ctx.exitScope(); } {
        $$ = std::make_unique<ForStmt>(std::move($4), ctx.castToBoolIfNeeded(std::move($6)),
                                       std::move($8), std::move($10), /*isParallel*/ false);
      }
    | PAR_FOR "(" { ctx.enterScope(); } Local_var_decl ";" Expr ";" Expr ")" Stmt { ctx.exitScope(); } {
        $$ = std::make_unique<ForStmt>(std::move($4), ctx.castToBoolIfNeeded(std::move($6)),
                                       std::move($8), std::move($10), /*isParallel*/ true);
      }
    | IF "(" Expr ")" Stmt List_of_elsif {
        $$ = std::make_unique<IfStmt>(ctx.castToBoolIfNeeded(std::move($3)), std::move($5),
                                      std::move($6));
      }
    | IF "(" Expr ")" Stmt List_of_elsif Else_stmt {
        $$ = std::make_unique<IfStmt>(ctx.castToBoolIfNeeded(std::move($3)), std::move($5),
                                      std::move($6), std::move($7));
      }
    | WHILE "(" Expr ")" { ctx.enterScope(); } Stmt { ctx.exitScope(); } {
        $$ = std::make_unique<WhileStmt>(ctx.castToBoolIfNeeded(std::move($3)), std::move($6));
      }
    | RET Expr ";" {
        $$ = ctx.makeReturnStmt(std::move($2));
      }
    | RET ";" {
        $$ = ctx.makeReturnStmt(nullptr);
      }
    | DEFER Expr ";" {
        $$ = std::make_unique<DeferStmt>(std::move($2));
      }
    | PRINT "<-" Expr "{" List_of_expr "}" ";" {
        llvm::SmallVector<std::unique_ptr<Expr>> exprs;
        for (auto &param : $5) {
          exprs.push_back(AddCastIfNeededAndVarRefExpr(std::move(param)));
        }
        $$ = std::make_unique<PrintStmt>(std::move($3), std::move(exprs));
      }
    | DEALLOC Expr ";" {
        auto expr = std::move($2);
        if (!llvm::isa<PtrType>(&expr->getType())) {
          throw quark::QuarkParser::syntax_error("Deallocating non pointer");
        }
        $$ = std::make_unique<DeallocStmt>(std::move(expr));
      }
    | Expr ";"           { $$ = std::make_unique<ExprStmt>(std::move($1)); }
    | Local_var_decl ";" { $$ = std::move($1); }
    | Block              { $$ = std::move($1); };

List_of_elsif: List_of_elsif Elsif_stmt {
                 $1.push_back(std::move($2));
                 $$ = std::move($1);
               }
             | %empty {};

Elsif_stmt: ELSIF "(" Expr ")" Stmt {
              $$ = IfStmt::CondAndStmt(ctx.castToBoolIfNeeded(std::move($3)), std::move($5));
            };

Else_stmt: ELSE Stmt { $$ = std::move($2); };

Local_var_decl: VAR ID ":" Type_declaration "=" Expr  {
                                          ctx.checkNonExistenceOfVarCurrentLevel($2);
                                          auto varDecl = std::make_unique<VarDecl>($2, std::move($4));
                                          varDecl->setKind(VarDeclKind::LocalVar);
                                          ctx.addVar(*varDecl);
                                          $$ = std::make_unique<VarDeclStmt>(std::move(varDecl), AddCastIfNeededAndVarRefExpr(std::move($6)));
                                        }
              | VAR ID ":=" Expr        {
                                          ctx.checkNonExistenceOfVarCurrentLevel($2);
                                          auto expr = std::move($4);
                                          auto varDecl = std::make_unique<VarDecl>($2, expr->getType().clone());
                                          varDecl->setKind(VarDeclKind::LocalVar);
                                          ctx.addVar(*varDecl);
                                          $$ = std::make_unique<VarDeclStmt>(std::move(varDecl), AddCastIfNeededAndVarRefExpr(std::move(expr)));
                                        }
              | VAR ID ":" Type_declaration           {
                                          ctx.checkNonExistenceOfVarCurrentLevel($2);
                                          auto varDecl = std::make_unique<VarDecl>($2, std::move($4));
                                          varDecl->setKind(VarDeclKind::LocalVar);
                                          ctx.addVar(*varDecl);
                                          $$ = std::make_unique<VarDeclStmt>(std::move(varDecl), nullptr);
                                        };

/*List_of_qualifiers: List_of_qualifiers Qualifier
                  | %empty;

Qualifier: MUT;*/

Expr: "(" Expr ")"       { $$ = std::move($2); }
    | Expr "+" Expr      { $$ = ctx.createArithmeticBinaryExpr(BinaryOperatorKind::Add, std::move($1), std::move($3)); }
    | Expr "-" Expr      { $$ = ctx.createArithmeticBinaryExpr(BinaryOperatorKind::Minus, std::move($1), std::move($3)); }
    | Expr "*" Expr      { $$ = ctx.createArithmeticBinaryExpr(BinaryOperatorKind::Mul, std::move($1), std::move($3)); }
    | Expr "/" Expr      { $$ = ctx.createArithmeticBinaryExpr(BinaryOperatorKind::Div, std::move($1), std::move($3)); }
    | Expr "%" Expr      { $$ = ctx.createArithmeticBinaryExpr(BinaryOperatorKind::Mod, std::move($1), std::move($3)); }
    | Expr "=" Expr      {
                            if ($1->isRValue()) {
                              throw quark::QuarkParser::syntax_error("Assigning value to temporary value");
                            }
                            $$ = std::make_unique<BinaryExpr>(BinaryOperatorKind::Assign, std::move($1),
                                                              AddCastIfNeeded(std::move($3)),
                                                              ValueTypeKind::LeftValue);
                         }
    | Expr "!=" Expr     { $$ = ctx.createLogicalBinaryExpr(BinaryOperatorKind::LogicalNotEquals,
                                                            std::move($1), std::move($3));
                         }
    | Expr "==" Expr     { $$ = ctx.createLogicalBinaryExpr(BinaryOperatorKind::LogicalEquals,
                                                            std::move($1), std::move($3));
                         }
    | Expr "&&" Expr     { $$ = ctx.createLogicalBinaryExpr(BinaryOperatorKind::LogicalAnd,
                                                            std::move($1), std::move($3));
                         }
    | Expr "||" Expr     { $$ = ctx.createLogicalBinaryExpr(BinaryOperatorKind::LogicalOr,
                                                            std::move($1), std::move($3));
                         }
    | Expr "<" Expr      { $$ = ctx.createLogicalBinaryExpr(BinaryOperatorKind::LogicalLess,
                                                            std::move($1), std::move($3));
                         }
    | Expr "<=" Expr     {
                           $$ = ctx.createLogicalBinaryExpr(BinaryOperatorKind::LogicalLessEqual,
                                                            std::move($1), std::move($3));
                         }
    | Expr ">" Expr      { $$ = ctx.createLogicalBinaryExpr(BinaryOperatorKind::LogicalGreater,
                                                            std::move($1), std::move($3));
                         }
    | Expr ">=" Expr     { $$ = ctx.createLogicalBinaryExpr(BinaryOperatorKind::LogicalGreaterEqual,
                                                            std::move($1), std::move($3));
                         }
    | "!" Expr           {
                          $$ = std::make_unique<UnaryExpr>(UnaryOperatorKind::LogicalNegation,
                                                           AddCastIfNeeded(std::move($2)),
                                                           ValueTypeKind::RightValue);
                         }
    | "-" Expr %prec "&" {
                          $$ = std::make_unique<UnaryExpr>(UnaryOperatorKind::ArithmeticNegation,
                                                           AddCastIfNeeded(std::move($2)),
                                                           ValueTypeKind::RightValue);
                         }
    | "*" Expr %prec "&" {
                          auto ptrType = llvm::dyn_cast<PtrType>(&$2->getType());
                          if (!ptrType) {
                            throw quark::QuarkParser::syntax_error("Deferencing non pointer");
                          }
                          $$ = std::make_unique<UnaryExpr>(UnaryOperatorKind::Dereference,
                                                           AddCastIfNeeded(std::move($2)),
                                                           ptrType->PointeeType->clone(),
                                                           ValueTypeKind::LeftValue);
                         }
    | "&" Expr           {
                          if ($2->isRValue()) {
                            throw quark::QuarkParser::syntax_error("Cannot take address of r-value");
                          }
                          auto type = $2->getType().clone();
                          $$ = std::make_unique<UnaryExpr>(UnaryOperatorKind::AddressOf,
                                                           std::move($2), std::make_unique<PtrType>(std::move(type)),
                                                           ValueTypeKind::LeftValue);
                         }
    | Func_call          { $$ = std::move($1); }
    | Method_call        { $$ = std::move($1); }
    | ALLOC ID {
                                $$ = std::make_unique<AllocExpr>(std::make_unique<PtrType>(ctx.getType($2)->clone()),
                                                                 std::make_unique<IntegerExpr>(1));
                              }
    | ALLOC ID "[" Expr "]" {
                              auto ptrType = std::make_unique<PtrType>(ctx.getType($2)->clone());
                              $$ = std::make_unique<AllocExpr>(std::move(ptrType), AddCastIfNeeded(std::move($4)));
                            }
    | Term { $$ = std::move($1); };

Type_declaration: List_of_pointers ID List_of_static_arrays  {
                                                        std::unique_ptr<Type> result = ctx.getType($2)->clone();
                                                        for(unsigned i = 0; i < $1; i++) {
                                                          result = std::make_unique<PtrType>(std::move(result));
                                                        }
                                                        for(unsigned size : llvm::reverse($3)) {
                                                          result = std::make_unique<ArrayType>(std::move(result), size);
                                                        }
                                                        $$ = std::move(result);
                                                      }

List_of_dinamic_arrays: List_of_dinamic_arrays "[" Expr "]" {
                                                              $1.push_back(AddCastIfNeeded(std::move($3)));
                                                              $$ = std::move($1);
                                                            }
                      | %empty {};

List_of_static_arrays: List_of_static_arrays "[" INTEGER "]"  {
                                                                $1.push_back($3);
                                                                $$ = std::move($1);
                                                              }
              | %empty {};

List_of_pointers: List_of_pointers "*" { $$ += $1 + 1; }
                | %empty {};

Term: Var_access_array_subscript  { $$ = std::move($1); }
    | Member_access               { $$ = std::move($1); }
    | Literal                     { $$ = std::move($1); };

Func_call: ID "(" List_of_expr ")" {
                                     llvm::SmallVector<std::unique_ptr<Expr>, 4> params;
                                     params.reserve($3.size());
                                     for (auto &param : $3) {
                                       params.push_back(AddCastIfNeeded(std::move(param)));
                                     }

                                     const FuncDecl *func = ctx.getFunctionDecl(FuncDecl::FuncSignature($1, params, nullptr));
                                     $$ = std::make_unique<FunctionCallExpr>(*func, std::move(params));
                                  };

Method_call: Var_access_array_subscript List_of_accesses "(" List_of_expr ")" {
                                                                                std::unique_ptr<Expr> memberExpr = std::move($1);
                                                                                llvm::MutableArrayRef<TypeAccess> accesses = $2;
                                                                                while (accesses.size() > 1) {
                                                                                  memberExpr = GetMemberAccess(std::move(memberExpr), accesses[0]);
                                                                                  accesses = accesses.take_back(accesses.size() - 1);
                                                                                }

                                                                                CheckPtrToValueOrValue(memberExpr->getType());
                                                                                memberExpr = DerefererenceIfNeeded(std::move(memberExpr), accesses[0]);

                                                                                llvm::SmallVector<std::unique_ptr<Expr>, 4> params;
                                                                                params.reserve($4.size());
                                                                                for (auto &param : $4) {
                                                                                  params.push_back(AddCastIfNeeded(std::move(param)));
                                                                                }

                                                                                $$ = std::make_unique<MemberCallExpr>(
                                                                                    *ctx.getFunctionDecl(FuncDecl::FuncSignature(accesses[0].Name, params,
                                                                                                                                 memberExpr->getType().clone())),
                                                                                    std::move(memberExpr), params);
                                                                              };

List_of_expr: List_of_expr1 { $$ = std::move($1); }
            | %empty        {};
List_of_expr1: List_of_expr1 "," Expr {
                                        $1.push_back(std::move($3));
                                        $$ = std::move($1);
                                      }
             | Expr                   { $$.push_back(std::move($1)); };

Var_access_array_subscript: Var_access List_of_dinamic_arrays {
                                                                std::unique_ptr<Expr> result = std::move($1);
                                                                bool loaded = false;
                                                                for (auto &arrayAccess : $2) {
                                                                  if (llvm::isa<PtrType>(&result->getType())) {
                                                                    result = AddCastIfNeeded(std::move(result));
                                                                  }
                                                                  std::unique_ptr<Type> innerType = GetArrayAccessType(*result);
                                                                  result = std::make_unique<ArrayAccessExpr>(std::move(result),
                                                                                                             std::move(innerType),
                                                                                                             std::move(arrayAccess));
                                                                  loaded = true;
                                                                }

                                                                $$ = std::move(result);
                                                              };

Var_access: ID {
                $$ = std::make_unique<VarRefExpr>(*ctx.getVar($1));
               };

Member_access: Var_access_array_subscript List_of_accesses {
                                              std::unique_ptr<Expr> memberAccess = std::move($1);
                                              for (auto &access : $2) {
                                                memberAccess = GetMemberAccess(std::move(memberAccess), access);
                                              }
                                              $$ = std::move(memberAccess);
                                           }

List_of_accesses: List_of_accesses Subtype_access {
                                                    $1.push_back(std::move($2));
                                                    $$ = std::move($1);
                                                  }
                | Subtype_access                  {
                                                    $$.push_back(std::move($1));
                                                  };

Subtype_access: "." ID List_of_dinamic_arrays  { $$ = {TypeAccessKind::Value, $2, std::move($3)}; }
              | "->" ID List_of_dinamic_arrays { $$ = {TypeAccessKind::Pointer, $2, std::move($3)}; };

Literal: ConstNumber  { $$ = std::move($1); }
       | STRING       { $$ = std::make_unique<StringExpr>($1); }
       | CHAR         { $$ = std::make_unique<CharExpr>($1); }
       | TRUE         { $$ = std::make_unique<BooleanExpr>($1); }
       | FALSE        { $$ = std::make_unique<BooleanExpr>($1); };

ConstNumber: INTEGER  { $$ = std::make_unique<IntegerExpr>($1); }
           | REAL     { $$ = std::make_unique<FloatingExpr>($1); };
%%
