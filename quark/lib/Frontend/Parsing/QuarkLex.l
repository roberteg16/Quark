%option noyywrap nounput noinput batch
%{
  #include "QuarkParser.hpp" /* for token definitions and yylval */
  #define tk(tk)                                                               \
      ctx.Loc.columns(yyleng);                                                 \
      return quark::QuarkParser::make_##tk(ctx.Loc)
  #define tkv(tk, v)                                                           \
      ctx.Loc.columns(yyleng);                                                 \
      return quark::QuarkParser::make_##tk(v, ctx.Loc)

  # define YY_DECL                                                             \
    quark::QuarkParser::symbol_type yylex (quark::LexContext &ctx,             \
                                           quark::SourceModule &sm)
  // ... and declare it for the parser's sake.
  YY_DECL;
%}

DIGIT [0-9]
ID [a-zA-Z][a-zA-Z0-9]*
STRING \".*\"

%%
  // Update current location
  ctx.Loc.step();

"module"            { tk(MODULE); }
"export"            { tk(EXPORT); }
"import"            { tk(IMPORT); }
"for"               { tk(FOR); }
"par_for"           { tk(PAR_FOR); }
"while"             { tk(WHILE); }
"if"                { tk(IF); }
"elsif"             { tk(ELSIF); }
"else"              { tk(ELSE); }
"ret"               { tk(RET); }
"defer"             { tk(DEFER); }
"type"              { tk(TYPE); }
"fn"                { tk(FN); }
"alloc"             { tk(ALLOC); }
"dealloc"           { tk(DEALLOC); }
"var"               { tk(VAR); }
"mut"               { tk(MUT); }
"print"             { tk(PRINT); }
"<-"                { tk(INV_ARROW); }
"true"              { tkv(TRUE, true); }
"false"             { tkv(FALSE, false); }
"+"                 { tk(PLUS); }
"-"                 { tk(MINUS); }
"/"                 { tk(SLASH); }
"*"                 { tk(ASTERISK); }
"&"                 { tk(AMPERSAND); }
"%"                 { tk(MOD); }
"="                 { tk(EQUAL); }
"||"                { tk(PIPEPIPE); }
"<"                 { tk(LESS); }
">"                 { tk(GREAT); }
"&&"                { tk(AMPERSANDAMPERSAND); }
"!"                 { tk(EXCLAMATION); }
"=="                { tk(EQUALEQUAL); }
"!="                { tk(EXCLAMATIONEQUAL); }
">="                { tk(GREATEQUAL); }
"<="                { tk(LESSEQUAL); }
"->"                { tk(ARROW); }
":="                { tk(DOUBLEDOTEQUAL); }
":"                 { tk(DOUBLEDOT); }
"."                 { tk(DOT); }
";"                 { tk(SEMICOLON); }
","                 { tk(COMMA); }
"{"                 { tk(LEFTKEY); }
"}"                 { tk(RIGHTKEY); }
"["                 { tk(LEFTBRACE); }
"]"                 { tk(RIGHTBRACE); }
"("                 { tk(LEFTPARENTHESIS); }
")"                 { tk(RIGHTPARENTHESIS); }
{ID}                { tkv(ID, llvm::StringRef(yytext)); }
{DIGIT}+            { tkv(INTEGER, std::stoi(yytext)); }
{DIGIT}+"."{DIGIT}* { tkv(REAL, std::stod(yytext)); }
{STRING}            { tkv(STRING, llvm::StringRef(yytext).drop_front(1)
                                                         .drop_back(1)); }
'.{1}'              { tkv(CHAR, yytext[1] ); }
"//".*              { return yylex(ctx, sm);}
[ ]                 { ctx.Loc.columns(); return yylex(ctx, sm);}
[\t]                { ctx.Loc.columns(); return yylex(ctx, sm);}
[\n]                { ctx.Loc.lines();  return yylex(ctx, sm); }
<<EOF>>             { tk(END); }
.                   {
                      std::string str("Unrecognized char '");
                      str += yytext[0];
                      str += "'";
                      throw quark::QuarkParser::syntax_error(ctx.Loc, str);
                    }
%%
