%{
open Abstract_syntax_tree
%}

/* tokens */
/**********/

%token TOK_BOOL
%token TOK_INT
%token TOK_VOID
%token TOK_AUTO
%token TOK_TRUE
%token TOK_FALSE
%token TOK_WHILE
%token TOK_IF
%token TOK_ELSE
%token TOK_HALT
%token TOK_RETURN
%token TOK_BREAK
%token TOK_RAND
%token TOK_GOTO
%token TOK_ASSERT
%token TOK_PRINT

%token TOK_LPAREN
%token TOK_RPAREN
%token TOK_LCURLY
%token TOK_RCURLY
%token TOK_STAR
%token TOK_PLUS
%token TOK_MINUS
%token TOK_EXCLAIM
%token TOK_DIVIDE
%token TOK_PERCENT
%token TOK_LESS
%token TOK_GREATER
%token TOK_LESS_EQUAL
%token TOK_GREATER_EQUAL
%token TOK_EQUAL_EQUAL
%token TOK_NOT_EQUAL
%token TOK_AND_AND
%token TOK_BAR_BAR
%token TOK_SEMICOLON
%token TOK_COLON
%token TOK_COMMA
%token TOK_EQUAL

%token <string> TOK_id
%token <string> TOK_int

%token TOK_EOF

/* priorities of binary operators (lowest to highest) */
%left TOK_BAR_BAR
%left TOK_AND_AND
%left TOK_EQUAL_EQUAL TOK_NOT_EQUAL
%left TOK_LESS TOK_GREATER TOK_LESS_EQUAL TOK_GREATER_EQUAL
%left TOK_PLUS TOK_MINUS
%left TOK_STAR TOK_DIVIDE TOK_PERCENT


/* entry-points */
/****************/

%start<Abstract_syntax_tree.toplevel list Abstract_syntax_tree.ext> file


%%


/* toplevel */
/************/

file: t=ext(list(toplevel)) TOK_EOF { t }

toplevel:
| d=ext(stat)           { AST_stat d }
| d=ext(fun_decl)       { AST_fun_decl d }


/* expressions */
/***************/

primary_expr:
| TOK_LPAREN e=expr TOK_RPAREN     { e }
| e=ext(TOK_id)                    { AST_identifier e }
| e=ext(TOK_int)                   { AST_int_const e }
| TOK_TRUE                         { AST_bool_const true }
| TOK_FALSE                        { AST_bool_const false }
| TOK_RAND TOK_LPAREN e1=ext(sign_int_literal)  
           TOK_COMMA  e2=ext(sign_int_literal) TOK_RPAREN
  { AST_int_rand (e1, e2) }
| e=ext(TOK_id) TOK_LPAREN l=separated_list(TOK_COMMA,ext(expr)) TOK_RPAREN TOK_SEMICOLON
   { AST_expr_call (e, l) }

/* integer with optional sign */
sign_int_literal:
| i=TOK_int            { i }
| TOK_PLUS i=TOK_int   { i }
| TOK_MINUS i=TOK_int  { "-"^i }


unary_expr:
| e=primary_expr                   { e }
| o=unary_op e=ext(unary_expr)     { AST_unary (o, e) }

%inline unary_op:
| TOK_PLUS           { AST_UNARY_PLUS }
| TOK_MINUS          { AST_UNARY_MINUS }
| TOK_EXCLAIM        { AST_NOT }


binary_expr:
| e=unary_expr                                        { e }
| e=ext(binary_expr) o=binary_op f=ext(binary_expr)   { AST_binary (o, e, f) }

%inline binary_op:
| TOK_STAR           { AST_MULTIPLY }
| TOK_DIVIDE         { AST_DIVIDE }
| TOK_PERCENT        { AST_MODULO }
| TOK_PLUS           { AST_PLUS }
| TOK_MINUS          { AST_MINUS }
| TOK_LESS           { AST_LESS }
| TOK_GREATER        { AST_GREATER }
| TOK_LESS_EQUAL     { AST_LESS_EQUAL }
| TOK_GREATER_EQUAL  { AST_GREATER_EQUAL }
| TOK_EQUAL_EQUAL    { AST_EQUAL }
| TOK_NOT_EQUAL      { AST_NOT_EQUAL }
| TOK_AND_AND        { AST_AND }
| TOK_BAR_BAR        { AST_OR }

expr:
| e=binary_expr { e }

lvalue:
| i=TOK_id   { i }


/* declarations */
/****************/

var_decl:
| s=ext(typ) i=separated_list(TOK_COMMA,init_declarator) TOK_SEMICOLON
  { s, i }

init_declarator:
| v=ext(TOK_id)                         { v, None }
| v=ext(TOK_id) TOK_EQUAL i=ext(expr)   { v, Some i }

fun_decl:
| t=ext(typ_or_void) i=ext(TOK_id)
        TOK_LPAREN p=separated_list(TOK_COMMA,param_decl) TOK_RPAREN 
        b=block
  { { Abstract_syntax_tree.fun_name = i;
      Abstract_syntax_tree.fun_typ = t;
      Abstract_syntax_tree.fun_args = p;
      Abstract_syntax_tree.fun_body = b; }
  }

param_decl:
| s=ext(typ) v=ext(TOK_id) { s, v }

typ:
| TOK_INT    { AST_TYP_INT }
| TOK_BOOL   { AST_TYP_BOOL }
| TOK_AUTO   { AST_TYP_AUTO }

%inline typ_or_void:
| t=typ      { Some t }
| TOK_VOID   { None }


/* statements */
/**************/

block:
| TOK_LCURLY l=list(ext(stat)) TOK_RCURLY  { l }

stat:
| l=block                     
  { AST_block l }

| e=ext(lvalue) TOK_EQUAL f=ext(expr) TOK_SEMICOLON
  { AST_assign (e, f) }

| TOK_IF TOK_LPAREN e=ext(expr) TOK_RPAREN s=ext(stat)
  { AST_if (e, s, None) }

| TOK_IF TOK_LPAREN e=ext(expr) TOK_RPAREN s=ext(stat) TOK_ELSE t=ext(stat) 
  { AST_if (e, s, Some t) }

| TOK_WHILE TOK_LPAREN e=ext(expr) TOK_RPAREN s=ext(stat)
  { AST_while (e, s) }

| TOK_ASSERT TOK_LPAREN e=ext(expr) TOK_RPAREN TOK_SEMICOLON
  { AST_assert e }

| TOK_PRINT TOK_LPAREN l=separated_list(TOK_COMMA,ext(lvalue)) TOK_RPAREN TOK_SEMICOLON
  { AST_print l }

| v=var_decl
  { AST_local v }

| e=ext(TOK_id) TOK_LPAREN l=separated_list(TOK_COMMA,ext(expr)) TOK_RPAREN TOK_SEMICOLON
  { AST_stat_call (e, l) }

| TOK_RETURN e=option(ext(expr)) TOK_SEMICOLON
  { AST_return e }

| TOK_BREAK TOK_SEMICOLON
  { AST_BREAK }

| TOK_HALT TOK_SEMICOLON
  { AST_HALT }

| TOK_GOTO l=ext(TOK_id) TOK_SEMICOLON
  { AST_goto l }

| l=ext(TOK_id) TOK_COLON
  { AST_label l }



/* utilities */
/*************/

/* adds extent information to rule */
%inline ext(X): 
| x=X { x, ($startpos, $endpos) }


%%
