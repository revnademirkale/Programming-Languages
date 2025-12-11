%{
#include <stdio.h>
#include <stdlib.h>

int yylex(void);
void yyerror(const char *s);
%}


%token tPLUS tMINUS tMUL tEXP tLPR tRPR tASSIGN tCOMMA tSEMICOLON
%token tCALCULATE tDERIVATION tINTEGRATION tIDENTIFIER tINTEGER


%left tPLUS tMINUS
%left tMUL
%right tEXP

%%



program
    : def_block calc_block
    ;


def_block
    : /* boş olabilir */
    | def_block func_def
    ;

func_def
    : tIDENTIFIER tLPR param_list tRPR tASSIGN ordinary_expr tSEMICOLON
    ;

param_list
    : /* boş olabilir */
    | var_list
    ;

var_list
    : tIDENTIFIER
    | var_list tCOMMA tIDENTIFIER
    ;


calc_block
    : /* boş olabilir */
    | calc_block calc_stmt
    ;

calc_stmt
    : tCALCULATE extended_expr tSEMICOLON
    ;



ordinary_expr
    : tINTEGER
    | tIDENTIFIER
    | tLPR ordinary_expr tRPR
    | ordinary_expr tPLUS ordinary_expr
    | ordinary_expr tMINUS ordinary_expr
    | ordinary_expr tMUL ordinary_expr
    | ordinary_expr tEXP tINTEGER
    | ordinary_expr ordinary_expr  /* implicit multiplication */
    ;



extended_expr
    : tINTEGER
    | tIDENTIFIER
    | tLPR extended_expr tRPR
    | extended_expr tPLUS extended_expr
    | extended_expr tMINUS extended_expr
    | extended_expr tMUL extended_expr
    | extended_expr tEXP tINTEGER
    | extended_expr extended_expr   /* implicit multiplication */
    | tDERIVATION tLPR tIDENTIFIER tCOMMA tIDENTIFIER tCOMMA tINTEGER tRPR
    | tINTEGRATION tLPR tIDENTIFIER tCOMMA tIDENTIFIER tCOMMA tINTEGER tRPR
    | function_call
    ;


function_call
    : tIDENTIFIER tLPR expr_list tRPR
    ;


expr_list
    : 
    | extended_expr
    | expr_list tCOMMA extended_expr
    ;

%%



void yyerror(const char *s) { }

int main() {
    if (yyparse())
        printf("ERROR\n");
    else
        printf("OK\n");
    return 0;
}
