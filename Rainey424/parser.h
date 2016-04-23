/*****************************************************************************************
File name:	parser.h
Compiler:	MS Visual Studio 2012
Author:		Warren Rainey 040740424
Course:		CST 8152 - Compilers, Lab Section: 011 
Assignment: 4 Parser
Date:		December 11 2015 
Professor:	Sv. Ranev 
Purpose:	Defines functions for PLATYPUS language productions, token matching,
			and error printing.
*******************************************************************************************/

#ifndef PARSER_H
#define PARSER_H

#include "token.h"
#include "buffer.h"
#include "stable.h"

static Token lookahead;
static Buffer *sc_buf;
int synerrno;

enum
{
    ELSE,
    IF,
    INPUT,
    OUTPUT,
    PLATYPUS,
    REPEAT,
    THEN,
    USING
};

/*  DEFINES  */
#define NO_ATTR -1

extern char * kw_table[];
extern int line;         
extern STD sym_table;    
extern Buffer * str_LTBL; 
extern Token mlwpar_next_token(Buffer *);

/*   FUNCTION DECLARATIONS  */
void parser(Buffer *);
void match(int pr_token_code,int pr_token_attribute);
void syn_eh(int sync_token_code);
void syn_printe();
void gengen_incode(char *);
void program(void);
void opt_statements();
void statements();
void statement();
void statements_prime();
/*Add The rest here*/
void assignment_statement();
void assignment_expression();
void selection_statement();
void iteration_statement();
void input_statement();
void variable_list();
void variable_identifier_prime();
void variable_identifier();
void output_statement();
void opt_variable_list();
void arithmetic_expression();
void unary_arithmetic_expression();
void additive_arithmetic_expression();
void additive_arithmetic_expression_prime();
void multiplicative_arithmetic_expression();
void multiplicative_arithmetic_expression_prime();
void primary_arithmetic_expression();
void string_expression();
void string_expression_prime();
void primary_string_expression();
void conditional_expression();
void logical_or_expression();
void logical_or_expression_prime();
void logical_and_expression();
void logical_and_expression_prime();
void relational_expression();
void relational_expression_prime();
void relational_expression_doubleprime();
void primary_a_relational_expression();
void primary_s_relational_expression();

#endif
