/*****************************************************************************************
File name: parser.c 
Compiler: MS Visual Studio 2012
Author: Warren Rainey 040740424
Course: CST 8152 - Compilers, Lab Section: 011 
Assignment: 4 Parser
Date: December 11 2015
Professor: Sv. Ranev 
Production: To validate that the program is syntacticaly correct
Function list:	void parser(Buffer *);
				void match(int pr_token_code,int pr_token_attribute);
				void syn_eh(int sync_token_code);
				void syn_printe();
				All Future productions
*******************************************************************************************/ 
#include "parser.h"
#include<stdio.h>
#include <stdlib.h>
/********************************************************************
Function name:      parser()
Author:             Svillen Ranev
History/Versions:   1.0
**********************************************************************/ 
void parser(Buffer * in_buf){
  sc_buf = in_buf;
  lookahead = mlwpar_next_token(sc_buf);
  program(); 
  match(SEOF_T,NO_ATTR);
  gen_incode("PLATY: Source file parsed\n");
}

/********************************************************************
Function name:      match()
Author:             Warren Rainey
History/Versions:   1.0
Match function, Assignmet 4, F15
 ********************************************************************/
void match(int pr_token_code,int pr_token_attribute){
	  /* If the token codes match, verify the attributes */
    if (pr_token_code == lookahead.code)
    {
        switch (pr_token_code)
        {
        case ART_OP_T: 
        case REL_OP_T:
        case LOG_OP_T: 
        case KW_T: 
            /* If the attributes do not match, break to print error */
            if (pr_token_attribute != lookahead.attribute.get_int)
                break;
        default:
            if (lookahead.code == SEOF_T){/*Remember to return if next = seof*/
                return;
			}
            /* Get the next token */
            lookahead = mlwpar_next_token(sc_buf);
         
            if (lookahead.code == ERR_T)
            {
                syn_printe();
                ++synerrno;/* increments the error counter synerrno, and returns. */
                
                lookahead = mlwpar_next_token(sc_buf);
            }

            return;
        }
    }

    /* Result from break */
    syn_eh(pr_token_code);
}

/********************************************************************
Function name:      syn_printe()
Author:             Svillen Ranev
History/Versions:   1.0
Parser error printing function, Assignmet 4, F15
 ********************************************************************/
void syn_eh(int sync_token_code){
	syn_printe();
    synerrno++;
    while(lookahead.code != SEOF_T){
		
        lookahead = mlwpar_next_token(sc_buf);      
        if (lookahead.code == sync_token_code){    
            if (lookahead.code != SEOF_T){
                lookahead = mlwpar_next_token(sc_buf);
			}
			/*If the Token is SEOF  */
            return;
        }
    }
    if (sync_token_code != SEOF_T){
        exit(synerrno);
	}
}

/********************************************************************
Function name:      syn_printe()
Author:             Svillen Ranev
History/Versions:   1.0
Parser error printing function, Assignmet 4, F15
 ********************************************************************/
void syn_printe(){
Token t = lookahead;

printf("PLATY: Syntax error:  Line:%3d\n",line);
printf("*****  Token code:%3d Attribute: ", t.code);
switch(t.code){
	case  ERR_T: /* ERR_T     0   Error token */
		printf("%s\n",t.attribute.err_lex);
	 break;
	case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
		printf("NA\n" );
	 break;
	case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
	case  SVID_T :/* SVID_T    3  String Variable identifier token */
		printf("%s\n",sym_table.pstvr[t.attribute.get_int].plex);
	 break;
	case  FPL_T: /* FPL_T     4  Floating point literal token */
		printf("%5.1f\n",t.attribute.flt_value);
	 break;
	case INL_T: /* INL_T      5   Integer literal token */
	        printf("%d\n",t.attribute.get_int);
	 break;
	case STR_T:/* STR_T     6   String literal token */
	        printf("%s\n",b_setmark(str_LTBL,t.attribute.str_offset));
	break;
        
        case SCC_OP_T: /* 7   String concatenation operator token */
	        printf("NA\n" );
	break;
	
	case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
		printf("NA\n" );
	break;
	case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
		printf("%d\n",t.attribute.get_int);
	break;
	case  REL_OP_T: /*REL_OP_T  10   Relational operator token */ 
		printf("%d\n",t.attribute.get_int);
	break;
	case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
		printf("%d\n",t.attribute.get_int);
	break;
	
	case  LPR_T: /*LPR_T    12  Left parenthesis token */
		printf("NA\n" );
	break;
	case  RPR_T: /*RPR_T    13  Right parenthesis token */
	        printf("NA\n" );
	break;
	case LBR_T: /*    14   Left brace token */
	        printf("NA\n" );
	break;
	case RBR_T: /*    15  Right brace token */
	        printf("NA\n" );
	break;
		
	case KW_T: /*     16   Keyword token */
	        printf("%s\n",kw_table [t.attribute.get_int]);
	break;
	
	case COM_T: /* 17   Comma token */
	        printf("NA\n");
	break;
	case EOS_T: /*    18  End of statement *(semi - colon) */
	        printf("NA\n" );
	break; 		
	default:
	        printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
    }/*end switch*/
}/* end syn_printe()*/

/********************************************************************
Function name:      gen_incode()
Author:             Warren Rainey
Takes in string and prints it
Modiefied for bonus
**********************************************************************/ 
void gen_incode(char * c){
	printf("%s", c);
}
/********************************************************************
Function name:      program()
Production:         program grammar production
Author:             Svillen Ranev
FIRST SET:			{KW_T(PLATYPUS)}
PRODUCTION:			<program> -> PLATYPUS{<opt_statments>}
**********************************************************************/ 
void program(void){
	match(KW_T,PLATYPUS);
	match(LBR_T,NO_ATTR);
	opt_statements();
	match(RBR_T,NO_ATTR);
	gen_incode("PLATY: Program parsed\n");
}

/********************************************************************
Function name:      opt_statements()
Production:         opt_statements grammar production
Author:             Svillen Ranev
FIRST SET:			{AVID_T,SVID_T,KW_T(IF),KW_T(USING),KW_T(INPUT),KW_T(OUTPUT), E }
PRODUCTION:			<opt_statments> -> <statements>|Ɛ
**********************************************************************/ 
void opt_statements(){
	switch(lookahead.code){
		case AVID_T:
		case SVID_T: 
			statements();
			break;
		case KW_T:	
			if (lookahead.attribute.get_int != PLATYPUS && lookahead.attribute.get_int != ELSE && lookahead.attribute.get_int != THEN  && lookahead.attribute.get_int != REPEAT){
			statements();
			break; 
		}
		default: /*empty string – optional statements*/ ;
		gen_incode("PLATY: Opt_statements parsed\n");
	} 
}
/********************************************************************
Function name:      statements()
Production:         statements grammar production
Author:             Warren Rainey
FIRST SET:			{AVID , SVID ,KW_T(IF), KW_T(USING) , KW_T(INPUT) , KW_T(OUTPUT) }
PRODUCTION:			<statements> -> <statement><statements’>
**********************************************************************/ 
void statements(){
	statement();
	statements_prime();
}

/********************************************************************
Function name:      statement()
Production:         statements grammar production
Author:             Warren Rainey
FIRST SET:			{AVID , SVID ,KW_T(IF), KW_T(USING) , KW_T(INPUT) , KW_T(OUTPUT) }
PRODUCTION:			<statements> -> <statement><statements’>
**********************************************************************/ 
void statement(){
	switch(lookahead.code){
    case AVID_T:
    case SVID_T:
        //Eventually 
		assignment_statement();
        break;
    case KW_T:
        switch(lookahead.attribute.kwt_idx){/*idx index from Token.h*/
        case IF:
       //Eventually 
			selection_statement();
            break;
        case USING:
        //Eventually    
			iteration_statement();
            break;
        case INPUT:
        //Eventually    
			input_statement();
            break;
        case OUTPUT:
         //Eventually   
			output_statement();
            break;
        }
        break;
    default:
        syn_printe();
    }
}

/********************************************************************
Function name:      statements_prime()
Production:         Statements grammar production
Author:             Warren Rainey
FIRST SET:			{AVID , SVID ,KW_T(IF), KW_T(USING) , KW_T(INPUT) , KW_T(OUTPUT) }
PRODUCTION:			<statements> -> <statement><statements’>
**********************************************************************/ 
void statements_prime(){
	switch(lookahead.code){
    case KW_T:
        switch(lookahead.attribute.kwt_idx){
        case IF:
        case USING:
		case INPUT:
        case OUTPUT:
            break;
        default: 
            return;
        }
    case AVID_T:
    case SVID_T:
        statement();
        statements_prime();
        break;
	default:
		break;
    }
}

/********************************************************************
Function name:      assignment_statement()
Production:         Assignment statement grammar production
Author:             Warren Rainey
FIRST SET:			{ AVID_T, SVID_T }
PRODUCTION:			<assignment statement> ->  <assignment expression>;
**********************************************************************/ 
void assignment_statement(){
	assignment_expression();
	match(EOS_T,NO_ATTR);
    gen_incode("PLATY: Assignment statement parsed\n");
}
/********************************************************************
Function name:      assignment_expression()
Production:         Assignment expression grammar production
Author:             Warren Rainey
FIRST SET:			{ AVID_T, SVID_T }
PRODUCTION:			< assignment expression> ->  AVID = <arithmetic expression> | SVID = <string expression>
**********************************************************************/ 
void assignment_expression(){
	switch(lookahead.code){
		case AVID_T:
			match(AVID_T, NO_ATTR);
			match(ASS_OP_T, NO_ATTR);
			arithmetic_expression();
			gen_incode("PLATY: Assignment expression (arithmetic) parsed\n");
			break;
		case SVID_T:
			match(SVID_T, NO_ATTR);
			match(ASS_OP_T, NO_ATTR);
			string_expression();
			gen_incode("PLATY: Assignment expression (string) parsed\n");
			break;
		default: 
			syn_printe();
			return;
    }
}
/********************************************************************
Function name:      selection_statement()
Production:         Selection statement grammar production
Author:             Warren Rainey
FIRST SET:			{ KW_T(IF) }
PRODUCTION:			<selection statement> ->  IF (<conditional expression>)  THEN <opt_statements> ELSE { <opt_statements> } ;
**********************************************************************/ 
void selection_statement(){
	match(KW_T, IF);
    match(LPR_T, NO_ATTR);
    conditional_expression();
    match(RPR_T, NO_ATTR);
    match(KW_T, THEN);
    opt_statements();
    match(KW_T, ELSE);
    match(LBR_T, NO_ATTR);
    opt_statements();
    match(RBR_T, NO_ATTR);
    match(EOS_T, NO_ATTR);
    gen_incode("PLATY: IF statement parsed\n");
}
/********************************************************************
Function name:      iteration_statement()
Production:         Iteration statement grammar production
Author:				Warren Rainey
FIRST SET:			{ KW_T(USING) }
PRODUCTION:			<iteration statement> ->
						 USING  (<assignment expression> , <conditional expression> , <assignment  expression> )
						 REPEAT {
								<opt_statements>
								};
**********************************************************************/ 
void iteration_statement(){
	match(KW_T, USING);
    match(LPR_T, NO_ATTR);
    assignment_expression();
    match(COM_T, NO_ATTR);
    conditional_expression();
    match(COM_T, NO_ATTR);
    assignment_expression();
    match(RPR_T, NO_ATTR);
    match(KW_T, REPEAT);
    match(LBR_T, NO_ATTR);
    opt_statements();
    match(RBR_T, NO_ATTR);
    match(EOS_T, NO_ATTR);
    gen_incode("PLATY: USING statement parsed\n");
}
/********************************************************************
Function name:      input_statement()
Production:         Input statement grammar production
Author:             Warren Rainey
FIRST SET:			{ KW_T(INPUT) }
PRODUCTION:			<input statement> -> INPUT (<variable list>);
**********************************************************************/ 
void input_statement(){
	match(KW_T,INPUT);
	match(LPR_T,NO_ATTR);
	variable_list();
	match(RPR_T,NO_ATTR);
	match(EOS_T,NO_ATTR);
	gen_incode("PLATY: INPUT statement parsed\n"); 
}
/********************************************************************
Function name:      variable_list()
Production:         Variable list grammar production
Author:             Warren Rainey
FIRST SET:			{ AVID_T, SVID_T }
PRODUCTION:			<variable list> -> <variable identifier>  <variable identifier’>
**********************************************************************/ 
void variable_list(){
	variable_identifier();
	variable_identifier_prime();
    gen_incode("PLATY: Variable list parsed\n");
}
/********************************************************************
Function name:      variable_identifier
Production:         Variable identifier grammar production
Author:             Warren Rainey
FIRST SET:			None
PRODUCTION:			<variable identifier> -> AVID_T | SVID_T
**********************************************************************/ 
void variable_identifier(){
	switch(lookahead.code){
    case AVID_T:
    case SVID_T:
		match(lookahead.code, NO_ATTR);
        break;
    default: 
        syn_printe();
        return;
    }
}
/********************************************************************
Function name:      variable_identifier_prime()
Production:         Variable identifier prime grammar production
Author:             Warren Rainey
FIRST SET:			{, , E }
PRODUCTION:			<variable identifier’> -> ,<variable identifier> <variable identifier’>|Ɛ
**********************************************************************/ 
void variable_identifier_prime(){
	if(lookahead.code == COM_T){
        match(lookahead.code, NO_ATTR);
        variable_identifier();
		variable_identifier_prime();
    }
}
/********************************************************************
Function name:      output_statement
Production:         Output statement grammar production
Author:             Warren Rainey
FIRST SET:			{ KW_T(OUTPUT) }
PRODUCTION:			<output statement> ->  OUTPUT (<opt_variable list>);
**********************************************************************/ 
void output_statement(){
	match(KW_T, OUTPUT);
    match(LPR_T, NO_ATTR);
	opt_variable_list();
    match(RPR_T, NO_ATTR);
    match(EOS_T, NO_ATTR);
    gen_incode("PLATY: OUTPUT statement parsed\n");
}
/********************************************************************
Function name:      opt_variable_list
Production:         opt_variable_list grammar production
Author:             Warren Rainey
FIRST SET:			{ AVID_T, SVID_T, STR_T, E }
PRODUCTION:			<opt_variable list> ->variablelist | STR | Ɛ
**********************************************************************/ 
void opt_variable_list(){
	switch(lookahead.code){
    case AVID_T:
    case SVID_T:
        variable_list();
        break;
    case STR_T:
        match(lookahead.code, NO_ATTR);
        gen_incode("PLATY: Output list (string literal) parsed\n");
        break;
    default: 
        gen_incode("PLATY: Output list (empty) parsed\n");
    }
}
/********************************************************************
Function name:      arithmetic_expression
Production:         Arithmetic expression grammar production
Author:             Warren Rainey
FIRST SET:			{ -, +, AVID_T, FPL_T, INL_T, ( }
PRODUCTION:			<arithmetic expression> - >
						   <unary arithmetic expression> | <additive arithmetic expression>
**********************************************************************/ 
void arithmetic_expression(){
	switch(lookahead.code){
    case ART_OP_T:
        switch(lookahead.attribute.arr_op){
        case MULT:
        case DIV:
            syn_printe(); 
            return;
        default:	
            break;
        }
        unary_arithmetic_expression();
        break;
    case AVID_T:
    case FPL_T:
    case INL_T:
    case LPR_T:
        additive_arithmetic_expression();
        break;
    default:	
        syn_printe();
        return;
    }
    gen_incode("PLATY: Arithmetic expression parsed\n");
}

/********************************************************************
Function name:      unary_arithmetic_expression
Production:         Unary arithmetic_expression grammar production
Author:             Warren Rainey
FIRST SET:			{ -, + }
PRODUCTION:			<unary arithmetic expression> ->
  							-  <primary arithmetic expression> | + <primary arithmetic expression>
**********************************************************************/ 
void unary_arithmetic_expression(){
    switch(lookahead.code){
    case ART_OP_T:
        switch(lookahead.attribute.arr_op){
        case MULT:
        case DIV:
            syn_printe(); 
            return;
        default:
            break;
        }
        match(lookahead.code, lookahead.attribute.arr_op);
        primary_arithmetic_expression();
        gen_incode("PLATY: Unary arithmetic expression parsed\n");
        break;
    default:	
        syn_printe();
        return;
    }
}
/********************************************************************
Function name:      additive_arithmetic_expression
Production:         Additive arithmetic expression grammar production
Author:             Warren Rainey
FIRST SET:			{ AVID_T, FPL_T, INL_T, ( }
PRODUCTION:			<additive arithmetic expression> -> 
							<multiplicative arithmetic expression> <additive arithmetic expression’>
**********************************************************************/  
void additive_arithmetic_expression(){
	multiplicative_arithmetic_expression();
    additive_arithmetic_expression_prime();
}
/********************************************************************
Function name:      additive_arithmetic_expression_prime()
Production:         Additive arithmetic expression prime grammar production
Author:             Warren Rainey
FIRST SET:			{ +, -, E }
PRODUCTION:			<additive arithmetic expression’> ->
							  +  <multiplicative arithmetic expression> additive arithmetic expression’>
							| -  <multiplicative arithmetic expression> additive arithmetic expression’>
							| Ɛ
**********************************************************************/ 
void additive_arithmetic_expression_prime(){
	  if (lookahead.code == ART_OP_T){
        switch(lookahead.attribute.arr_op){
        case MULT:
        case DIV:           
            return;
        default: 
            break;
        }
        match(lookahead.code, lookahead.attribute.arr_op); 
        multiplicative_arithmetic_expression();
        additive_arithmetic_expression_prime();
        gen_incode("PLATY: Additive arithmetic expression parsed\n");
    }
}
/********************************************************************
Function name:      multiplicative_arithmetic_expression
Production:         Multiplicative arithmetic expression grammar production
Author:             Warren Rainey
FIRST SET:			{ AVID_T, FPL_T, INL_T, ( }
PRODUCTION:			<multiplicative arithmetic expression> -> <primary arithmetic expression> <multiplicative arithmetic expression’>
**********************************************************************/ 
void multiplicative_arithmetic_expression(){
	primary_arithmetic_expression();
	multiplicative_arithmetic_expression_prime();
}
/********************************************************************
Function name:      multiplicative_arithmetic_expression_prime
Production:         Multiplicative arithmetic expression prime grammar production
Author:             Warren Rainey
FIRST SET:			{ *, / , E }
PRODUCTION:			<multiplicative arithmetic expression’>->
							  * <primary arithmetic expression><multiplicative arithmetic expression’>
							| / <primary arithmetic expression><multiplicative arithmetic expression’>
							| Ɛ
**********************************************************************/ 
void multiplicative_arithmetic_expression_prime(){
	if (lookahead.code == ART_OP_T){
        switch(lookahead.attribute.arr_op){
        case PLUS:
        case MINUS:
            return;
        default: 
            break;
        }
        match(lookahead.code, lookahead.attribute.arr_op);
        primary_arithmetic_expression();
        multiplicative_arithmetic_expression_prime();
        gen_incode("PLATY: Multiplicative arithmetic expression parsed\n");
    }
}
/********************************************************************
Function name:      primary_arithmetic_expression()
Production:         Primary arithmetic expression grammar production
Author:             Warren Rainey
FIRST SET:			{ AVID_T, FPL_T, INL_T, ( }
PRODUCTION:			<primary arithmetic expression> ->
								  AVID_T
								| FPL_T
								| INL_T
								| (<arithmetic expression>)	
**********************************************************************/ 
void primary_arithmetic_expression(){
	switch(lookahead.code){
    case AVID_T:
    case FPL_T:
    case INL_T:
        match(lookahead.code, lookahead.attribute.arr_op);
        break;
    case LPR_T:
        match(lookahead.code, lookahead.attribute.arr_op);
        arithmetic_expression();
        match(RPR_T, NO_ATTR);
        break;
    default:	
        syn_printe();
        return;
    }
    gen_incode("PLATY: Primary arithmetic expression parsed\n");
}
/********************************************************************
Function name:      string_expression()
Production:         String expression grammar production
Author:             Warren Rainey
FIRST SET:			{ SVID_T, STR_T }
PRODUCTION:			<string expression> -> <primary string expression> <string expression’>  
**********************************************************************/ 
void string_expression(){
	primary_string_expression();
	string_expression_prime();

}
/********************************************************************
Function name:      string_expression_prime()
Production:         String expression prime grammar production
Author:             Warren Rainey
FIRST SET:			{ SCC_OP_T, E }
PRODUCTION:			<string expression’> -> #  <primary string expression> <string expression’>  | Ɛ
**********************************************************************/ 
void string_expression_prime(){
	if (lookahead.code == SCC_OP_T){
        match(lookahead.code, lookahead.attribute.arr_op);
        primary_string_expression();
        string_expression_prime();
    }else{ 
        gen_incode("PLATY: String expression parsed\n");
	}
}
/********************************************************************
Function name:      primary_string_expression()
Production:         Primary string expression grammar production
Author:             Warren Rainey
FIRST SET:			{ SVID_T, STR_T }
PRODUCTION:			<primary string expression> ->	SVID_T| STR_T
**********************************************************************/ 
void primary_string_expression(){
	switch(lookahead.code){
    case SVID_T:
    case STR_T:
        match(lookahead.code, lookahead.attribute.arr_op);
        break;
    default:	
        syn_printe();
    }
    gen_incode("PLATY: Primary string expression parsed\n");
}

/********************************************************************
Function name:      conditional_expression()
Production:         Conditional expression grammar production
Author:             Warren Rainey
FIRST SET:			{ AVID_T, FPL_T, INL_T, SVID_T, STR_T }
PRODUCTION:			<conditional expression> ->  <logical OR  expression>
**********************************************************************/ 
void conditional_expression(){
    logical_or_expression();
    gen_incode("PLATY: Conditional expression parsed\n");
}
/********************************************************************
Function name:      logical_or_expression()
Production:         Logical OR expression grammar production
Author:             Warren Rainey
FIRST SET:			{ AVID_T, FPL_T, INL_T, SVID_T, STR_T  }
PRODUCTION:			<logical  OR expression> -> <logical AND expression> <logical OR expression’>   
**********************************************************************/ 
void logical_or_expression(){
	logical_and_expression();
    logical_or_expression_prime();
}
/********************************************************************
Function name:      logical_or_expression_prime()
Production:         Logical OR expression prime grammar production
Author:             Warren Rainey
FIRST SET:			{ .OR.,  E }
PRODUCTION:			<logical OR expression’> -> .OR. <logical AND expression><logical OR expression’> | Ɛ
**********************************************************************/  
void logical_or_expression_prime(){
	 if (lookahead.code == LOG_OP_T){
        switch(lookahead.attribute.log_op){
        case AND:
            return;
        default: 
            break;
        }
        match(lookahead.code, lookahead.attribute.arr_op);
        logical_and_expression();
        logical_or_expression_prime();
        gen_incode("PLATY: Logical OR expression parsed\n");
    }
}
/********************************************************************
Function name:      logical_and_expression()
Production:         Logical AND expression grammar production
Author:             Warren Rainey
FIRST SET:			{ AVID_T, FPL_T, INL_T, SVID_T, STR_T }
PRODUCTION:			<logical  AND expression> -> <relational expression> <logical AND expression’>   
**********************************************************************/ 
void logical_and_expression(){
	relational_expression();
    logical_and_expression_prime();
}
/********************************************************************
Function name:      logical_and_expression_prime()
Production:         Logical and expression prime grammar production
Author:             Warren Rainey
FIRST SET:			{ .AND, E }
PRODUCTION:			<logical AND expression’> -> .AND. <relational expression><logical AND expression’> | Ɛ
**********************************************************************/ 
void logical_and_expression_prime(){
	if (lookahead.code == LOG_OP_T){
        switch(lookahead.attribute.log_op){
        case OR:
            return;
        default:
            break;
        }
        match(lookahead.code, lookahead.attribute.arr_op);
        relational_expression();
        logical_and_expression_prime();
        gen_incode("PLATY: Logical AND expression parsed\n");
    }
}

/********************************************************************
Function name:      relational_expression()
Production:         Relational expression grammar production
Author:             Warren Rainey
FIRST SET:			{ AVID_T, FPL_T, INL_T, SVID_T, STR_T  }
PRODUCTION:			<relational_expression>->   
						<primary a_relational_expression><relational expression’>  | <primary a_relational_expression> <relational_expression2’>
**********************************************************************/ 
void relational_expression(){
	switch(lookahead.code){
    case AVID_T:
    case FPL_T:
    case INL_T:
        primary_a_relational_expression();
        relational_expression_prime();
        break;
    case SVID_T:
    case STR_T:
        primary_s_relational_expression();
        relational_expression_doubleprime();
        break;
    default:
        syn_printe();
    }
    gen_incode("PLATY: Relational expression parsed\n");
}
/********************************************************************
Function name:      relational_expression_prime()
Production:         Relational expression prime grammar production
Author:             Warren Rainey
FIRST SET:			{ ==, !=, >, < }
PRODUCTION:			<relational_expression’> -> 
							== <primary a_relational_expression>
							|<> <primary a_relational_expression>                        
							| > <primary a_relational_expression>      
							| <  <primary a_relational_expression> 
**********************************************************************/ 
void relational_expression_prime(){
	 if (lookahead.code == REL_OP_T){
        switch(lookahead.attribute.rel_op){
        case EQ:
        case NE:
        case GT:
        case LT:
            match(lookahead.code, lookahead.attribute.arr_op);
            primary_a_relational_expression();
            break;
        default: 
            syn_printe();
        }
    }else{  
        syn_printe();
	 }
}
/********************************************************************
Function name:      relational_expression_doubleprime()
Production:         Relational expression double prime grammar production
Author:             Warren Rainey
FIRST SET:			{ ==, !=, >, < }
PRODUCTION:			<relational_expression2’> -> 
							== <primary s_relational_expression>
							|<> <primary s_relational_expression>                        
							| > <primary s_relational_expression>      
							| <  <primary s_relational_expression> 
**********************************************************************/ 
void relational_expression_doubleprime(){
	if (lookahead.code == REL_OP_T){
        switch(lookahead.attribute.rel_op){
        case EQ:
        case NE:
        case GT:
        case LT:
            match(lookahead.code, lookahead.attribute.arr_op);
            primary_s_relational_expression();
            break;
        default:
            syn_printe();
        }
    }else{  
        syn_printe();
	}
}
/********************************************************************
Function name:      primary_a_relational_expression()
Production:         Primary a relational expression grammar production
Author:             Warren Rainey
FIRST SET:			{ AVID_T, FPL_T, INL_T }
PRODUCTION:			<primary a_relational expression> ->   AVID_T | FPL_T | INL_T
**********************************************************************/ 
void primary_a_relational_expression(){
	 switch(lookahead.code){
    case AVID_T:
    case FPL_T:
    case INL_T:
        match(lookahead.code, NO_ATTR);
        break;
    default: 
        syn_printe();
    }
    gen_incode("PLATY: Primary a_relational expression parsed\n");
}
/********************************************************************
Function name:      primary_s_relational_expression()
Production:         Primary s relational expression grammar production
Author:             Warren Rainey
FIRST SET:			{ SVID_T, STR_T }
PRODUCTION:			<primary s_relational expression> -> <primary string expression> 
**********************************************************************/ 
void primary_s_relational_expression(){
	primary_string_expression();
    gen_incode("PLATY: Primary s_relational expression parsed\n");
}

