/*
File name: table.h
Compiler: MS Visual Studio 2012
Author: Warren Rainey 040740424
Course: CST 8152 – Compilers
Assignment: 02
Date: 27 October 2015
Professor: Sv. Ranev
Purpose: to contain the tables and definitions used by the scanner
*/

#ifndef  TABLE_H_
#define  TABLE_H_ 

#ifndef BUFFER_H_
#include "buffer.h"
#endif

#ifndef NULL
#include <_null.h> /* NULL pointer constant is defined there */
#endif

/*   Source end-of-file (SEOF) sentinel symbol
 *    '\0' or only one of the folowing constants: 255, 0xFF , EOF
 */
#define SEOF 255

#define KW_L 8

/*  Single-lexeme tokens processed separately one by one
 *  in the token-driven part of the scanner
 *  '=' , ' ' , '(' , ')' , '{' , '}' , == , <> , '>' , '<' ,
 *       space
 *  !<comment , ',' , '"' , ';' , '-' , '+' , '*' , '/', << ,
 *  .AND., .OR. , SEOF, 'wrong symbol',
 */
 
#define ES 12 /* Error state */
#define IS -3    /* Inavalid state */

/* State transition table definition */ 

#define TABLE_COLUMNS 7
/*transition table - type of states defined in separate table */
int  st_table[ ][TABLE_COLUMNS] = {
/* State 0 */  {1 ,6 ,4 ,4 ,ES,ES,ES},
/* State 1 */  {1 ,1 ,1 ,1 ,2 ,3 ,2 },
/* State 2 */  {IS,IS,IS,IS,IS,IS,IS}, 
/* State 3 */  {IS,IS,IS,IS,IS,IS,IS},
/* State 4 */  {5 ,4 ,4 ,4 ,7 ,5 ,5 },
/* State 5 */  {IS,IS,IS,IS,IS,IS,IS},
/* State 6 */  {ES,ES,ES,9 ,7 ,ES,5 },
/* State 7 */  {8 ,7 ,7 ,7 ,8 ,8 ,8 },
/* State 8 */  {IS,IS,IS,IS,IS,IS,IS},
/* State 9 */  {IS,9 ,ES,9 ,ES,10,10},
/* State 10 */ {IS,10,IS,10,IS,8 ,8 },
/* State 11 */ {IS,IS,IS,IS,IS,IS,IS},
/* State 12 */ {IS,IS,IS,IS,IS,IS,IS},
/* State 13 */ {IS,IS,IS,IS,IS,IS,IS}
};
 
/* Accepting state table definition */
#define ASWR    2  /* accepting state with retract */
#define ASNR    3  /* accepting state with no retract */
#define NOAS    4  /* not accepting state */

int as_table[ ] = { NOAS, NOAS, ASWR, ASNR, NOAS, ASWR, NOAS, NOAS, ASWR, NOAS, ASWR, ASWR, ASNR, NOAS };
/*					0	  1		2	  3		4	  5		6	  7		8	  9		10	  11	12	  13*/
/* Accepting action function declarations */

/* FOR EACH OF YOUR ACCEPTING STATES YOU MUST PROVIDE
ONE FUNCTION PROTOTYPE. THEY ALL RETURN Token AND TAKE
ONE ARGUMENT: A string REPRESENTING A TOKEN LEXEME. */ 

Token aa_func02(char *lexeme); 
Token aa_func03(char *lexeme); 
Token aa_func05(char *lexeme); 
Token aa_func08(char *lexeme); 
Token aa_func10(char *lexeme); 
Token aa_func12(char *lexeme); 
Token aa_func13(char *lexeme); 


/* defining a new type: pointer to function (of one char * argument) 
   returning Token
*/  

typedef Token (*PTR_AAF)(char *lexeme);


/* Accepting function (action) callback table (array) definition */
/* If you do not want to use the typedef, the equvalent declaration is:
 * Token (*aa_table[])(char lexeme[]) = {
 */

PTR_AAF aa_table[ ] ={
	NULL,
	NULL,
	aa_func02,
	aa_func03,
	NULL,
	aa_func05,
	NULL,
	NULL,
	aa_func08,
	NULL,
	aa_func10,
	NULL,
	aa_func12,
	NULL
};

/* Keyword lookup table (.AND. and .OR. are not keywords) */

#define KWT_SIZE  8

char * kw_table []= {
                      "ELSE",
                      "IF",
                      "INPUT",
                      "OUTPUT",
                      "PLATYPUS",
                      "REPEAT",
                      "THEN",
                      "USING"   
                     };

#endif
                     
