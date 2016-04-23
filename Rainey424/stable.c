/*****************************************************************************************
File name:stable.c 
Compiler: MS Visual Studio 2012
Author: Warren Rainey 040740424
Course: CST 8152 - Compilers,
Assignment: 3 Symbol Table
Date: November 20 2015
Professor: Sv. Ranev 
Purpose: To populate a symbol table
Function list:	st_create()
				st_install()
				st_lookup()
				st_update_type()
				st_update_value()
				st_get_type()
				st_destroy()
				st_print()
				st_setsize()
				st_incoffset()
				st_store()
				st_sort()
*******************************************************************************************/ 
#include "stable.h"
#include <string.h>
#include <stdlib.h>

STD sym_table;

static void st_setsize(void);
static void st_incoffset(void);

/********************************************************************
Function name:      st_create()
Purpose:            Creates a new empty symbol table
Author:             Warren Rainey
History/Versions:   1.0
Called functions:   malloc(), st_setsize(), b_create(), free()
In parameters:      int
Return Value:       STD (Symbol Table Descriptor)
**********************************************************************/  
STD st_create(int st_size){
	STD symTable;
	symTable.plsBD=0;
	sym_table.st_size = 0;

	if(st_size <= 0){
		return symTable;
	}
	symTable.pstvr = (STVR*) malloc(sizeof (STVR)*st_size);
	if(symTable.pstvr == NULL){
		st_setsize();/*This is useless. Setting to 0 inorder to not return garbage*/
		return symTable;
	}
	symTable.plsBD = b_create(INITIAL_CAPACITY,INCREMENT_FACTOR,'a');
	symTable.st_offset = 0;
	if(symTable.plsBD == NULL){
		free(sym_table.pstvr);
		st_setsize();
	}else{
		symTable.st_size = st_size;
	}	
	return symTable;
	
	
}
/********************************************************************
Function name:      st_install()
Purpose:            Install a new entry in the symbol table
Author:             Warren Rainey
History/Versions:   1.0
Called functions:   st_lookup(), b_setmark, b_get_r_flag(), b_addc(), b_get_chloc(), st_incoffset()
In parameters:      STD, char *, char, int
Return Value:       Int
Algorithm:			- Check if symbol table is valid
					- This function installs a new entry (VID record) in the symbol table.  
					First, It calls the st_lookup() function to search for the lexeme (variable name) in the symbol table. 
					- Add new record to symbol table and increment st_offest.
**********************************************************************/  
int st_install(STD sym_table, char *lexeme, char type, int line){
	int location = 0;
	int i = 0;
	int j = 0;
	STVR new_rec;
	
	/*intf("1lexeme: %s\n", lexeme);Test*/
	if(!sym_table.st_size){
		return R_FAIL_2;
	}

	location = st_lookup(sym_table,lexeme);

	if(location >= 0){/*lexeme found in symbol table*/  
		return location;
	}

	if(sym_table.st_offset == sym_table.st_size){
		return TABLE_FULL;
	}

	/*printf("2lexeme:%s\n", lexeme);Test*/
	/*sym_table.pstvr[sym_table.st_offset].plex = b_setmark(sym_table.plsBD,b_size(sym_table.plsBD));/*sym_table.st_offset*/
	new_rec.plex = b_setmark(sym_table.plsBD,b_size(sym_table.plsBD));/*sym_table.st_offset*/
	/*plex = b_setmark(sym_table.plsBD, getOffset(plsBD);
	b_setmark(sym_table.plsBD, 0); /*set to addc_offset*/
		for(i = 0; lexeme[i] != '\0'; i++){ /*add lexeme to lexeme storage*/
			b_addc(sym_table.plsBD, lexeme[i]);
			/*if plsBD has moves as result of realloc of buffer*/
			/*if( b_rflag(sym_table.plsBD)){/*Remove as it may creat p
				
				while(j < sym_table.st_offset){ 
					sym_table.pstvr[j].plex = temp;
					temp += strlen(temp) + 1; /* move temp to end of lexeme in buffer*
					j++;
				}
			}*/
		}
		b_addc(sym_table.plsBD, '\0');
		
	new_rec.status_field = DEFAULT;
	new_rec.o_line = line;
	
	
	/*Finding out if int, string or float*/
	if(type == 'I'){/*Is int*/
		new_rec.i_value.int_val = 0;
		new_rec.status_field |= SET_INTEGER;
	}else if(type == 'S'){/*Is string*/
		new_rec.i_value.str_offset = -1;
		new_rec.status_field |= SET_STRING;
		new_rec.status_field |= CHK_LSB;
	}else{/*Is float*/
		new_rec.i_value.fpl_val = 0.0f;
		new_rec.status_field |= SET_FLOAT;
	}
	/*printf("New plex: %s", new_rec.plex);Test*/
	sym_table.pstvr[sym_table.st_offset] = new_rec; /*add new record to table*/	
	st_incoffset();		
	return sym_table.st_offset;
}
/********************************************************************
Function name:      st_lookup()
Purpose:            Searches if lexeme is in symbol table
Author:             Warren Rainey
History/Versions:   1.0
Called functions:   b_getsize(), strcmp()
In parameters:      STD, char *
Return Value:       Int
**********************************************************************/  
int st_lookup(STD sym_table,char *lexeme){ 
	int i = 0;
	/*Return -2 on invalid table*/
	if(!sym_table.st_size){
		return R_FAIL_2;
	}	
	/*Must be Performed Backwards as dscussed in Lecture, 
	Ignorable helpful tip #301 */
	for(i = sym_table.st_offset-1; i >= 0; i--){
		if(strcmp(sym_table.pstvr[i].plex,lexeme) == 0){
			return i;
		}		
	}
	return R_FAIL_1; 	
}
/********************************************************************
Function name:      update_type()
Purpose:            Upodates the data type indicator
Author:             Warren Rainey
History/Versions:   1.0
Called functions:   No functions called
In parameters:      STD, int, char
Return Value:       Int
**********************************************************************/  
int st_update_type(STD sym_table,int vid_offset,char v_type){

	if ( !sym_table.st_size || vid_offset < 0 || vid_offset >= sym_table.st_offset ){
		return R_FAIL_2;
	}	
		
	if((sym_table.pstvr[vid_offset].status_field & CHK_LSB) == 1){/*Exit the function if LSB is already updated.*/
		return R_FAIL_1;
	}		

		sym_table.pstvr[vid_offset].status_field &= 0xFFF9;/*Setting to 0 before seting to float/ string etc*/
		/*Set bits acoording to type*/
		if (v_type == 'F'){
			sym_table.pstvr[vid_offset].status_field |= SET_FLOAT;
		}else if (v_type == 'I'){/*If integer set 3rd bit to 1*/			
			sym_table.pstvr[vid_offset].status_field |= SET_INTEGER;
		}else{ /*Error check, Invalid parameter.*/
			return R_FAIL_1;
		}		
		sym_table.pstvr[vid_offset].status_field |= SET_LSB;/*Set LSB to 1.*/
		return vid_offset;
	

}
/********************************************************************
Function name:      st_update_value()
Purpose:            Updates the i_value of the variable specified by vid_offset
Author:             Warren Rainey
History/Versions:   1.0
Called functions:   No functions called
In parameters:      STD, Int, InitialValue(Union)
Return Value:       STD (Symbol Table Descriptor)
**********************************************************************/  
int st_update_value(STD sym_table, int vid_offset,InitialValue i_value){
	if( vid_offset < 0 || vid_offset >= sym_table.st_offset ){/*>=*/
		 return R_FAIL_2;
	}	
	if(sym_table.st_size != 0){
		sym_table.pstvr[vid_offset].i_value = i_value; 		
		return vid_offset;
	}
	return -1;
}
/********************************************************************
Function name:      st_get_type()
Purpose:            Returs type of variable specified by vid_offset
Author:             Warren Rainey
History/Versions:   1.0
Called functions:   No functions called
In parameters:      STD, int
Return Value:       char
**********************************************************************/  
char st_get_type (STD sym_table, int vid_offset){
	unsigned short temp;
	if (sym_table.st_size == 0){
		return R_FAIL_1;
	}
	
	if ( !sym_table.st_size || vid_offset < 0 || vid_offset >= sym_table.st_offset ){
		return R_FAIL_1;
	}
	
	temp = sym_table.pstvr[vid_offset].status_field;

	temp &= CHECK_TYPE;
	switch(temp){
	case SET_FLOAT:
		return 'F';
	case SET_INTEGER:
		return 'I';
	case SET_STRING:
		return 'S';
	default: 
		return R_FAIL_1;	
	}
}
/********************************************************************
Function name:      st_destroy()
Purpose:            This function frees the memory occupied by the symbol table dynamic areas and sets st_size to 0. 
Author:             Warren Rainey
History/Versions:   1.0
Called functions:   b_destroy(), free(), st_setsize()
In parameters:      STD
Return Value:       Void
**********************************************************************/  
void st_destroy(STD sym_table){
	if(sym_table.plsBD==NULL && sym_table.pstvr == NULL){
		return;
	}
	if(sym_table.st_size != 0){
		b_destroy(sym_table.plsBD);
		free(sym_table.pstvr);
		st_setsize();/*Set size of symboltable to 0*/
	}
	return;
}
/********************************************************************
Function name:      st_print()
Purpose:            Prints contents of symbol table
Author:             Warren Rainey
History/Versions:   1.0
Called functions:   No functions called
In parameters:      STD
Return Value:       Int
**********************************************************************/  
int st_print(STD sym_table){
	int i =0;
	int entries = 0;
	if(sym_table.plsBD==NULL && sym_table.pstvr == NULL){/*plsBD pointer to bufferdescrip, pstvr pointer to array of stvr Like in Diaggram*/
		return R_FAIL_1;
	}
	if(sym_table.st_size == 0){/*Catch issues before*/
		return R_FAIL_1;
	}
	printf( "\nSymbol Table\n"
			"____________\n\n"
			"Line Number Variable Identifier\n");
	for(i = 0; i < sym_table.st_offset; i++){		
		printf("%2d          %s\n", sym_table.pstvr[i].o_line, sym_table.pstvr[i].plex);/*o_line; line of first occurrence, plex * to lexeme*/
		entries++;
	}
	return entries;
}
/********************************************************************
Function name:      st_setsize()
Purpose:            Sets the size of symbol table to 0
Author:             Warren Rainey
History/Versions:   1.0
Called functions:   No functions called
In parameters:      Void
Return Value:       Void
**********************************************************************/  
static void st_setsize(void){
	sym_table.st_size = 0;
	return;
}
/********************************************************************
Function name:      st_incoffset()
Purpose:            Increments st_offset by 1
Author:             Warren Rainey
History/Versions:   1.0
Called functions:   No functions called
In parameters:      Void
Return Value:       Void
**********************************************************************/  
static void st_incoffset(void){
	++sym_table.st_offset;
	return;
}
/********************************************************************
Function name:      st_store()
Purpose:            Stores symbol table in a file
Author:             Warren Rainey
History/Versions:   1.0
Called functions:   fopen(), fclose()
In parameters:      STD
Return Value:       Int
**********************************************************************/  
int st_store(STD sym_table){
	int i = 0;
	FILE * fptr;
	char * fileName = "$stable.ste";

		if (sym_table.st_size == 0){
		return R_FAIL_1;
	}
	fptr=fopen(fileName,"w+");/*Fopen Unsafe*/
	if(fptr == NULL){
		return R_FAIL_1;
	}
	/*printf("Store1\n");Test*/
	fprintf(fptr, "%d", sym_table.st_size);
	for (i = 0; i < sym_table.st_offset; i++){		
		fprintf(fptr, " %X %d %s %d ", sym_table.pstvr[i].status_field, strlen(sym_table.pstvr[i].plex),sym_table.pstvr[i].plex, sym_table.pstvr[i].o_line);
		switch(st_get_type(sym_table, i)){
		case 'I':/*INT*/
			fprintf(fptr,"%d", 0); 
			break;
		case 'F':/*FLOAT*/
			fprintf(fptr,"%.2f", 0);
			break;
		case 'S':/*STRING*/
			fprintf(fptr,"%d", -1);
			break;
		}		
	}
	/*printf("Store2\n");Test*/
	fclose(fptr);	
	printf("\nSymbol Table stored.\n");
	return sym_table.st_size;
}
/*****************BONUS***************************************************
Function name:      st_sort()
Purpose:            Sorts the symbol table for bonus, if not returns 0;
Author:             Warren Rainey
History/Versions:   1.0
Called functions:   
In parameters:      STD, char
Return Value:       Int
**********************************************************************/  
int st_sort(STD sym_table, char s_order){   

	return 0;
}
