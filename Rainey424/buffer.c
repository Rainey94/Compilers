/*
File name: buffer.c
Compiler: MS Visual Studio 2012
Author: Warren Rainey 040740424
Course: CST 8152 – Compilers, Lab Section: 011
Assignment: 01
Date: 29/09/2015 Actual submission October 6th 2015
Professor: Sv. Ranev
Purpose: Create a buffer that will ultimately be used for the compiler
Function list: b_create (short init_capacity,char inc_factor,char o_mode);
	 b_load (FILE * const fi, Buffer * const pBD);
	 b_size(Buffer * const pBD);
	 b_setmark(Buffer * const pBD, short mark);
	 b_reset(Buffer * const pBD);
	 b_inc_factor (Buffer * const pBD)
	 b_getc(Buffer * const pBD);
	 b_print (Buffer * const pBD);
	 *b_pack(Buffer * const pBD);
	 b_isfull(Buffer * const pBD);
	 b_isempty(Buffer * const pBD);
	 b_addc (pBuffer const pBD, char symbol);
	 b_destroy(Buffer * const pBD);
	 b_capacity(Buffer * const pBD);
	 b_get_r_flag(Buffer * const pBD);
	 b_retract(Buffer * const pBD) ;
	 b_retract_to_mark(Buffer * const pBD);
	 b_getc_offset(Buffer * const pBD) ;
	 b_getmode(Buffer * const pBD);
	 b_getmark(Buffer * const pBD);
	 b_eob(Buffer * const pBD);
*/

#include "buffer.h"


/*
Purpose: This function creates a new buffer in memory
Author: Warren Rainey
History/Versions: 2 29/09/2015
Called functions: none
Parameters: init_capacity: the intial capacity of the buffer, greater than 0, less than shrt_max
			inc_factor: the increment factor of the buffer
			o_mode: the mode for the buffer to be set to
Return value: buffer *
Algorithm: Allocate the memory for the buffer
		   Set the mode and inc factor
		   Set capacity
		   Return a pointer to the newly created buffer
*/
Buffer* b_create (short init_capacity,char inc_factor,char o_mode){
	
	Buffer* nBuffer = (Buffer*) calloc(1, sizeof(Buffer));
	

	/*Check to see it worked*/
	if(nBuffer == NULL){
			/*printf("Allocation error");For Testing, Should not print*/
		return NULL;
	}


		/*nBuffer->cb_head = (char *) malloc(init_capacity*(sizeof(char))); Too Early*/

	/*If capacity is greater then 0 but less then SIZEMax*/
	if(init_capacity >= 0 && init_capacity <= SHRT_MAX){
		
		nBuffer->cb_head = (char *) malloc(init_capacity*(sizeof(char)));
		

		if(o_mode == 'f' || inc_factor == 0){

			nBuffer->mode = 0;
			nBuffer->inc_factor=0;

		}
		else if(o_mode == 'f' && inc_factor != 0){

			nBuffer->mode = 0;
			nBuffer->inc_factor=0;

		}
		else if(o_mode == 'a' && inc_factor >= 1 && inc_factor <= 255){

			nBuffer->mode = 1;
			nBuffer->inc_factor= inc_factor;

		}
		else if(o_mode == 'm' && inc_factor >= 1 && inc_factor <= 100){

			nBuffer->mode = -1;
			nBuffer->inc_factor= inc_factor;

		}
	

		nBuffer->capacity = init_capacity *sizeof(char);/*Platform indepenent*/
	}
	
	return nBuffer;
}
/*
Purpose: Loads in File
Author: Warren Rainey
History/Versions: 2 29/09/2015
Called functions: none
Parameters: fi: the file that is specified in the command line
			pBD: The buffer pointer pBD
Return value: int
*/
int b_load (FILE * const fi, Buffer * const pBD){

	int a=0;
	char temp;

	if(fi == NULL || pBD == NULL){/*checking for NULL*/
		return LOAD_FAIL;
	}
	
	/*Grab the first Char REMEMBER TO CAST*/
	temp = (char)fgetc(fi);
	
	while (feof(fi) == 0) {/*File That is being passed in*/
		if (b_addc(pBD, temp) == NULL){
			return LOAD_FAIL;
		}
		a++;
		/*Get next char*/
		temp = (char) fgetc(fi);
	}
	
	return a;/*If the loading operation is successful, the function must return the number of characters added to the buffer*/
}


/*
Purpose: This function returns the size of the buffer
Author: Warren Rainey
History/Versions: 2 29/09/2015
Called functions: none
Parameters: init_capacity: the intial capacity of the buffer, greater than 0, less than shrt_max
			inc_factor: the increment factor of the buffer
			o_mode: the mode for the buffer to be set to
Return value: pointer->capacity
*/
short b_size(Buffer * const pBD) {
	
	if(pBD == NULL){
		return R_FAIL_1;
	}
	/**if(pBD->addc_offset == 0){
		printf("The buffer is empty\n");
		/*Tester out bahd pBD->addc_offset++;
		return 1;
	}*/
	else{
		/*return pBD->capacity; capactity is in bytes not char*/
		return pBD->addc_offset;
	}

	
}

/*
Purpose: This function sets the mark offset
Author: Warren Rainey
History/Versions: 2 29/09/2015
Called functions: none
Parameters: init_capacity: the intial capacity of the buffer, greater than 0, less than shrt_max
			inc_factor: the increment factor of the buffer
			o_mode: the mode for the buffer to be set to
Return value: the setmark
*/
char * b_setmark(Buffer * const pBD, short mark) {

	if(pBD == NULL){
		return NULL;
	}
	if((0>mark || mark>pBD->addc_offset)){/*inclusivly*/
		/*printf("Mark is out of bounds");*/
		return NULL;/*Mark should be able to be 0*/
	}
	if(pBD->addc_offset==0){
		pBD->mark_offset=0;
	}
	else{
		pBD->mark_offset=mark;
	}
	return pBD->cb_head + pBD->mark_offset;
	/*return (char *)mark;/*Must Cast*/


}

/*
Purpose: This function resets the add offset get offset and mark offset
Author: Warren Rainey
History/Versions: 2 29/09/2015
Called functions: none
Parameters: init_capacity: the intial capacity of the buffer, greater than 0, less than shrt_max
			inc_factor: the increment factor of the buffer
			o_mode: the mode for the buffer to be set to
Return value: 1
*/
int b_reset(Buffer * const pBD){

	if(pBD == NULL){
		return R_FAIL_1;
	}
	/*Set everything to 0*/

	pBD->addc_offset=0;
	pBD->getc_offset=0;
	pBD->mark_offset=0;
	
	return 1;


}

/*
Purpose: This function gets the char for the buffer
Author: Warren Rainey
History/Versions: 2 29/09/2015
Called functions: none
Parameters: init_capacity: the intial capacity of the buffer, greater than 0, less than shrt_max
			inc_factor: the increment factor of the buffer
			o_mode: the mode for the buffer to be set to
Return value: c
*/
char b_getc(Buffer * const pBD) {
	
	if(pBD==NULL){
	return R_FAIL_1;
	}

	if(pBD->getc_offset == pBD->addc_offset){
		pBD->eob=1;
		return R_FAIL_1;
	}
	else{
		pBD->eob=0;
	}
	/*if(pBD->ca_head[pBD->getc_offset]==EOF){
		return 0;
	}

	else{*/
		return pBD->cb_head[pBD->getc_offset++];
	//}	
}

/*
Purpose: This function prints out
Author: Warren Rainey
History/Versions: 2 29/09/2015
Called functions: none
Parameters: init_capacity: the intial capacity of the buffer, greater than 0, less than shrt_max
			inc_factor: the increment factor of the buffer
			o_mode: the mode for the buffer to be set to
Return value: int counter
*/
int b_print (Buffer * const pBD) {
	char temp;
	int counter =0;

	if(pBD==NULL){
	return R_FAIL_1;
	}
	pBD->eob=0;

	if(pBD->addc_offset == 0){
		printf("The buffer is empty.\n"); //Uncomment for sending in
		//printf("The buffer is empty @Print\n"); Testing
		return counter;
	}
	pBD->getc_offset=0;

	while(!b_eob(pBD)){
		temp= b_getc(pBD);/*The function prints the content calling b_getc() in a loop and using b_eob() to detect the end of the buffer content*/
		if(b_eob(pBD) !=1){
		printf("%c",temp);
		counter++;
		}
		/*pBD->getc_offset++;*/
		
	}
	pBD->getc_offset=0;
	printf("\n");



		return counter-1;

}

/*
Purpose: This function sets the current size and adds one
Author: Warren Rainey
History/Versions: 2 29/09/2015
Called functions: none
Parameters: init_capacity: the intial capacity of the buffer, greater than 0, less than shrt_max
			inc_factor: the increment factor of the buffer
			o_mode: the mode for the buffer to be set to
Return value: pBD pointer
*/
Buffer *b_pack(Buffer * const pBD) {

	char *temp;
	int newCap; /*This was added after the Buffer corrections as pBD->addc_offset + 1 */

	if(pBD == NULL || pBD->cb_head== NULL){
		return NULL;/* The function must return NULL if for some reason it cannot to perform the required operation.*/
	}

	newCap = ((pBD->addc_offset + 1) * sizeof(char));

	if(pBD->addc_offset == SHRT_MAX){
		return NULL;
	}


	temp=(char*) realloc(pBD->cb_head,newCap);/*Need to be fixed*/
	if(temp==NULL){
		return NULL;
	}

	if(temp != pBD->cb_head){
		pBD->r_flag=SET_R_FLAG;/*Setting flag*/
		pBD->cb_head=temp;
	}
	
	pBD->capacity=newCap;/*The new capacity is the current size plus a space for one more char Edit was: pBD->addc_offset + 1*/
	pBD->eob=0;
	return pBD;
}

/*
Purpose: This function checks if the buffer is full
Author: Warren Rainey
History/Versions: 2 29/09/2015
Called functions: none
Parameters: init_capacity: the intial capacity of the buffer, greater than 0, less than shrt_max
			inc_factor: the increment factor of the buffer
			o_mode: the mode for the buffer to be set to
Return value: -1,0,1
*/
int b_isfull(Buffer * const pBD){

	if(pBD == NULL){
		return R_FAIL_1;
	}

	if(pBD->addc_offset ==  pBD->capacity){
		return 1;
	}
	/*no need for else*/
		return 0;
	
}

/*
Purpose: This function checks if the buffer is empty
Author: Warren Rainey
History/Versions: 2 29/09/2015
Called functions: none
Parameters: init_capacity: the intial capacity of the buffer, greater than 0, less than shrt_max
			inc_factor: the increment factor of the buffer
			o_mode: the mode for the buffer to be set to
Return value: -1,0,1 reffering to buffer emptyness 
*/
int b_isempty(Buffer * const pBD) {

	if(pBD == NULL){
		return R_FAIL_1;
	}

	//if(pBD->addc_offset == pBD->capacity*sizeof(char)){ OG code need to be fixed
	if(pBD->addc_offset == 0){
		return 1;
	}
	else{
		return 0;
	}


}

/*
Purpose: This function adds the next char to the array and takes care of moving to the next
Author: Warren Rainey
History/Versions: 2 29/09/2015
Called functions: none
Parameters: init_capacity: the intial capacity of the buffer, greater than 0, less than shrt_max
			inc_factor: the increment factor of the buffer
			o_mode: the mode for the buffer to be set to
Return value: pBD *
*/
pBuffer b_addc (pBuffer const pBD, char symbol){
	char *tempP;
	short available = 0;
	short tempInc; /*was char but that was my issue with the buffer! overflow was occuring*/
	int newCap;

	if(pBD == NULL){
		return NULL;
	}

	
	/*= (SHRT_MAX - pBD->capacity)*/

	if(b_size(pBD) !=  pBD->capacity){ /*If there is space*/
			pBD->addc_offset;
			pBD->cb_head[pBD->addc_offset++]=symbol;/*Add Symbol to the Buffer*/
			return pBD;
	}/* end of if there is space*/

	switch(pBD->mode){

		case 0:
			return NULL;
		break;

		case 1:
			pBD->r_flag=0;/*Reset R_Flag*/
			newCap = pBD->capacity+pBD->inc_factor;
			if(newCap <= SHRT_MAX){
				tempP =(char*) realloc(pBD->cb_head, newCap);/*Set*/
				pBD->capacity = (short)newCap;/*cast to short*/
				if(tempP == NULL){/*checking for error with realloc as it returns NULL if there is an issue*/
					return NULL;
				}
				if(pBD->cb_head != tempP){
					pBD->cb_head = tempP; /*set the head to the new locaton of realloc'd memory*/
					pBD->r_flag=SET_R_FLAG;/*realocation flag*/
				}
			}
			else{
				return NULL;
			}
			pBD->cb_head[pBD->addc_offset]=symbol;/*Add Symbol to the Buffer*/
			pBD->addc_offset++;
			//return pBD;
		break;


		case -1:
			/*available space = maximum buffer size – current capacity 
			new increment = available space * inc_factor / 100
			new capacity = current capacity + new increment */
			pBD->r_flag=0;/*Reset R_Flag*/
			available=SHRT_MAX-(pBD->capacity);
			tempInc = (short)(available * (pBD->inc_factor/100.0f));/*result will always be zero, MUST CAST supdate:now zero*/
			/*newCap = pBD->capacity+pBD->inc_factor; The original */
			newCap = pBD->capacity+tempInc;
			if(newCap <= SHRT_MAX){
				tempP =(char*) realloc(pBD->cb_head, newCap);/*Set*/
				pBD->capacity = (short)newCap;/*cast to short*/
				if(tempP == NULL){/*checking for error with realloc as it returns NULL if there is an issue*/
					return NULL;
				}
				if(pBD->cb_head != tempP){
					pBD->cb_head = tempP; /*set the head to the new locaton of realloc'd memory*/
					pBD->r_flag=SET_R_FLAG;/*realocation flag*/
				}
			}
			else{
				return NULL;
			}
			pBD->cb_head[pBD->addc_offset]=symbol;/*Add Symbol to the Buffer*/
			pBD->addc_offset++;
			//return pBD;
		break;


	}/*end of switch*/

	return pBD;

		
}

/*
Purpose: This function destroys the pBD and head pointer
Author: Warren Rainey
History/Versions: 2 29/09/2015
Called functions: none
Parameters: init_capacity: the intial capacity of the buffer, greater than 0, less than shrt_max
			inc_factor: the increment factor of the buffer
			o_mode: the mode for the buffer to be set to
Return value: 
*/
void b_destroy(Buffer * const pBD){
	/**The function de-allocates (frees) the memory occupied by the character buffer and the Buffer
structure (buffer descriptor). The function should not crash.*/
		free(pBD->cb_head);
		free(pBD);
}

/*
Purpose: This function gets the capacity of the buffer
Author: Warren Rainey
History/Versions: 2 29/09/2015
Called functions: none
Parameters: Buffer * const pBD: Buffer pointer
Return value: pBD->capacity 
*/
short b_capacity(Buffer * const pBD){
	if(pBD->capacity <0){
		return R_FAIL_1;
	}
	else{
		return pBD->capacity;
	}

}

/*
Purpose: This function returns the r_flag
Author: Warren Rainey
History/Versions: 2 29/09/2015
Called functions: none
Parameters: Buffer * const pBD: Buffer pointer
Return value: flag value
*/
char b_rflag(Buffer * const pBD){

	if(pBD==NULL){
		return R_FAIL_1;
	}
	else{
		return pBD->r_flag;
	}
}

/*
Purpose: This function retracts the offset
Author: Warren Rainey
History/Versions: 2 29/09/2015
Called functions: none
Parameters: Buffer * const pBD: Buffer pointer
Return value: getc_offset
*/
short b_retract(Buffer * const pBD) {
	
	if(pBD == NULL){
		return R_FAIL_1;
	}
	
	pBD->getc_offset--;

	return pBD->getc_offset;
}

/*
Purpose: This function retracts the mark
Author: Warren Rainey
History/Versions: 2 29/09/2015
Called functions: none
Parameters: Buffer * const pBD: Buffer pointer
Return value: The offset for the buffer
*/
short b_retract_to_mark(Buffer * const pBD) {
	if(pBD->cb_head == NULL){
		return R_FAIL_1;
	}
	else{
		pBD->getc_offset = pBD->mark_offset;
	return pBD->getc_offset;
	}
}
 

/*
Purpose: This function returns the mode
Author: Warren Rainey
History/Versions: 2 29/09/2015
Called functions: none
Parameters: Buffer * const pBD: Buffer pointer
Return value: The mode for the buffer
*/
int b_mode(Buffer * const pBD){
	if(pBD == NULL){
		return R_FAIL_1;
	}
	return pBD->mode;
}


/*
Purpose: This function returns the inc_factor
Author: Warren Rainey
History/Versions: 2 29/09/2015
Called functions: none
Parameters: Buffer * const pBD: Buffer pointer
Return value: The inc_factor for the buffer
*/
size_t b_inc_factor (Buffer * const pBD){

	if(pBD == NULL){
		/*return R_FAIL_1;*/
		return (unsigned char)256;
	}

	return (unsigned char)pBD->inc_factor;
}

/*
Purpose: This function returns the mark offset
Author: Warren Rainey
History/Versions: 2 29/09/2015
Called functions: none
Parameters: Buffer * const pBD: Buffer pointer
Return value: The mark for the buffer
*/
short b_mark(Buffer * const pBD){
	if(pBD == NULL){
		return R_FAIL_1;
	}
	return pBD->mark_offset;
}

/*
Purpose: This function checks for the end of the buffer
Author: Warren Rainey
History/Versions: 2 29/09/2015
Called functions: none
Parameters: Buffer * const pBD: Buffer pointer
Return value: The end of the buffer inorder to keep track of the end of the buffer
*/
int b_eob(Buffer * const pBD){
	if(pBD == NULL){
		return R_FAIL_1;
	}
/*	else if(pBD->capacity==pBD->getc_offset){
		pBD->eob = 1;
	}
	else{
		pBD->eob=0;
	}*/

	return pBD->eob;

}

/*
Purpose: This function returns the c_offset
Author: Warren Rainey
History/Versions: 2 29/09/2015
Called functions: none
Parameters: Buffer * const pBD: Buffer pointer
Return value: The c_offset for the buffer
*/
short b_getc_offset (Buffer * const pBD){

	if(pBD != NULL){
		return pBD->getc_offset;
	}
	else
		return R_FAIL_1;

}

