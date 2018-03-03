   /*******************************************************/
   /*      "C" Language Integrated Production System      */
   /*                                                     */
   /*               CLIPS Version 6.30  08/16/14          */
   /*                                                     */
   /*                USER FUNCTIONS MODULE                */
   /*******************************************************/

/*************************************************************/
/* Purpose:                                                  */
/*                                                           */
/* Principal Programmer(s):                                  */
/*      Gary D. Riley                                        */
/*                                                           */
/* Contributing Programmer(s):                               */
/*                                                           */
/* Revision History:                                         */
/*                                                           */
/*      6.24: Created file to seperate UserFunctions and     */
/*            EnvUserFunctions from main.c.                  */
/*                                                           */
/*      6.30: Removed conditional code for unsupported       */
/*            compilers/operating systems (IBM_MCW,          */
/*            MAC_MCW, and IBM_TBC).                         */
/*                                                           */
/*************************************************************/

/***************************************************************************/
/*                                                                         */
/* Permission is hereby granted, free of charge, to any person obtaining   */
/* a copy of this software and associated documentation files (the         */
/* "Software"), to deal in the Software without restriction, including     */
/* without limitation the rights to use, copy, modify, merge, publish,     */
/* distribute, and/or sell copies of the Software, and to permit persons   */
/* to whom the Software is furnished to do so.                             */
/*                                                                         */
/* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS */
/* OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF              */
/* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT   */
/* OF THIRD PARTY RIGHTS. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY  */
/* CLAIM, OR ANY SPECIAL INDIRECT OR CONSEQUENTIAL DAMAGES, OR ANY DAMAGES */
/* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN   */
/* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF */
/* OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.          */
/*                                                                         */
/***************************************************************************/

#include "clips.h"
#include "string.h"

void UserFunctions(void);
void EnvUserFunctions(void *);

/*********************************************************/
/* UserFunctions: Informs the expert system environment  */
/*   of any user defined functions. In the default case, */
/*   there are no user defined functions. To define      */
/*   functions, either this function must be replaced by */
/*   a function with the same name within this file, or  */
/*   this function can be deleted from this file and     */
/*   included in another file.                           */
/*********************************************************/
void UserFunctions()
  {
   // Use of UserFunctions is deprecated.
   // Use EnvUserFunctions instead.
  }
  
/***********************************************************/
/* EnvUserFunctions: Informs the expert system environment */
/*   of any user defined functions. In the default case,   */
/*   there are no user defined functions. To define        */
/*   functions, either this function must be replaced by   */
/*   a function with the same name within this file, or    */
/*   this function can be deleted from this file and       */
/*   included in another file.                             */
/***********************************************************/

double certezza (void *environment){
	double mb,md;
	mb = EnvRtnDouble(environment,1);
	md = EnvRtnDouble(environment,2);

	return (mb - md);
}

double incr_mb (void *environment){
	double mb;
	double x;
	mb = EnvRtnDouble(environment,1);
    x = EnvRtnDouble(environment,2);

	return ((mb * (1-x))+x);
}

double incr_md (void *environment){
	double md;
	double x;
	md = EnvRtnDouble(environment,1);
	x = EnvRtnDouble(environment,2);

	return ((md * (1-x))+x);
}

double imc (void *environment){
	double peso,altezza;
	peso = EnvRtnDouble(environment,1);
    altezza = EnvRtnDouble(environment,2);

	return ((peso) /(altezza*altezza));
}

void creacartella (void *environment){
	struct paziente{
		char nome[100];
		char cognome[100];
		char genere[10];
		int eta;
		int frequenza;
		double peso;
		double altezza;
		char patologia[100];
	};

	struct paziente corridore;
	strcpy(corridore.nome, EnvRtnLexeme(environment,1));
	strcpy(corridore.cognome, EnvRtnLexeme(environment,2));
	strcpy(corridore.genere, EnvRtnLexeme(environment,3));
	corridore.eta = EnvRtnLong(environment,4);
	corridore.frequenza = EnvRtnLong(environment,5);
	corridore.peso = EnvRtnDouble(environment,6);
	corridore.altezza = EnvRtnDouble(environment,7);
	strcpy(corridore.patologia, EnvRtnLexeme(environment,8));

	strcat(corridore.nome,corridore.cognome);
	char path[100];
	char temp1[50];
	char temp2[50];
	char temp3[50];
	char temp4[50];

	strcpy(path,"C:/Users/Costa/ICSE-C/clips/clp/");
	strcat(path,corridore.nome);
	strcat(path,".clp");

	FILE *fp;
	fp = fopen(path,"w");
	if (fp == NULL) printf("Errore nell'apertura della cartella \n");
	char cartella[1000];
	strcpy(cartella,"(defrule cartella-clinica \n (declare (salience 10000)) \n => \n (assert (genere ");
	strcat(cartella,corridore.genere);
	strcat(cartella," )) \n (assert (eta  ");
	itoa(corridore.eta,temp1,10);
	strcat(cartella,temp1);
	strcat(cartella," )) \n (assert (peso ");
	sprintf(temp2,"%lf",corridore.peso);
	strcat(cartella,temp2);
	strcat(cartella," )) \n (assert (altezza ");
	sprintf(temp3,"%lf",corridore.altezza);
	strcat(cartella,temp3);
	strcat(cartella," )) \n (assert (frequenza-riposo ");
	strcat(cartella,itoa(corridore.frequenza,temp4,10));
	strcat(cartella," )) \n (assert (patologia ");
	strcat(cartella,corridore.patologia);
	strcat(cartella," )))");
	fputs(cartella,fp);
	fclose(fp);
}

void EnvUserFunctions(
  void *environment)
  {
#if MAC_XCD
#pragma unused(environment)
#endif

	extern double certezza (void *);
	EnvDefineFunction2(environment,"certezza",'d',PTIF certezza,"certezza","22n");

	extern double incr_mb (void *);
    EnvDefineFunction2(environment,"incr_mb",'d',PTIF incr_mb,"incr_mb","22n");

    extern double incr_md (void *);
    EnvDefineFunction2(environment,"incr_md",'d',PTIF incr_md,"incr_md","22n");

	extern double imc (void *);
	EnvDefineFunction2(environment,"imc",'d',PTIF imc,"imc","22n");

	extern void creacartella (void *);
	EnvDefineFunction2(environment,"creacartella",'v',PTIEF creacartella,"creacartella","88n");

  }

