/******************************************************************************
 * $Id: dbfdump.c,v 1.9 2002/01/15 14:36:07 warmerda Exp $
 * $Id: dbfdump.c,v 1.5 1999/11/05 14:12:04 warmerda Exp $
 *
 * Project:  Shapelib
 * Purpose:  Sample application for dumping .dbf files to the terminal.
 * Author:   Frank Warmerdam, warmerdam@pobox.com
 *
 ******************************************************************************
 * Copyright (c) 1999, Frank Warmerdam
 *
 * This software is available under the following "MIT Style" license,
 * or at the option of the licensee under the LGPL (see LICENSE.LGPL).  This
 * option is discussed in more detail in shapelib.html.
 *
 * --
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 ******************************************************************************
 *
 * $Log: dbfdump.c,v $
 * Revision 1.9  2002/01/15 14:36:07  warmerda
 * updated email address
 *
 * Revision 1.8  2001/05/31 18:15:40  warmerda
 * Added support for NULL fields in DBF files
 *
 * Revision 1.7  2000/09/20 13:13:55  warmerda
 * added break after default:
 *
 * Revision 1.6  2000/07/07 13:39:45  warmerda
 * removed unused variables, and added system include files
 *
 * $Log: dbfdump.c,v $
 * Revision 1.5  1999/11/05 14:12:04  warmerda
 * updated license terms
 *
 * Revision 1.4  1998/12/31 15:30:13  warmerda
 * Added -m, -r, and -h commandline options.
 *
 * Revision 1.3  1995/10/21 03:15:01  warmerda
 * Changed to use binary file access.
 *
 * Revision 1.2  1995/08/04  03:16:22  warmerda
 * Added header.
 *
 */

#include "maptools.h"

#include <R_ext/PrtUtil.h>



SEXP Rdbfread(SEXP);


static char* nameMangle(char *dbfname, int len){
    int i, flag=0;
    for(i=0;i<len;i++) {
      if (dbfname[i]=='_') {
	dbfname[i]='.';
	flag++;
      }
    }
    if (flag > 0) Rprintf("DBF field with \"_\" changed to %s\n", dbfname);
    return dbfname;
}

SEXP Rdbfread(SEXP dbfnm)
{
    DBFHandle	hDBF;
    int		 i, iRecord, nflds, nrecs, nRvar;
    char	labelbuff[81], *pszFilename = NULL;
    int		nWidth, nDecimals;
    char	szTitle[12], szBuff[50];
    DBFFieldType	eType;
    SEXP       df, tmp, varlabels, types, row_names;

/* -------------------------------------------------------------------- */
/*      Handle arguments.                                               */
/* -------------------------------------------------------------------- */

    pszFilename = CHAR(STRING_ELT(dbfnm, 0)); 


/* -------------------------------------------------------------------- */
/*      Open the file.                                                  */
/* -------------------------------------------------------------------- */
    hDBF = DBFOpen(pszFilename, "rb" );
    if( hDBF == NULL )
    {
/*	REprintf( "DBFOpen(%s,\"r\") failed.\n",
		  pszFilename );
	exit(1); */
	error("unable to open DBF file");
    }
    
/* -------------------------------------------------------------------- */
/*	If there is no data in this file let the user know.		*/
/* -------------------------------------------------------------------- */
    if( DBFGetFieldCount(hDBF) == 0 )
    {
/*	REprintf( "There are no fields in this table!\n" );
	exit(2); */
    	DBFClose( hDBF );
	error("No fields in DBF table");
    }


    nRvar=0;
    nflds=DBFGetFieldCount(hDBF);
    nrecs=DBFGetRecordCount(hDBF);
    PROTECT(types=allocVector(INTSXP,nflds));
    for( i = 0; i < nflds; i++ )
    {
	eType = DBFGetFieldInfo( hDBF, i, szTitle, &nWidth, &nDecimals );

	if( eType == FTString ){
	  INTEGER(types)[i]= 1;
  	  nRvar++; 
	}
	else if( eType == FTInteger ){
	  INTEGER(types)[i]= 2;
	  nRvar++;
	}
	else if( eType == FTDouble )
	{
	  INTEGER(types)[i]= 3;
	  nRvar++;
        }
	else if( eType == FTInvalid )
	{
	  INTEGER(types)[i]= 0;
      	}
/*  	Rprintf( "Field %d: Title=`%s', Width=%d, Decimals=%d\n", */
/*  		   i, nameMangle(szTitle,12), nWidth, nDecimals ); */
    }
/*      Rprintf("%d fields, %d recs \n",nflds,nrecs); */
    PROTECT(df=allocVector(VECSXP, nRvar));
    PROTECT(varlabels=allocVector(STRSXP, nRvar)); 
    for(i=0, nRvar=0; i<nflds; i++){
      eType = DBFGetFieldInfo( hDBF, i, szTitle, &nWidth, &nDecimals );
      if(INTEGER(types)[i]==0) continue;
      if(INTEGER(types)[i]==1) {
  	SET_VECTOR_ELT(df, nRvar, allocVector(STRSXP,nrecs)); 
    	SET_STRING_ELT(varlabels, nRvar, mkChar(nameMangle(szTitle, 12)));  
  	nRvar++; 
      }
      if(INTEGER(types)[i]==2) {
	SET_VECTOR_ELT(df, nRvar, allocVector(INTSXP,nrecs));
    	SET_STRING_ELT(varlabels, nRvar, mkChar(nameMangle(szTitle, 12)));  
	nRvar++;
      }
      if(INTEGER(types)[i]==3) {
	SET_VECTOR_ELT(df, nRvar, allocVector(REALSXP,nrecs));
    	SET_STRING_ELT(varlabels, nRvar, mkChar(nameMangle(szTitle, 12)));  
	nRvar++;
      }
    }


    
    for(iRecord=0; iRecord<nrecs; iRecord++){
      nRvar=0;
      for(i=0; i<nflds; i++){
	if(INTEGER(types)[i]!=0) {
	  if(INTEGER(types)[i]==1) {
	    if( DBFIsAttributeNULL( hDBF, iRecord, i )) {
	      SET_STRING_ELT(VECTOR_ELT(df, nRvar), iRecord, NA_STRING);
	    } else {
    	      strcpy(szBuff, DBFReadStringAttribute( hDBF, iRecord, i));
/*              Rprintf("  %s  ",szBuff); */
  	      SET_STRING_ELT(VECTOR_ELT(df, nRvar), iRecord,  
  	        mkChar(szBuff)); 
	    }
  	    nRvar++; 
	  }
	  else if(INTEGER(types)[i]==2) {
	    if( DBFIsAttributeNULL( hDBF, iRecord, i )) {
	      INTEGER(VECTOR_ELT(df, nRvar))[iRecord] = NA_INTEGER;
	    } else {
	      INTEGER(VECTOR_ELT(df, nRvar))[iRecord]= 
	        DBFReadIntegerAttribute( hDBF, iRecord, i );
	    }
	    nRvar++;
	  }
	  else if(INTEGER(types)[i]==3) {
	    if( DBFIsAttributeNULL( hDBF, iRecord, i )) {
	      REAL(VECTOR_ELT(df, nRvar))[iRecord] = NA_REAL;
	    } else {
	      REAL(VECTOR_ELT(df, nRvar))[iRecord]= 
	        DBFReadDoubleAttribute( hDBF, iRecord, i );
	    }
	    nRvar++;
	  }
	}	 
      }
/*       Rprintf("\n");  */   
    }
    DBFClose( hDBF );
    PROTECT(tmp = mkString("data.frame"));
    setAttrib(df, R_ClassSymbol, tmp);
    setAttrib(df, R_NamesSymbol, varlabels); 
    PROTECT(row_names = allocVector(STRSXP, nrecs));
    for (i=0; i<nrecs; i++) {
        sprintf(labelbuff, "%d", i+1);
        SET_STRING_ELT(row_names,i, mkChar(labelbuff));
    }
    setAttrib(df, R_RowNamesSymbol, row_names);

    UNPROTECT(5);
    return(df);
}

