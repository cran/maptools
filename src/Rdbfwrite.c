/******************************************************************************
 * $Id: dbfdump.c,v 1.5 1999/11/05 14:12:04 warmerda Exp $
 *
 * Project:  Shapelib
 * Purpose:  Sample application for dumping .dbf files to the terminal.
 * Author:   Frank Warmerdam, warmerda@home.com
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
 */

#include "maptools.h"

#include <R_ext/PrtUtil.h>
#include <Rmath.h>


DBFHandle Rdbfwrite(DBFHandle, SEXP, int);

static char* nameMangleOut(char *dbfFldname, int len){
    int i;
    for(i=0;i<len;i++){
      if (dbfFldname[i]=='.') dbfFldname[i]='_';
    }
    
    return dbfFldname;
}

static int findMaxString(SEXP strvec){
  int maxstrlen, i;
  maxstrlen=0;
  for (i=0; i<length(strvec);i++){
    if(length(STRING_ELT(strvec,i)) > maxstrlen) 
      maxstrlen=length(STRING_ELT(strvec,i));
  }
  return maxstrlen;
}
static int findMaxInt(SEXP intvec){
  int maxint, i;
  maxint=0;
  for (i=0; i<length(intvec);i++){
    if(INTEGER(intvec)[i] > maxint) 
      maxint=INTEGER(intvec)[i];
  }
  return maxint;
}
static double findMaxReal(SEXP realvec){
  int  i;
  double maxreal;
  maxreal=0.0;
  for (i=0; i<length(realvec);i++){
    if(REAL(realvec)[i] > maxreal) 
      maxreal=REAL(realvec)[i];
  }
  return maxreal;
}



SEXP DoWritedbf(SEXP call)
{ 
    SEXP fname,  df, precision;
    DBFHandle hDBF;

    if (!isValidString(fname = CADR(call)))
	error ("first argument must be a file name\n");

    hDBF = DBFCreate(R_ExpandFileName(CHAR(STRING_ELT(fname,0))));
    if (hDBF==NULL)
	error("unable to open file");
 
    df=CADDR(call);
    if (!inherits(df,"data.frame"))
        error("data to be saved must be in a data frame.");
    
    precision=CADDDR(call);
    Rdbfwrite(hDBF,df,INTEGER(precision)[0]);
    DBFClose( hDBF ); 
    return R_NilValue;
}


DBFHandle Rdbfwrite(DBFHandle hDBF, SEXP df, int pr)
{
    
    int		i, iRecord, nflds, nrecs;
    int		nWidth, nDecimals=0, maxi;
    double      maxr;
    char	szTitle[12];
    SEXP        names;

    nflds=length(df);
    nrecs=length(VECTOR_ELT(df,0));
    PROTECT(names=getAttrib(df,R_NamesSymbol));
    for( i = 0; i < nflds; i++ ){

      strncpy(szTitle,CHAR(STRING_ELT(names,i)),11);
      switch(TYPEOF(VECTOR_ELT(df,i))){
        case LGLSXP:
        case INTSXP:
	  maxi = findMaxInt(VECTOR_ELT(df,i));
	  if(maxi==0) maxi=1;
	  nWidth=ceil(log1p((double)maxi));
	  if(strlen(szTitle) > nWidth) nWidth = strlen(szTitle);
	  DBFAddField(hDBF,nameMangleOut(szTitle,11),FTInteger,nWidth,0);
	  break;
	case REALSXP:
	  maxr = findMaxReal(VECTOR_ELT(df,i));
	  if(maxr==0.0) maxr=1.0;
	  nWidth=ceil(log1p(maxr));
	  if(strlen(szTitle) > nWidth) nWidth = strlen(szTitle);
	  if(pr > -1) nDecimals = pr;
	  DBFAddField(hDBF,nameMangleOut(szTitle,11),FTDouble,nWidth,
		      nDecimals);
	  break;
        case STRSXP:
	  nWidth = findMaxString(VECTOR_ELT(df,i));
	  if(strlen(szTitle) > nWidth) nWidth = strlen(szTitle);
	  DBFAddField(hDBF,nameMangleOut(szTitle,11),FTString,nWidth,0);
	  break;
	default:
	  error("Unknown data type");
	  break;
      }
    }

    UNPROTECT(1);
    for(iRecord=0;iRecord<nrecs;iRecord++){
      for(i=0;i<nflds;i++){
	switch(TYPEOF(VECTOR_ELT(df,i))){
        case INTSXP:
	  DBFWriteIntegerAttribute(hDBF,iRecord,i,
				   INTEGER(VECTOR_ELT(df,i))[iRecord]);
	  break;
	case REALSXP:
	  DBFWriteDoubleAttribute(hDBF,iRecord,i,
				   REAL(VECTOR_ELT(df,i))[iRecord]);
	  break;
        case STRSXP:
	  DBFWriteStringAttribute(hDBF,iRecord,i,
				   CHAR(STRING_ELT(VECTOR_ELT(df,i),iRecord)));
	  break;
	default:
	  error("Unknown data type");
	  break;
	}
      } 
    }

    return(hDBF);
}

