
/*
  Opens an ESRI shape file and reads the information into a shapelist
  object
*/

#include "maptools.h"
#include <R_ext/PrtUtil.h>

/*#define DEBUG 1*/
SEXP Rshapeget(SEXP);

SEXP Rshapeget(SEXP shpnm)

{
    SHPHandle	hSHP;
    int    nShapeType, nEntities, i;
    double  adfMinBound[4], adfMaxBound[4];
    int j, vdir=0, Ring;
    SHPObject *psShape;

    SEXP  ShapePartNames, Rshplst, MapPartNames, temp, shplistnms;
#ifdef DEBUG
    PT Cent;
    double Area;
#endif

/* -------------------------------------------------------------------- */
/*      Open the passed shapefile.                                      */
/* -------------------------------------------------------------------- */

    hSHP = SHPOpen(CHAR(STRING_ELT(shpnm,0)), "rb" );
    if( hSHP == NULL )    
	error("unable to open file");


/* -------------------------------------------------------------------- */
/*      Print out the file bounds.                                      */
/* -------------------------------------------------------------------- */
      SHPGetInfo( hSHP, &nEntities, &nShapeType, adfMinBound, adfMaxBound ); 
      Rprintf("Shapefile Type: %s   # of Shapes: %d\n\n",
            SHPTypeName( nShapeType ), nEntities );

        PROTECT(Rshplst=allocVector(VECSXP, nEntities));
        PROTECT(MapPartNames = allocVector(STRSXP, nEntities));
        PROTECT(temp=allocVector(STRSXP, 1));
        
	if(nShapeType==1){ /* Points */
		SET_STRING_ELT(temp, 0, mkChar("point"));
	        setAttrib(Rshplst, install("shp.type"), temp);
	}
	else if(nShapeType==3){  /* Lines */
		SET_STRING_ELT(temp, 0,  mkChar("arc"));
	        setAttrib(Rshplst, install("shp.type"), temp);
	}
	else if(nShapeType==5){/* Polygons */
		SET_STRING_ELT(temp, 0, mkChar("poly"));
	        setAttrib(Rshplst, install("shp.type"), temp);
		vdir=1;
	}
	else{
	  error("Not a valid shape type");
	}
        UNPROTECT(1);

        PROTECT(temp=allocVector(INTSXP,1));
        INTEGER(temp)[0] = nEntities;
	setAttrib(Rshplst,install("nshps"),temp);
	UNPROTECT(1);

        PROTECT(temp=allocVector(REALSXP,4));
	REAL(temp)[0] = adfMinBound[0];
	REAL(temp)[1] = adfMinBound[1];
	REAL(temp)[2] = adfMinBound[2];
	REAL(temp)[3] = adfMinBound[3];
	setAttrib(Rshplst,install("minbb"),temp);
	UNPROTECT(1);

        PROTECT(temp=allocVector(REALSXP,4));
	REAL(temp)[0] = adfMaxBound[0];
	REAL(temp)[1] = adfMaxBound[1];
	REAL(temp)[2] = adfMaxBound[2];
	REAL(temp)[3] = adfMaxBound[3];
	setAttrib(Rshplst,install("maxbb"),temp);
	UNPROTECT(1);


/*--------------------------------------------------------------------
	Skim over the list of shapes, printing all the vertices.	
 --------------------------------------------------------------------*/
   for( i = 0; i < nEntities; i++ ) 
     { 
	psShape = SHPReadObject( hSHP, i);
        SET_VECTOR_ELT(Rshplst, i, allocVector(VECSXP, 2));
 
        PROTECT(temp=allocVector(INTSXP, 1));
        INTEGER(temp)[0] = psShape->nSHPType;
	setAttrib(VECTOR_ELT(Rshplst, i), install("shp.type"), temp);
        UNPROTECT(1);
        PROTECT(temp=allocVector(INTSXP, 1));
        INTEGER(temp)[0] = psShape->nVertices;
	setAttrib(VECTOR_ELT(Rshplst, i), install("nVerts"), temp);
        UNPROTECT(1);
        PROTECT(temp=allocVector(INTSXP, 1));
        INTEGER(temp)[0] = psShape->nParts;
	setAttrib(VECTOR_ELT(Rshplst, i), install("nParts"), temp);
        UNPROTECT(1);
        if(vdir==1){
	   PROTECT(temp=allocVector(INTSXP, psShape->nParts));
	   if(psShape->nParts == 1){
	     INTEGER(temp)[0] = SHPRingDir_2d (psShape,0);
#ifdef DEBUG
	     Rprintf("direction: %d \n", INTEGER(temp)[0]);
#endif               
	   }
           else{
#ifdef DEBUG
	     Rprintf("direction:");
#endif  
	     for(Ring=0;Ring<psShape->nParts;Ring++){
	       INTEGER(temp)[Ring] =SHPRingDir_2d (psShape,Ring);
#ifdef DEBUG
	       Rprintf(" %d ", INTEGER(temp)[Ring]);
#endif 
	     }
#ifdef DEBUG
	     Rprintf("\n");
#endif  
	   }
	   setAttrib(VECTOR_ELT(Rshplst,i),install("RingDir"),temp);
	   UNPROTECT(1);
#ifdef DEBUG
           
           Cent=SHPCentrd_2d ( psShape );
           Rprintf("Center X: %f, Center Y: %f \n", Cent.x, Cent.y);
           Area = SHPArea_2d (psShape );
           Rprintf("Area: %f \n", Area);
#endif   
	}
	PROTECT(temp=allocVector(REALSXP,4));
	REAL(temp)[0] = psShape->dfXMin;
	REAL(temp)[1] = psShape->dfYMin;
	REAL(temp)[2] = psShape->dfXMax;
	REAL(temp)[3] = psShape->dfYMax;
	setAttrib(VECTOR_ELT(Rshplst,i),install("bbox"),temp);
        UNPROTECT(1);

        SET_VECTOR_ELT(VECTOR_ELT(Rshplst,i),0, 
	  allocVector(INTSXP,psShape->nParts));	
        SET_VECTOR_ELT(VECTOR_ELT(Rshplst,i),1,
	  allocMatrix(REALSXP,psShape->nVertices,2));
        PROTECT(shplistnms = allocVector(STRSXP,2));
	SET_STRING_ELT(shplistnms,0,mkChar("Pstart"));
	SET_STRING_ELT(shplistnms,1,mkChar("verts"));
	setAttrib(VECTOR_ELT(Rshplst,i),R_NamesSymbol,shplistnms);
        UNPROTECT(1);

	for( j = 0; j < psShape->nVertices; j++ )
	{
	    REAL(VECTOR_ELT(VECTOR_ELT(Rshplst,i),1))[j]=psShape->padfX[j];
	    REAL(VECTOR_ELT(VECTOR_ELT(Rshplst,i),1))[j+psShape->nVertices]=
	                                  psShape->padfY[j];
	}
	
        
        if(psShape->nParts > 0 ){
	  const char	*pszPartType = "";   
	  PROTECT(ShapePartNames = allocVector(STRSXP,psShape->nParts));         
	  if(psShape->nParts == 1){
	    INTEGER(VECTOR_ELT(VECTOR_ELT(Rshplst,i),0))[0]=0;
	    pszPartType = SHPPartTypeName( psShape->panPartType[0] );
	    SET_STRING_ELT(ShapePartNames,0,mkChar(pszPartType));
	  }
	  else{
	    for(j = 0; j < psShape->nParts; j++ )
	      {
		INTEGER(VECTOR_ELT(VECTOR_ELT(Rshplst,i),0))[j]=
		  psShape->panPartStart[j];
		pszPartType = SHPPartTypeName( psShape->panPartType[j] );
	        SET_STRING_ELT(ShapePartNames,j,mkChar(pszPartType));
	      }
	  }
	  setAttrib(VECTOR_ELT(VECTOR_ELT(Rshplst,i),0),
		    R_RowNamesSymbol,ShapePartNames);
	  UNPROTECT(1);
	  SHPDestroyObject( psShape );
	}
     }
     SHPClose(hSHP);
     UNPROTECT(2);
#ifdef USE_DBMALLOC
    malloc_dump(2);
#endif

    return(Rshplst);
}

/******************************************************************************
 * Copyright (c) 1999, Carl Anderson
 *
 * This code is based in part on the earlier work of Frank Warmerdam
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
 * requires shapelib 1.2
 *   gcc shpproj shpopen.o dbfopen.o -lm -lproj -o shpproj
 * 
 * this may require linking with the PROJ4.3 projection library available from
 *
 * ftp://kai.er.usgs.gov/ftp/PROJ.4
 *
 * use -DPROJ4 to compile in Projection support
 *
 * $Log: shpgeo.c,v $
 * Revision 1.3  2000/03/17 14:15:16  warmerda
 * Don't try to use system nan.h ... doesn't always exist. */

 /* I'm using some shorthand throughout this file
 *      R+ is a Clockwise Ring and is the positive portion of an object
 *      R- is a CounterClockwise Ring and is a hole in a R+
 *      A complex object is one having at least one R-
 *      A compound object is one having more than one R+
 *	A simple object has one and only one element (R+ or R-)
 *
 *	The closed ring constraint is for polygons and assumed here
 *	Arcs or LineStrings I am calling Rings (generically open or closed)
 *	Point types are vertices or lists of vertices but not Rings
 *
 *   SHPT_POLYGON, SHPT_POLYGONZ, SHPT_POLYGONM and SHPT_MULTIPATCH
 *   can have SHPObjects that are compound as well as complex
 *  
 *   SHP_POINT and its Z and M derivatives are strictly simple
 *   MULTI_POINT, SHPT_ARC and their derivatives may be simple or compound
 *
 */

/* **************************************************************************
 * SHPRingDir_2d
 *
 * Test Polygon for CW / CCW  ( R+ / R- )
 *
 * return 1  for R+
 * return -1 for R-
 * return 0  for error
 * **************************************************************************/
 int SHPRingDir_2d (SHPObject *psCShape, int Ring) {
   int		i, ti=0, last_vtx;
   double	tX;
   double 	*a, *b;
   double	dx0, dx1, dy0, dy1, /* v1, v2, */ v3;
   
   tX = 0.0;
   a = psCShape->padfX;
   b = psCShape->padfY;
   
   if ( Ring >= psCShape->nParts ) return ( 0 );
   
   if ( Ring >= psCShape->nParts -1 )
     { last_vtx = psCShape->nVertices; }
    else
     { last_vtx = psCShape->panPartStart[Ring + 1]; }
      
   /* All vertices at the corners of the extrema (rightmost lowest, leftmost lowest, 	*/
   /* topmost rightest, ...) must be less than pi wide.  If they werent they couldnt be	*/
   /* extrema.																			*/   
   /* of course the following will fail if the Extents are even a little wrong 			*/      
   
   for ( i = psCShape->panPartStart[Ring]; i < last_vtx; i++ ) {
     if ( b[i] == psCShape->dfYMax && a[i] > tX ) 
      { ti = i; }
   }

#ifdef DEBUG2
   Rprintf ("(shpgeo:SHPRingDir) highest Rightmost Pt is vtx %d (%f, %f)\n", ti, a[ti], b[ti]);
#endif   
   
   /* cross product */
   /* the sign of the cross product of two vectors indicates the right or left half-plane	*/
   /* which we can use to indicate Ring Dir													*/ 
   if ( (ti > psCShape->panPartStart[Ring]) & (ti < last_vtx) ) 
    { dx0 = a[ti-1] - a[ti];
      dx1 = a[ti+1] - a[ti];
      dy0 = b[ti-1] - b[ti];
      dy1 = b[ti+1] - b[ti];
   }
   else
   /* if the tested vertex is at the origin then continue from 0 */ 
   {  dx1 = a[1] - a[0];
      dx0 = a[last_vtx] - a[0];
      dy1 = b[1] - b[0];
      dy0 = b[last_vtx] - b[0];
   }
   
/*   v1 = ( (dy0 * 0) - (0 * dy1) );
   v2 = ( (0 * dx1) - (dx0 * 0) );
 these above are always zero so why do the math */
   v3 = ( (dx0 * dy1) - (dx1 * dy0) );

#ifdef DEBUG2   
   Rprintf ("(shpgeo:SHPRingDir)  cross product for vtx %d was %f \n", ti, v3); 
#endif

   if ( v3 > 0 )
    { return (1); }
   else
    { return (-1); }
}


