
/* 
 * Modified for R by Nicholas Lewin-Koh also made modifications for
 * multipart polygons. Sept 29, 2000
 */


#include <R.h>
#include <Rinternals.h>
#include "maptools.h"

SEXP RshpCentrd_2d (SEXP);
SEXP R_RingCentrd_2d (int , SEXP, double *);


/* **************************************************************************
 * RshpCentrd_2d
 *
 * Return the single mathematical / geometric centroid of a potentially 
 * complex/compound RShapeObject
 *
 * reject non area SHP Types
 * 
 * **************************************************************************/
SEXP RshpCentrd_2d (SEXP call) {
    int		ring, ringPrev, ring_nVertices, rStart, nprts;
    int         i,j,totvert;
    double	Area, ringArea;
    SEXP ringCentrd, Cent, shape, flag, ringVerts;
    shape = CADR(call);
    flag = CADDR(call);     
   
/*     if ( !(SHPDimension(INTEGER(getAttrib(shape,install("shp.type")))[0])  */
/*            & SHPD_AREA) )   */
/*         	 error("Not a class of shape with defined 2d area"); */

   nprts = INTEGER(getAttrib(shape, install("nParts")))[0];
   Area = 0;
   if(INTEGER(flag)[0]==0 ||nprts==1){
     PROTECT(Cent=allocVector(REALSXP, 2));
     REAL(Cent)[0] = 0.0;
     REAL(Cent)[1] = 0.0;
   }
   else{
     PROTECT(Cent=allocMatrix(REALSXP, nprts, 2));
   }
   /* for each ring in compound / complex object calc the ring cntrd	*/
   
   ringPrev = INTEGER(getAttrib(shape, install("nVerts")))[0];
   totvert = INTEGER(getAttrib(shape, install("nVerts")))[0];

   if(nprts==0) nprts=1;
   for ( ring = nprts-1; ring >= 0; ring-- ) {
     rStart = INTEGER(VECTOR_ELT(shape,0))[ring];
     ring_nVertices = ringPrev - rStart;
/*  Rprintf("ringPrev= %d, rStart=%d, ring_nVertices=%d \n", */
/*          ringPrev, rStart, ring_nVertices); */

   PROTECT(ringVerts=allocMatrix(REALSXP, ring_nVertices, 2));
   for(i=rStart,j=0;i<ringPrev ;i++,j++){
     REAL(ringVerts)[j]=REAL(VECTOR_ELT(shape,1))[i];
     REAL(ringVerts)[j+ring_nVertices]=REAL(VECTOR_ELT(shape,1))[i+totvert];
   }
/*  Rprintf(" matrix begin %f, matrix end: %f \n", */
/*  	     REAL(ringVerts)[0],REAL(ringVerts)[(2*ring_nVertices)-1]); */   
     PROTECT(ringCentrd = 
             R_RingCentrd_2d (ring_nVertices, ringVerts, &ringArea));  
/*  Rprintf("xcent: %f, ycent: %f, area: %f\n ",  */
/*              REAL(ringCentrd)[0],REAL(ringCentrd)[1],ringArea ); */

     /* use Superposition of these rings to build a composite Centroid	*/
     /* sum the ring centrds * ringAreas,  at the end divide by total area */
     if(INTEGER(flag)[0]==0 ||nprts==1){
       REAL(Cent)[0] +=  REAL(ringCentrd)[0] * ringArea;
       REAL(Cent)[1] +=  REAL(ringCentrd)[1] * ringArea; 
     }
     else{
       REAL(Cent)[ring]= REAL(ringCentrd)[0];
       REAL(Cent)[ring+nprts]= REAL(ringCentrd)[1]; 
     }
     Area += ringArea; 
     ringPrev = rStart;
     UNPROTECT(2);
    }    

     /* hold on the division by AREA until were at the end */
   if(INTEGER(flag)[0]==0 ||nprts==1){
     REAL(Cent)[0] = REAL(Cent)[0] / Area;
     REAL(Cent)[1] = REAL(Cent)[1] / Area;
     UNPROTECT(1);   
     return ( Cent );
   }
   else{
     UNPROTECT(1);   
     return ( Cent );
   }
}


/* **************************************************************************
 * RingCentroid_2d
 * Copyright (c) 1999, Carl Anderson
 *
 * This code is based in part on the earlier work of Frank Warmerdam
 * 
 *
 * Return the mathematical / geometric centroid of a single closed ring
 *
 * **************************************************************************/
SEXP R_RingCentrd_2d (int nVert, SEXP xy, double *Area ) {
  int		iv /*, jv */;
/*  int		sign_x, sign_y; */
  double	/* dy_Area, */ dx_Area, Cx_accum, Cy_accum, ppx, ppy;
  double 	x_base, y_base, x, y;
  SEXP          RingCent;
/* the centroid of a closed Ring is defined as
 *
 *      Cx = sum (cx * dArea ) / Total Area
 *  and
 *      Cy = sum (cy * dArea ) / Total Area
 */      
   
  x_base = REAL(xy)[0];
  y_base = REAL(xy)[nVert];
  
  Cy_accum = 0.0;
  Cx_accum = 0.0;

  ppx = REAL(xy)[1] - x_base;
  ppy = REAL(xy)[nVert + 1] - y_base;
  *Area = 0;

/* Skip the closing vector */
  for ( iv = 2; iv <= nVert - 2; iv++ ) {
    x = REAL(xy)[iv] - x_base;
    y = REAL(xy)[nVert + iv] - y_base;

    /* calc the area and centroid of triangle built out of an arbitrary  */
    /* base_point on the ring and each successive pair on the ring  */
    
    /* Area of a triangle is the cross product of its defining vectors	 */
    /* Centroid of a triangle is the average of its vertices		 */

    dx_Area =  ((x * ppy) - (y * ppx)) * 0.5;
    *Area += dx_Area;
    
    Cx_accum += ( ppx + x ) * dx_Area;       
    Cy_accum += ( ppy + y ) * dx_Area;
/*  #ifdef DEBUG2 */
/*      printf("(ringcentrd_2d)  Pp( %f, %f), P(%f, %f)\n", ppx, ppy, x, y); */
/*      printf("(ringcentrd_2d)    dA: %f, sA: %f, Cx: %f, Cy: %f \n",  */
/*  		dx_Area, *Area, Cx_accum, Cy_accum); */
/*  #endif   */  
    ppx = x;
    ppy = y;
  }

/*  #ifdef DEBUG2 */
/*    printf("(ringcentrd_2d)  Cx: %f, Cy: %f \n",  */
/*    	( Cx_accum / ( *Area * 3) ), ( Cy_accum / (*Area * 3) )); */
/*  #endif */

  /* adjust back to world coords 
  */
  PROTECT(RingCent=allocVector(REALSXP,2));
  REAL(RingCent)[0] = ( Cx_accum / ( *Area * 3)) + x_base;
  REAL(RingCent)[1] = ( Cy_accum / ( *Area * 3)) + y_base;
  UNPROTECT(1);   
  return (RingCent);
}

