#include "maptools.h"

#include <R_ext/PrtUtil.h>
#include <Rmath.h>

 
SEXP shpwrite(SEXP call)
{
    SEXP fname, shptype, shapes;
    SHPHandle   hSHP;
    SHPObject   *psShape;
    int         nShapeType, i, nShapes;
 
    if (!isValidString(fname = CADR(call)))
        error ("first argument must be a file name\n");
    
    if (!isValidString(shptype = CADDR(call)))
        error ("second argument must be a string\n");

    shapes=CADDDR(call);

    Rprintf("stage 1 \n");
/* -------------------------------------------------------------------- */
/*      Figure out the shape type.                                      */
/* -------------------------------------------------------------------- */
    if( strcmp(CHAR(STRING_ELT(shptype,0)),"POINT") == 0 || 
	strcmp(CHAR(STRING_ELT(shptype,0)),"point") == 0 )
        nShapeType = SHPT_POINT;
    else if( strcmp(CHAR(STRING_ELT(shptype,0)),"ARC") == 0 || 
	     strcmp(CHAR(STRING_ELT(shptype,0)),"arc") == 0 )
        nShapeType = SHPT_ARC;
    else if( strcmp(CHAR(STRING_ELT(shptype,0)),"POLYGON") == 0 || 
	     strcmp(CHAR(STRING_ELT(shptype,0)),"polygon") == 0 )
        nShapeType = SHPT_POLYGON;
    else if( strcmp(CHAR(STRING_ELT(shptype,0)),"MULTIPOINT")==0 ||
	     strcmp(CHAR(STRING_ELT(shptype,0)),"multipoint")==0)
        nShapeType = SHPT_MULTIPOINT;
    else
    {
        error( "Shape Type `%s' not recognised.\n", CHAR(STRING_ELT(shptype,0)));
        exit( 2 );
    }
/* -------------------------------------------------------------------- */
/*      Create the requested layer.                                     */
/* -------------------------------------------------------------------- */
    Rprintf("stage 2, Shapetype is %d \n", nShapeType);
    hSHP = SHPCreate(R_ExpandFileName(CHAR(STRING_ELT(fname,0))), nShapeType );

    if( hSHP == NULL )
    {
         error("Unable to create:%s\n", CHAR(STRING_ELT(fname,0)) );
    }

/* -------------------------------------------------------------------- */
/*      If points, add individual points                                */
/* -------------------------------------------------------------------- */
    if(nShapeType == SHPT_POINT){
      nShapes=LENGTH(shapes)/2;
      for(i = 0; i<nShapes; i++){
	psShape = SHPCreateSimpleObject( nShapeType, 1, 
					  &REAL(shapes)[i] ,
					  &REAL(shapes)[i + nShapes] , 
					  NULL);
	SHPWriteObject( hSHP, -1, psShape );
        SHPDestroyObject( psShape );
      }
    }



    SHPClose( hSHP );
    Rprintf("stage 3 \n");
    return R_NilValue;
}
