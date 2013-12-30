/*	generic.h -- contains some generic definitions
 that could be useful in any program

 USES: generic.c

 REQUIRES: none

 Chris Bennett @ LTER-CSU 6/15/2000
 5/19/2001  moved rand functions to rands.h
 10/22/2010	(drs)	replaced every occurence of F_DELTA, #define F_DELTA (10*FLT_EPSILON), with ( max( 10.*FLT_EPSILON, FLT_EPSILON*pow(10., ceil(log10(max(fabs(x),max(fabs(y), FLT_EPSILON))+1.)) ) ) ) and similar for D_DELTA
 because earlier version worked only for 0<=x<fabs(31)
 01/03/2011	(drs) macro 'isless' renamed to 'isless2'
 05/25/2012	(DLM) added regression() function
 05/29/2012   (DLM) added lobf(), lobfM(), & lobfCB() functions
 05/29/2012   (DLM) added squared(x) definition, this squares the value x (ie. returns the value of x^2).  the definition simply calls pow(x, 2) in the cmath library.  x must be a double. added for convenience
 03/08/2013	(clk) added abs(x) definition, this returns the absolute value of x. If x < 0, returns -x, else returns x.
 06/19/2013	(DLM) replaced isless2(), ismore(), iszero(), isequal() macros with new MUCH faster ones... these new macros make the program run about 4x faster as a whole.
 06/26/2013 (dlm)	powe(): an alternate definition of pow(x, y) for x>0... this is faster (ca. 20%) then the one in math.h, but comes with a cost as the precision is slightly lower.  The measured precision drop I was getting was at max a relative error of about 100 billion percent (12 places after the decimal point) per calculation though so I don't think it's a big deal... (though it's hard to even accurately tell)
 */

#ifndef GENERIC_H
#define GENERIC_H

#include <stdio.h>
#include <float.h>
#include <math.h>
#include <assert.h>

/***************************************************
 * Basic definitions
 ***************************************************/

/* ------ Convenience macros. ------ */
/* integer to boolean */
#define itob(i) ((i)?TRUE:FALSE)
/* integer versions */
#define max(a,b) (((a) > (b)) ? (a) : (b))
#define min(a,b) (((a) < (b)) ? (a) : (b))
/* floating point versions work for float or double */
#define fmax(a,b) ( ( GT((a),(b)) ) ? (a) : (b))
#define fmin(a,b) ( ( LT((a),(b)) ) ? (a) : (b))
/* absolute value for floating point values */
#define abs(a)  ( ( LT(a, 0.00) ) ? (-a) : (a) )
/* redefine sqrt for double (default) or float */
#ifdef NO_SQRTF
/* the case for Borland's compiler */
#define sqrtf sqrt
#endif

#define sqrt(x) ((sizeof(x)==sizeof(float)) ? sqrtf(x) : sqrt(x))

#define isnull(a) (NULL == (a))

/* -------  Some Time macros that should be always on ------ */
#define YearTo4Digit(y) ((TimeInt)(( (y) > 100)  \
                      ? (y)                      \
                      : ((y)<50) ? 2000+(y)      \
                                    : 1900+(y) ) )
#define WEEKDAYS 7
#define Doy2Week(d) ((TimeInt)(((d)-1) /WEEKDAYS))

/* ---------   Redefine basic types to be more malleable ---- */
typedef float RealF;
typedef double RealD;
typedef int Int;
typedef unsigned int IntU;
typedef short int IntS;
typedef unsigned short IntUS;
typedef long IntL;
#ifndef RSOILWAT
typedef enum {
	FALSE = (1 != 1), TRUE = (1 == 1)
} Bool;
#else
typedef int Bool;
#endif
typedef unsigned char byte;

/* an attempt to facilitate integer implementation of real */
/*
 typedef long IRealF
 typedef double long IRealD
 #define IF_GRAIN 10000000L
 #define F2I(x) ((IRealF)(x*IF_GRAIN))
 #define D2I(x) ((IRealD)(x*ID_GRAIN))
 #define I2F(x) ((( RealF)x/IF_GRAIN))
 #define I2D(x) ((( RealD)x/ID_GRAIN))
 */

/* --------------------------------------------------*/
/* These are facilities for logging errors.          */

/* constants for LogError() mode */
#define LOGNOTE  0x01
#define LOGWARN  0x02
#define LOGERROR 0x04
#define LOGEXIT  0x08
#define LOGFATAL 0x0c  /* LOGEXIT | LOGERROR */
#define MAX_ERROR 4096

extern FILE *logfp; /* REQUIRED */
/* This is the pointer to the log file.  It is declared in the
 * main module and externed here to make it available to any
 * file that needs it so all modules write to the same logfile.
 * See also the comments on 'logged' below.
 */

extern char errstr[]; /* REQUIRED */
/* declared in the main module, this is an ever-ready
 * buffer to put error text into for printing or
 * writing to the log file.
 */

extern int logged; /* REQUIRED */
/* use as a boolean: see gen_funcs.c.
 * Global variable indicates logfile written to via LogError.
 * In the main module, create a subroutine (eg, log_notify)
 * that tells the user or performs some other task based on the
 * event and include generic.h. Near the top of main(), set
 * logged = FALSE (ie, before calling LogError()) to clear the
 * flag.  Next call atexit(log_notify) to run the routine upon
 * termination.  Note that we can't declare logged as Bool
 * because Bool isn't defined in gen_funcs.c when logged is
 * declared, else we couldn't extern it here.
 */

/* --------------------------------------------------*/
/* --------------------------------------------------*/
/* The following tests account for imprecision in the
 floating point representation of either single
 or double real numbers.  Use these instead of
 the regular boolean operators.  Note that only
 the magnitude of the x operand is tested, so both
 operands (x & y) should be of the same type, or cast
 so.

 Update (DLM) : 6-19-2013 : These macros (iszero(), isequal(), isless2(), & ismore()) were taking a VERY long time to compute, so I wrote some new ones defined below.  I also rewrote the macros for LE() and GE() because they were making unnecessary calls.
 : These new macros cause the program to run about 4x faster as a whole.  The old macros are still here, but commented out for now.
 : The reason the old macros were taking sooo long is because they make lots of function calls many of which are to performance intensive functions (such as log10() and pow()) and the macros are used EVERYWHERE in the code (I'm guessing they are being called 100's of thousands of times).
 : The new macros perform the function (to account for imprecision in floating points), but do so in a less complicated way (by checking them against absolute/relative error)... the perform VERY close to the old functions (which I'm not sure why they are even making some of the calls they are as the complexity of the math is seemingly unnecessary), but not 100% the same... this shouldn't be much of an issue though because there will be errors in the floating point arithmetic at some point anyways, because that is the nature of floating point.
 : To have floating point calculations without some error is impossible by definition.  The reason why there are errors in floating point is because some numbers cannot be 100% accurately represented (like repeating decimals, PI, and the canonical example 0.1) in the scheme that floating point uses.  This makes sense if you think about it a little because numbers with a non-finite amount of digits after the decimal point cannot possibly be represented finitely by definition, so some numbers have to get cut off.
 : For the absolute error check I am using F_DELTA (D_DELTA for doubles) and for the relative error check I am using FLT_EPSILON (DBL_EPSILON for doubles).  F_DELTA is equal to 10*FLT_EPSILON, and DBL_DELTA is equal to 10*DBL_EPSILON.
 : FLT_EPSILON is defined in float.h (a part of the C STANDARD), and is equal to the smallest x such that "((1.0 + x) != 1.0)".  DBL_EPSILON is the same except for the double data type.
 : Also note that these macros should never be called with a side-effecting expression (ie x++ or ++x) as it will end up incrementing the variable more then desired... this is a problem that macros have.  This shouldn't be an issue though as these functions are only meant for floats/doubles, which are typically not used with side-effecting expressions.
 */
#define F_DELTA (10*FLT_EPSILON)
#define D_DELTA (10*DBL_EPSILON)

/*#define iszero(x) \
( (sizeof(x) == sizeof(float)) \
  ? ((x)>-( max( 10.*FLT_EPSILON, FLT_EPSILON*pow(10., ceil(log10(max(fabs(x),FLT_EPSILON)+1.)) ) ) ) && (x)<( max( 10.*FLT_EPSILON, FLT_EPSILON*pow(10., ceil(log10(max(fabs(x),FLT_EPSILON)+1.)) ) ) )) \
  : ((x)>-( max( 10.*DBL_EPSILON, DBL_EPSILON*pow(10., ceil(log10(max(fabs(x),DBL_EPSILON)+1.)) ) ) ) && (x)<( max( 10.*DBL_EPSILON, DBL_EPSILON*pow(10., ceil(log10(max(fabs(x),DBL_EPSILON)+1.)) ) ) )) )

 #define isequal(x,y) \
( (sizeof(x) == sizeof(float)) \
  ? ((x)>(y)-( max( 10.*FLT_EPSILON, FLT_EPSILON*pow(10., ceil(log10(max(fabs(x),max(fabs(y), FLT_EPSILON))+1.)) ) ) ) && (x)<(y)+( max( 10.*FLT_EPSILON, FLT_EPSILON*pow(10., ceil(log10(max(fabs(x),max(fabs(y), FLT_EPSILON))+1.)) ) ) )) \
  : ((x)>(y)-( max( 10.*DBL_EPSILON, DBL_EPSILON*pow(10., ceil(log10(max(fabs(x),max(fabs(y), DBL_EPSILON))+1.)) ) ) ) && (x)<(y)+( max( 10.*DBL_EPSILON, DBL_EPSILON*pow(10., ceil(log10(max(fabs(x),max(fabs(y), DBL_EPSILON))+1.)) ) ) )) )


 #define isless2(x,y) \
( (sizeof(x) == sizeof(float)) \
  ? ((x)<(y)-( max( 10.*FLT_EPSILON, FLT_EPSILON*pow(10., ceil(log10(max(fabs(x),max(fabs(y), FLT_EPSILON))+1.)) ) ) )) \
  : ((x)<(y)-( max( 10.*DBL_EPSILON, DBL_EPSILON*pow(10., ceil(log10(max(fabs(x),max(fabs(y), DBL_EPSILON))+1.)) ) ) )) )

 #define ismore(x,y) \
( (sizeof(x) == sizeof(float)) \
  ? ((x)>(y)+( max( 10.*FLT_EPSILON, FLT_EPSILON*pow(10., ceil(log10(max(fabs(x),max(fabs(y), FLT_EPSILON))+1.)) ) ) )) \
  : ((x)>(y)+( max( 10.*DBL_EPSILON, DBL_EPSILON*pow(10., ceil(log10(max(fabs(x),max(fabs(y), DBL_EPSILON))+1.)) ) ) )) )*/

// new definitions for these four macros (MUCH MUCH faster, by a factor of about 4)... just trying them out for now.  The idea behind how these work is that both an absolute error and relative error check are being used in conjunction with one another.  In this for now I'm using F_DELTA for the amount of absolute error allowed and FLT_EPSILON for the amount of relative error allowed.
#define GET_F_DELTA(x, y) ( (sizeof(x) == sizeof(float)) ? (max(F_DELTA, FLT_EPSILON * max(fabs(x), fabs(y)))) : (max(D_DELTA, DBL_EPSILON * max(fabs(x), fabs(y)))) )

#define isless2(x, y) ((x) < ((y) - GET_F_DELTA(x, y)))
#define ismore(x, y) ((x) > ((y) + GET_F_DELTA(x, y)))
#define iszero(x) ( (sizeof(x) == sizeof(float)) ? (fabs(x) <= F_DELTA) : (fabs(x) <= D_DELTA) ) //for iszero(x) we just use an absolute error check, because a relative error check doesn't make sense for any number close enough to zero to be considered equal... it would be a waste of time.
#define isequal(x, y) (fabs((x) - (y)) <= GET_F_DELTA(x, y))

/* some simpler invocations */
#define ZRO(x)   iszero(x)
#define EQ(x,y)  isequal(x,y)
#define LT(x,y)  isless2(x,y)
#define GT(x,y)  ismore(x,y)
#define LE(x,y) ((x) < (y) || EQ(x,y)) //changed from "(LT(x,y)||EQ(x,y))" because it evaluates to the same result (since EQ is already doing the checking for precision errors so use < instead of LT to avoid checking for precision errors twice) and this version is faster by avoiding the expensive LT() call.  
#define GE(x,y) ((x) > (y) || EQ(x,y))

// 06/26/2013 (dlm)	powe(): an alternate definition of pow(x, y) for x>0... this is faster (ca. 20%) then the one in math.h, but comes with a cost as the precision is slightly lower.  The measured precision drop I was getting was at max a relative error of about 100 billion percent (12 places after the decimal point) per calculation though so I don't think it's a big deal... (though it's hard to even accurately tell)
#define powe(x, y) (exp((y) * log(x))) //x^y == exponential(y * ln(x)) or e^(y * ln(x)).  NOTE: this will only work when x > 0 I believe
#define squared(x) powe(fabs(x), 2.0) // added for convenience
/***************************************************
 * Function definitions
 ***************************************************/

char *Str_TrimRight(char *s);
char *Str_TrimLeft(char *s);
char *Str_TrimLeftQ(char *s); /* "quick" version */
char *Str_ToUpper(char *s, char *r);
char *Str_ToLower(char *s, char *r);
int Str_CompareI(char *t, char *s);
void UnComment(char *s);
void LogError(FILE *fp, const int mode, const char *fmt, ...);
Bool Is_LeapYear(int yr);
double regression(double x1, double x2, double y1, double y2, double deltaX);
void st_getBounds(unsigned int *x1, unsigned int *x2, unsigned int *equal, unsigned int size, double depth, double bounds[]);
double lobfM(double xs[], double ys[], unsigned int n);
double lobfB(double xs[], double ys[], unsigned int n);
void lobf(double *m, double* b, double xs[], double ys[], unsigned int size);

#ifdef DEBUG
extern errstr[];
#define LogError(fp, m, fmt, p1, p2, p3, p4, p5, p6, p7, p8, p9) \
          sprintf(errstr, fmt, p1, p2, p3, p4, p5, p6, p7, p8, p9); \
          LogError(fp, m, errstr);
#endif

#define GENERIC_H
#endif
