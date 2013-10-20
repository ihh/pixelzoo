#ifndef UTIL_INCLUDED
#define UTIL_INCLUDED

#include <stdlib.h>

/* function pointer typedefs for generic containers */
typedef int (*CompareFunction) (void*, void*);
typedef void (*DestroyFunction) (void*);
typedef void* (*CopyFunction) (void*);
typedef void (*PrintFunction) (void*);

/* null functions for generic containers */
void NullDestroyFunction(void*);  /* does nothing */
void* NullCopyFunction(void*);  /* returns the supplied parameter without doing anything */
void NullPrintFunction(void*);  /* does nothing */

/* abort functions for when null functions should never be called */
void AbortDestroyFunction(void*);
void* AbortCopyFunction(void*);

/* typedef and container functions for 64-bit signed int's.
   (It's tempting to think that rather than allocating space, one could just use the (void*) pointer to store the int value;
   however, this risks platform-specific errors/warnings due to differences in bytesize/signedness between void* and int)
*/
typedef signed long long int Int64;

void* IntNew(Int64 a);
void* IntCopy(void* a);
void IntDelete(void* a);
int IntCompare(void* a, void* b);
void IntPrint(void* a);

/* Container functions for double's. */
void* DoubleNew(double a);
void* DoubleCopy(void* a);
void DoubleDelete(void* a);
int DoubleCompare(void* a, void* b);
void DoublePrint(void* a);

/* Synchronized integer times and probabilities */
/* The considerations driving the choice of units are as follows.
   We don't want arithmetic involving random numbers in BinTree/QuadTree to get too large:
    (max top-level value in a BinTree or QuadTree) * (max random number returned by rngRandomProb) < (max value of 'signed long long int')
 =>  (# of cells on board) * (max rate per cell in microHurtz) * (max random number returned by rngRandomProb) < (max value of 'signed long long int')
 =>  (2^X * 2^X) * (2^20) * (2^20) < (2^63)   for an X*X board
 =>  2X < 23
 =>   X <= 11
   We also want the time resolution to be fine enough to resolve individual cell events:
    (# of cells on board) * (max rate per cell in Hertz) * (shortest measurable time interval in Ticks) <= 1
 =>  (2^X * 2^X) * (1) * (2^{-20}) <= 2^0
 =>  2X <= 20
 =>   X <= 10
  So in practice the largest possible board should be 1024*1024.
  Note that, rather than Seconds and Hertz (i.e. Seconds^{-1}), we actually use Ticks (i.e. mean updates per cell) and Hurtz (i.e. Ticks^{-1}).
 */
typedef signed long long int int64_Ticks;        /* 1 Tick = one expected update per cell */
typedef signed long long int int64_Microticks;   /* 1 Microtick = 2^{-20} Ticks */
typedef signed long long int int64_Hurtz;        /* 1 Hurtz = 1 Tick^{-1} */
typedef signed long long int int64_Microhurtz;   /* 1 Microhurtz = 2^{-20} Hurtz */
typedef signed long long int int64_Millionths;   /* 1 Millionth = 2^{-20} */

#define PowerOfTwoClosestToOneMillion 0x100000
#define MillionthsMask                0x0fffff
#define FloatToIntMillionths(F) ((int) (.5 + (F) * PowerOfTwoClosestToOneMillion))
#define IntMillionthsToFloat(I) ((double) ((int64_Millionths) I) / (double) PowerOfTwoClosestToOneMillion)

/* Unimportant random numbers (e.g. for UI); these need not be reproducible.
   Use RandomNumberGenerator (mersenne.h) for reproducible pseudorandom numbers.
 */
double randomDouble();  /* randomDouble() returns a uniformly-distributed real number between 0 and 1 */
int randomInt(int);  /* randomInt(N) returns a uniformly-distributed integer from 0 to N-1 */
double randomExp();  /* randomExp() returns an exponentially-distributed real number between 0 and infinity */

/* DUMP */
#undef DUMP
#define DUMP(x, fmt) printf("%s:%u: %s=" fmt, __FILE__, __LINE__, #x, x)

/* MIN, MAX, ABS */
#undef MIN
#undef MAX
#undef ABS
#define MIN(X,Y) ((X) < (Y) ? (X) : (Y))
#define MAX(X,Y) ((X) > (Y) ? (X) : (Y))
#define ABS(X)   ((X) >= 0 ? (X) : -(X))

#define MIN2(X,Y,Z) MIN(MIN(X,Y),Z)
#define MAX2(X,Y,Z) MAX(MAX(X,Y),Z)

#define MIN3(W,X,Y,Z) MIN(MIN(W,X),MIN(Y,Z))
#define MAX3(W,X,Y,Z) MAX(MAX(W,X),MAX(Y,Z))

/* errors, warnings, assertions */
void Abort(char* error, ...);
void Assert(int assertion, char* error, ...);
void Warn(char* warning, ...);

/* alloc functions */
void *SafeMalloc(size_t size);
void *SafeCalloc(size_t count, size_t size);
#define SafeFree(PTR) free(PTR)
#define SafeFreeOrNull(PTR) if (PTR) SafeFree(PTR);

/* stupid quoting bollix */
#define QUOTEME_(x) #x
#define QUOTEME(x) QUOTEME_(x)

#endif /* UTIL_INCLUDED */
