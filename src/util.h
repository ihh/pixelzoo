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

/* Container functions for int's.
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
    (# of cells on board) * (max rate per cell in integer units) * (max random number returned by syncRandomProb) < (max value of 'signed long long int')
   Assume max rate is 100Hz = 10^8 Microhertz:
    (128*128) * (10^8) * (10^6) < (2^63)
   In fact this holds for a board size up to 256*256.
   We also want the time resolution to be fine enough to resolve individual cell events:
    (# of cells on board) * (max rate per cell in Hertz) * (shortest measurable time interval in seconds) <~ 1
    (128*128) * (10^2) * (10^-6) = 1.6384   .... a bit high, but should be OK unless board is literally filled with bullets/gas

  Note that, rather than Seconds and Hertz (i.e. Seconds^{-1}), we actually use Ticks (i.e. mean updates per cell) and Hurtz (i.e. Ticks^{-1}).
 */
typedef signed long long int int64_Ticks;
typedef signed long long int int64_Microticks;   /* actually multiples of 2^{-20} Ticks */
typedef signed long long int int64_Hurtz;
typedef signed long long int int64_Microhurtz;   /* actually multiples of 2^{-20} Hurtz */
typedef signed long long int int64_Millionths;   /* actually multiples of 2^{-20} */

#define PowerOfTwoClosestToOneMillion (1 << 20)

/* Unimportant random numbers */
double randomDouble();  /* randomDouble() returns a uniformly-distributed real number between 0 and 1 */
int randomInt(int);  /* randomInt(N) returns a uniformly-distributed integer from 0 to N-1 */
double randomExp();  /* randomExp() returns an exponentially-distributed real number between 0 and infinity */

/* Important random numbers (must be reproducible) */
typedef void* RandomNumberGenerator;
int64_Millionths rngRandomProb (RandomNumberGenerator rng);  /* synchronized equivalent to (int) (2^{20} * randomDouble()) */
int64_Microticks rngRandomWait (RandomNumberGenerator rng, int64_Microhurtz rate);  /* synchronized equivalent to (int) (2^{40} * randomExp() / rate) */

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

/* alloc functions */
void Abort(char* error);
void Assert(int assertion, char* error);
void *SafeMalloc(size_t size);
void *SafeCalloc(size_t count, size_t size);
#define SafeFree(PTR) free(PTR)
#define SafeFreeOrNull(PTR) if (PTR) SafeFree(PTR);

/* stupid quoting bollix */
#define QUOTEME_(x) #x
#define QUOTEME(x) QUOTEME_(x)

#endif /* UTIL_INCLUDED */
