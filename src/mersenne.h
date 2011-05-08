#ifndef MERSENNE_INCLUDED
#define MERSENNE_INCLUDED

#include "util.h"

/* Pseudo-random number generator
   Uses Mersenne Twister MT19937ar
   http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/MT2002/emt19937ar.html
   http://en.wikipedia.org/wiki/Mersenne_twister
 */

#define MERSENNE_ARRAY_SIZE 624
#define MERSENNE_STATE_STRING_LENGTH (8 * MERSENNE_ARRAY_SIZE + 8)

typedef struct RandomNumberGenerator {
  unsigned long mt[MERSENNE_ARRAY_SIZE]; /* the array for the state vector  */
  int mti;  /* mti==MERSENNE_ARRAY_SIZE+1 means mt[MERSENNE_ARRAY_SIZE] is not initialized */
} RandomNumberGenerator;

RandomNumberGenerator* newRNG();
void deleteRNG (RandomNumberGenerator* rng);

/* the following "seeding" functions are used to create initial randomness */
void rngSeed (RandomNumberGenerator* rng, unsigned long seed);
void rngSeedArray (RandomNumberGenerator* rng, unsigned long init_key[], int key_length);

/* the following "state" functions can be used to save/load the state of the RNG for exact reproducibility */
char* getRngStateString (RandomNumberGenerator* rng);  /* allocates new string; caller's responsibility to delete */
void rngSetStateString (RandomNumberGenerator* rng, char* stateString);

#define deleteRngStateString(S) SafeFree(S)

/* actual methods to sample random deviates */
unsigned long rngRandomInt32 (RandomNumberGenerator* rng);  /* samples integer from [0,0xFFFFFFFF] */
long rngRandomInt31 (RandomNumberGenerator* rng);  /* samples integer from [0,0x7FFFFFFF] */

int64_Millionths rngRandomProb (RandomNumberGenerator* rng);  /* returns x from [0,0xFFFFF] where x = (unsigned long long) (p / 2^{20}) and p is sampled from [0,1) */
int64_Microticks rngRandomWait (RandomNumberGenerator* rng, int64_Microhurtz rate);  /* returns w from [0,0x7FFFFFFFFFFFFFFF] where w = (unsigned long long) (2^{40} * -log(p) / rate) and p is sampled from [0,1) */



#endif /* MERSENNE_INCLUDED */

