#ifndef MERSENNE_INCLUDED
#define MERSENNE_INCLUDED

#include "util.h"

/* Pseudo-random number generator
   Uses Mersenne Twister MT19937ar
   http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/MT2002/emt19937ar.html
   http://en.wikipedia.org/wiki/Mersenne_twister
 */

#define MERSENNE_ARRAY_SIZE   624
#define MERSENNE_DEFAULT_SEED 5489UL

typedef struct RandomNumberGenerator {
  unsigned long mt[MERSENNE_ARRAY_SIZE]; /* the array for the state vector  */
  int mti;  /* mti==MERSENNE_ARRAY_SIZE+1 means mt[MERSENNE_ARRAY_SIZE] is not initialized */
} RandomNumberGenerator;

RandomNumberGenerator* newRNG();
void deleteRNG (RandomNumberGenerator* rng);

void rngSeed (RandomNumberGenerator* rng, unsigned long seed);
void rngSeedArray (RandomNumberGenerator* rng, unsigned long init_key[], int key_length);

unsigned long rngRandomInt32 (RandomNumberGenerator* rng);
long rngRandomInt31 (RandomNumberGenerator* rng);

int64_Millionths rngRandomProb (RandomNumberGenerator* rng);  /* samples from [0,1) in multiples of 2^{-20} */
int64_Microticks rngRandomWait (RandomNumberGenerator* rng, int64_Microhurtz rate);  /* equivalent to (int) (2^{40} * randomExp() / rate) */



#endif /* MERSENNE_INCLUDED */

