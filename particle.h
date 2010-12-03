#ifndef PARTICLE_INCLUDED
#define PARTICLE_INCLUDED

#include "rule.h"
#include "util.h"

/*
  A Particle consists of:
   a Type,
   a name (C string),
   an RGB color,
   a variable number of StochasticRule's (all of the same Type as the Particle),
   a total firing rate (the sum of the firing rates for all the StochasticRule's for this type).
 */
typedef struct Particle {
  Type type;
  char* name;
  RGB color;
  int nRules;  /* number of rules */
  StochasticRule* rule;
  double totalRate, normalizedRate;
} Particle;

/* constructor/destructor are private to Board; use newBoardParticle instead */
Particle* newParticle (char* name, int nRules);
void deleteParticle (Particle* particle);

#endif /* PARTICLE_INCLUDED */
