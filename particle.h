#ifndef PARTICLE_INCLUDED
#define PARTICLE_INCLUDED

#include "rule.h"
#include "color.h"
#include "util.h"

/* number of ColorRule's per Particle */
#define NumColorRules 3

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
  ColorRule colorRule[NumColorRules];  /* results of ColorRule applications are summed */
  int nRules;  /* number of rules */
  StochasticRule* rule;
  double totalRate, normalizedRate, totalOverloadRate;
  StringMap* watchers;  /* BoardWatcher's (keyed by name), or NULL */
} Particle;

/* constructor/destructor */
Particle* newParticle (const char* name, int nRules);
void deleteParticle (Particle* particle);

/* color */
PaletteIndex getParticleColor (Particle* particle, State state);

#endif /* PARTICLE_INCLUDED */
