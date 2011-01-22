#ifndef PARTICLE_INCLUDED
#define PARTICLE_INCLUDED

#include "rule.h"
#include "color.h"
#include "util.h"

/* number of ColorRule's per Particle */
#define NumColorRules 4

/*
  A Particle consists of:
   a Type,
   a name (C string),
   an RGB color,
   a variable number of ParticleRule's (all of the same Type as the Particle),
   a total firing rate (the sum of the firing rates for all the ParticleRule's for this type).
 */
typedef struct Particle {
  Type type;
  char* name;
  ColorRule colorRule[NumColorRules];  /* results of ColorRule applications are summed */
  int synchronous, syncPeriod, syncPhase;  /* if synchronous=1, rules will be updated synchronously vs randomly, whenever (board->syncUpdates % syncPeriod == syncPhase) */
  /* rules */
  ParticleRule* rule;
  double rate, asyncFiringRate, syncFiringRate;  /* if async then (asyncFiringRate, syncFiringRate) = (rate, 0)  else (asyncFiringRate, syncFiringRate) = (0, rate) */
  /* meta-info */
  int count;  /* number on the board */
} Particle;

/* constructor/destructor */
Particle* newParticle (const char* name);
void deleteParticle (Particle* particle);

/* color */
PaletteIndex getParticleColor (Particle* particle, State state);

#endif /* PARTICLE_INCLUDED */
