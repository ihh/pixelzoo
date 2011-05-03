#ifndef PARTICLE_INCLUDED
#define PARTICLE_INCLUDED

#include "rule.h"
#include "color.h"
#include "util.h"
#include "statemap.h"

/* number of ColorRule's per Particle */
#define NumColorRules 4

/*
  A Particle, uniquely identified by the 16 most significant bits of a State,
  represents a type (class) of cell on the Board.
 */
typedef struct Particle {

  /* Name and type */
  char *name;
  Type type;

  /* Color rules */
  ColorRule colorRule[NumColorRules];  /* results of ColorRule applications are summed */

  /* Evolution rule info: */
  /*  evolution rule callback style: synchronous or (preferred) asynchronous? */
  int synchronous, syncPeriod, syncPhase;  /* if synchronous=1, rules will be updated synchronously vs randomly, whenever (board->syncUpdates % syncPeriod == syncPhase) */
  /*  evolution rule callback rate */
  int64_Microhurtz rate;
  /*  evolution rule */
  ParticleRule* rule;

  /* Read-only bits */
  State readOnly[ReadOnlyStates];

  /* Message dispatch table */
  RBTree *dispatch;  /* map from Message to (Rule*) */

  /* Meta-info */
  int count;  /* number of instances of this Particle on the board */
  int64_Microhurtz asyncFiringRate, syncFiringRate;  /* if async then (asyncFiringRate, syncFiringRate) = (rate, 0)  else (asyncFiringRate, syncFiringRate) = (0, rate) */

} Particle;

/* constructor/destructor */
Particle *newParticle (const char* name);
void deleteParticle (Particle *particle);

/* dispatch table builder */
void addParticleMessageHandler (Particle *particle, Message message, ParticleRule *handler);

/* color */
PaletteIndex getParticleColor (Particle *particle, State state);

#endif /* PARTICLE_INCLUDED */
