#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "particle.h"

Particle* newParticle (const char* name) {
  Particle* p;
  int c;
  p = SafeMalloc (sizeof (Particle));
  p->type = 0;
  p->name = SafeMalloc ((strlen (name) + 1) * sizeof(char));
  strcpy (p->name, (char*) name);
  for (c = 0; c < NumColorRules; ++c) {
    p->colorRule[c].mask = 0;
    p->colorRule[c].offset = 0;
    p->colorRule[c].multiplier = 0;
    p->colorRule[c].rightShift = 0;
  }
  p->colorRule[0].offset = HSB24White;  /* this ensures that the default ColorRule's generate a white particle */
  p->synchronous = p->shuffle = p->syncPeriod = p->syncPhase = 0;
  p->rule = NULL;
  p->rate = p->asyncFiringRate = p->syncFiringRate = 0.;
  p->count = 0;
  return p;
}

void deleteParticle (Particle* p) {
  if (p->rule)
    deleteParticleRule (p->rule);
  SafeFree(p->name);
  SafeFree(p);
}

PaletteIndex getParticleColor (Particle* particle, State state) {
  int n;
  HSB24 hsb;
  hsb = 0;
  for (n = 0; n < NumColorRules; ++n) {
    hsb = hsb + evalColorRule (&particle->colorRule[n], state);
  }
  return ConvertHsb24ToPaletteIndex(hsb);
}
