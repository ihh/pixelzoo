#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "particle.h"
#include "stringmap.h"
#include "vars.h"

Particle* newParticle (const char* name) {
  Particle* p;
  int c;
  p = SafeMalloc (sizeof (Particle));
  p->type = 0;
  p->name = StringCopy ((void*) name);
  p->vars = newList (AbortCopyFunction, deleteVarsDescriptor, NullPrintFunction);
  for (c = 0; c < NumColorRules; ++c) {
    p->colorRule[c].mask = 0;
    p->colorRule[c].offset = 0;
    p->colorRule[c].multiplier = 0;
    p->colorRule[c].rightShift = 0;
  }
  p->colorRule[0].offset = HSB24White;  /* this ensures that the default ColorRule's generate a white particle */
  p->synchronous = p->syncPeriod = p->syncPhase = 0;
  p->rule = NULL;
  p->rate = p->asyncFiringRate = p->syncFiringRate = 0.;
  p->dispatch = NULL;
  p->count = 0;
  return p;
}

void deleteParticle (Particle* p) {
  if (p->dispatch)
    deleteRBTree (p->dispatch);
  if (p->rule)
    deleteParticleRule (p->rule);
  deleteList (p->vars);
  StringDelete(p->name);
  SafeFree(p);
}

void addParticleMessageHandler (Particle *p, Message message, ParticleRule *handler) {
  if (p->dispatch == NULL)
    p->dispatch = newRBTree (IntCompare, IntCopy, AbortCopyFunction, IntDelete, deleteParticleRule, NullPrintFunction, NullPrintFunction);
  RBTreeInsert (p->dispatch, &message, handler);
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
