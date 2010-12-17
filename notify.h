#ifndef NOTIFY_INCLUDED
#define NOTIFY_INCLUDED

#include "rule.h"
#include "particle.h"
#include "board.h"
#include "goal.h"
#include "npc.h"
#include "xymap.h"

typedef struct BoardWatcher BoardWatcher;

/* only one type of message for rules: the rule has just been triggered.
   ignored[NumRuleConditions]: record of which RuleCondition's were ignored
   failed[NumRuleOperations]: record of which RuleOperation's failed
   oldState: record of pre-rule states (map from XYCoord->State)
 */
typedef void (*RuleNotifyFunction) (Particle *ruleOwner, StochasticRule *rule, Board *board, int x, int y, int *ignored, int *failed, XYMap *oldState, BoardWatcher *watcher);

/* Particles can receive several types of message, always sent after the Particle is written */
typedef void (*ParticleNotifyFunction) (Particle *affectedParticle, Board *board, int x, int y, State oldState, State newState, BoardWatcher *watcher);

/* a BoardWatcher is a name, a set of notify functions, and some context */
struct BoardWatcher {
  char *name;
  RuleNotifyFunction ruleTriggered;
  ParticleNotifyFunction particleCreated, particleModified, particleDestroyed;
  NPC *npc;
  Goal *goal;
  void *context;   /* room for miscellaneous extra context */
};

/* methods */
BoardWatcher* newBoardWatcher (char *name, RuleNotifyFunction ruleNotify, ParticleNotifyFunction particleNotify);  /* copies 'name' */
void deleteBoardWatcher (BoardWatcher* watcher);
void addRuleWatcher (StochasticRule* rule, BoardWatcher* watcher);
void addParticleWatcher (Particle* particle, BoardWatcher* watcher);
void removeRuleWatcher (StochasticRule* rule, char* name);
void removeParticleWatcher (Particle* particle, char* name);

/* null notify functions */
void NullRuleNotify (Particle *ruleOwner, StochasticRule *rule, Board *board, int x, int y, int *ignored, int *failed, XYMap *oldState, BoardWatcher *watcher);
void NullParticleNotify (Particle *affectedParticle, Board *board, int x, int y, State oldState, State newState, BoardWatcher *watcher);

#endif /* NOTIFY_INCLUDED */
