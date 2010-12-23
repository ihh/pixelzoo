#ifndef NOTIFY_INCLUDED
#define NOTIFY_INCLUDED

#include "rule.h"
#include "particle.h"
#include "board.h"
#include "xymap.h"

typedef struct BoardWatcher BoardWatcher;

/* only one type of message for rules: the rule has just been triggered.
 */
typedef void (*RuleNotifyFunction) (BoardWatcher *watcher,
				    Board *board,
				    Particle *ruleOwner,  /* Particle that owns this rule */
				    StochasticRule *rule,
				    int xOwner,           /* coords of owner cell */
				    int yOwner,
				    int *ignored,         /* ignored[NumRuleConditions]: record of which RuleCondition's were ignored */
				    int *failed,          /* failed[NumRuleOperations]: record of which RuleOperation's failed */
				    XYMap *oldState,      /* oldState: record of pre-rule states (map from XYCoord->State) */
				    XYMap *newState);     /* newState: record of post-rule states (map from XYCoord->State) */

/* Particles can receive several types of message, always sent after the Particle is written */
typedef void (*ParticleNotifyFunction) (BoardWatcher *watcher,
					Board *board,
					Particle *ruleOwner,  /* Particle that owns this rule */
					StochasticRule *rule,
					Particle *affected,   /* Particle being affected by this rule operation */
					int xOwner,           /* coords of owner cell */
					int yOwner,
					XYMap *oldState,
					XYMap *newState);

/* a BoardWatcher is a name, a set of notify functions, and some context */
struct BoardWatcher {
  char *name;
  RuleNotifyFunction ruleTriggered;
  ParticleNotifyFunction
  particleCreated,        /* called for each affected Particle going from having 0 to N>0 instances in a rule "neighborhood" (i.e. set of destination cells) */
    particleCopied,       /* called for each affected Particle going from M>0 to N>M instances in a rule neighborhood, in a different cell, with or without change of state  */
    particleModified,     /* called for each affected Particle going from 1 to 1 instances in a rule neighborhood, in the same cell, with some change of state (in the Vars word)  */
    particleMoved,        /* called for each affected Particle going from 1 to 1 instances in a rule neighborhood, in a different cell, with or without change of state  */
    particleDestroyed;    /* called for each affected Particle going from N to M<N instances in a rule neighborhood */
  void *context;   /* pointer to miscellaneous extra context */
};

/* methods */
BoardWatcher* newBoardWatcher (char *name, RuleNotifyFunction ruleNotify, ParticleNotifyFunction particleNotify);  /* copies 'name' */
void deleteBoardWatcher (BoardWatcher* watcher);
void addRuleWatcher (StochasticRule* rule, BoardWatcher* watcher);
void addParticleWatcher (Particle* particle, BoardWatcher* watcher);
void removeRuleWatcher (StochasticRule* rule, char* name);
void removeParticleWatcher (Particle* particle, char* name);

/* null notify functions */
void NullRuleNotify (BoardWatcher *watcher, Particle *ruleOwner, StochasticRule *rule, Board *board, int x, int y, int *ignored, int *failed, XYMap *oldState);
void NullParticleNotify (BoardWatcher *watcher, Particle *affectedParticle, Board *board, int x, int y, State oldState, State newState);

#endif /* NOTIFY_INCLUDED */
