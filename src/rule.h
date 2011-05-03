#ifndef RULE_INCLUDED
#define RULE_INCLUDED

#include "util.h"
#include "statemap.h"

/* Short-range relative co-ordinate offset.
   1 byte each for X & Y.
   If X or Y is equal to TempOffset, then the resulting co-ordinate is not physically located the board,
   but is instead treated as a temporary variable (to facilitate swapping two cells, etc.)
 */
typedef struct LocalOffset {
  char x, y;
} LocalOffset;

#define TempOffset -128

/* RuleFunction is one of the following functions (returning an int):

  (Lookup)
  matchRule[val]  or defaultRule, if no matchRule defined,
  where val = ((cell[orig+src] & srcMask) >> rightShift)

  (Modify)
  cell[orig+dest] = (cell[orig+dest] & (StateMask ^ destMask)) | (((writeVal) << leftShift) & destMask);
  then nextRule

  (Random)
  syncRandomProb() < prob ? passRule : failRule

  (Goal)
  passes control to a goal
*/
typedef struct ParticleRule ParticleRule;

enum RuleType { LookupRule, ModifyRule, DeliverRule, RandomRule, GoalRule, GotoRule };

typedef struct LookupRuleParams {
  LocalOffset loc;
  unsigned int shift;
  State mask;
  StateMap *matchRule;
  ParticleRule *defaultRule;
} LookupRuleParams;

typedef struct ModifyRuleParams {
  LocalOffset src, dest;
  unsigned int rightShift, leftShift;
  State srcMask, destMask, offset;
  ParticleRule *nextRule;
} ModifyRuleParams;

typedef struct DeliverRuleParams {
  LocalOffset recipient;
  Message message;
} DeliverRuleParams;

typedef struct RandomRuleParams {
  int64_Millionths prob;
  ParticleRule *passRule, *failRule;
} RandomRuleParams;

typedef union RuleParams {
  LookupRuleParams lookup;
  ModifyRuleParams modify;
  DeliverRuleParams deliver;
  RandomRuleParams random;
  void *goal;
  ParticleRule *gotoLabel;
} RuleParams;

struct ParticleRule {
  enum RuleType type;
  RuleParams param;
};

/* methods */
ParticleRule* newLookupRule();
ParticleRule* newModifyRule();
ParticleRule* newDeliverRule();
ParticleRule* newRandomRule();
ParticleRule* newGoalRule();
ParticleRule* newGotoRule();

void deleteParticleRule (void *rule);

#endif /* RULE_INCLUDED */
