#ifndef RULE_INCLUDED
#define RULE_INCLUDED

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
  randomDouble() < prob ? passRule : failRule

  (Overload)
  if board is overloaded, slowRule; else fastRule

  (Goal)
  passes control to a goal
*/
typedef struct ParticleRule ParticleRule;

enum RuleType { LookupRule, ModifyRule, RandomRule, OverloadRule, GoalRule };

typedef struct LookupRuleParams {
  LocalOffset loc;
  unsigned char shift;
  State mask;
  StateMap *matchRule;
  ParticleRule *defaultRule;
} LookupRule;

typedef struct ModifyRuleParams {
  LocalOffset src, dest;
  unsigned char rightShift, leftShift;
  State srcMask, destMask, offset;
  ParticleRule *nextRule;
} LookupRule;

typedef struct RandomRuleParams {
  double prob;
  ParticleRule *passRule, *failRule;
} LocalOffsetRules;

typedef struct OverloadRuleParams {
  ParticleRule *slowRule, *fastRule;
} LocalOffsetRules;
  
typedef union RuleParams {
  LookupRuleParams lookup;
  ModifyRuleParams modify;
  RandomRuleParams random;
  OverloadRuleParams overload;
  Goal *goal;
} RuleParams;

struct ParticleRule {
  enum RuleType type;
  RuleParams param;
};

/* methods */
ParticleRule* newLookupRule();
ParticleRule* newModifyRule();
ParticleRule* newRandomRule();
ParticleRule* newOverloadRule();
ParticleRule* newGoalRule();

void deleteParticleRule (void *rule);

#endif /* RULE_INCLUDED */
