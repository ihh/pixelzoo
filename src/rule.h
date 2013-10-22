#ifndef RULE_INCLUDED
#define RULE_INCLUDED

#include "util.h"
#include "statemap.h"
#include "stringmap.h"

/* Global registers for virtual machine */
#define NumberOfRegisters 64

/* Short-range relative co-ordinate offset.
   1 byte each for X & Y.
   If xyAreRegisters is nonzero, X & Y are treated as register indices (i.e. indirect addressing).
 */
typedef struct LocalOffset {
  signed char x, y;
  unsigned char xyAreRegisters;
} LocalOffset;

/* Types of rule */
typedef struct ParticleRule ParticleRule;

enum RuleType { LookupRule, ModifyRule, DeliverRule, RandomRule, GoalRule, GotoRule, LoadRule };

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
  unsigned char offsetIsRegister;  /* if nonzero, offset is treated as a register index */
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

typedef struct LoadRuleParams {
  int n;
  unsigned char *reg;
  State *state;
  ParticleRule *nextRule;
} LoadRuleParams;

typedef union RuleParams {
  LookupRuleParams lookup;
  ModifyRuleParams modify;
  DeliverRuleParams deliver;
  RandomRuleParams random;
  LoadRuleParams load;
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
ParticleRule* newLoadRule();

void deleteParticleRule (void *rule);

void defineSubRule (StringMap **subRuleIndex, const char* name, ParticleRule *rule, StringMap *globalSubRuleIndex);

#endif /* RULE_INCLUDED */
