#ifndef RULE_INCLUDED
#define RULE_INCLUDED

#include "util.h"
#include "statemap.h"
#include "stringmap.h"

/* Global registers for virtual machine */
#define NumberOfRegisters 64

/* Short-range relative co-ordinate offset.
   1 byte each for X, Y & Z.
   If xyzAreRegisters is nonzero, X, Y & Z are treated as register indices (i.e. indirect addressing).
   The maximum register index is NumberOfRegisters-1; the index one greater than this (NumberOfRegisters) is held at zero, and is the default value for registers that are omitted.
 */
typedef struct LocalOffset {
  signed char x, y, z;
  unsigned char xyzAreRegisters;
} LocalOffset;

/* Types of rule */
typedef struct ParticleRule ParticleRule;

enum RuleType { LookupRule, CompareRule, ModifyRule, DeliverRule, RandomRule, GotoRule, LoadRule };

typedef struct LookupRuleParams {
  LocalOffset loc;
  unsigned int shift;
  State mask;
  StateMap *matchRule;
  unsigned char matchRegister, useMatchRegister;
  ParticleRule *defaultRule, *lowRule, *highRule;
} LookupRuleParams;

typedef struct CompareRuleParams {
  LocalOffset loc;
  unsigned int shift;
  State mask;
  unsigned char registerIndex;
  ParticleRule *eqRule, *ltRule, *gtRule;
} CompareRuleParams;

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
  CompareRuleParams compare;
  ModifyRuleParams modify;
  DeliverRuleParams deliver;
  RandomRuleParams random;
  LoadRuleParams load;
  ParticleRule *gotoLabel;
} RuleParams;

struct ParticleRule {
  enum RuleType type;
  RuleParams param;
  const char *label;
};

/* methods */
ParticleRule* newLookupRule();
ParticleRule* newCompareRule();
ParticleRule* newModifyRule();
ParticleRule* newDeliverRule();
ParticleRule* newRandomRule();
ParticleRule* newGotoRule();
ParticleRule* newLoadRule();

void deleteParticleRule (void *rule);

void defineSubRule (StringMap **subRuleIndex, const char* name, ParticleRule *rule, StringMap *globalSubRuleIndex);

#endif /* RULE_INCLUDED */
