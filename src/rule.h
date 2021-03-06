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

/* Neighborhood */
typedef struct Neighborhood {
  unsigned int size;
  signed char *x, *y, *z;
} Neighborhood;

/* Types of rule */
typedef struct ParticleRule ParticleRule;

enum RuleType { LookupRule, CompareRule, ModifyRule, DeliverRule, RandomRule, GotoRule, LoadRule, VectorRule, NeighborRule,FunctionRule };
enum ModifyRuleType { ConserveModify, KillModify, EatModify };

typedef struct LookupRuleParams {  /* NB LookupRule can also be used to read a var into a register */
  LocalOffset loc;
  unsigned int shift;
  State mask;
  StateMap *matchRule;
  unsigned char matchRegister;  /* if matchRegister is a valid register index, var value is placed in that register */
  ParticleRule *defaultRule, *lowRule, *highRule;
} LookupRuleParams;

typedef struct CompareRuleParams {
  LocalOffset loc;
  unsigned int shift;
  State mask;
  unsigned char registerIndex;
  ParticleRule *eqRule, *ltRule, *gtRule;
} CompareRuleParams;

typedef struct ModifyRuleParams {  /* NB ModifyRule can also be used to write a register to a var */
  LocalOffset src, dest;
  unsigned int rightShift, leftShift;
  State srcMask, destMask, offset;
  unsigned char offsetIsRegister;  /* if nonzero, offset is treated as a register index */
  enum ModifyRuleType modifyType;
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
  int *val;
  ParticleRule *nextRule;
} LoadRuleParams;

typedef struct VectorRuleParams {
  int n;
  unsigned char *nbr;
  unsigned char x, y, z, dir, inv;
  ParticleRule *nextRule;
} VectorRuleParams;

typedef struct NeighborRuleParams {
  int n;
  unsigned char *nbr;
  unsigned char dir;
  ParticleRule *nextRule;
} NeighborRuleParams;

typedef struct FunctionRuleParams {
  char *schemeExpr;
  ParticleRule *passRule, *failRule;
} FunctionRuleParams;

typedef union RuleParams {
  LookupRuleParams lookup;
  CompareRuleParams compare;
  ModifyRuleParams modify;
  DeliverRuleParams deliver;
  RandomRuleParams random;
  LoadRuleParams load;
  VectorRuleParams vector;
  NeighborRuleParams neighbor;
  FunctionRuleParams function;
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
ParticleRule* newVectorRule();
ParticleRule* newNeighborRule();
ParticleRule* newFunctionRule();

ParticleRule* newGotoRuleTo(ParticleRule* label);

void deleteParticleRule (void *rule);

void defineSubRule (StringMap **subRuleIndex, const char* name, ParticleRule *rule, StringMap *globalSubRuleIndex);

Neighborhood* newNeighborhood (int size);
void deleteNeighborhood (Neighborhood* hood);

int getNeighborIndex (Neighborhood* hood, signed char x, signed char y, signed char z);

#endif /* RULE_INCLUDED */
