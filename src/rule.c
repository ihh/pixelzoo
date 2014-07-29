#include "rule.h"
#include "statemap.h"

ParticleRule* newParticleRule (enum RuleType type);

ParticleRule* newParticleRule (enum RuleType type) {
  ParticleRule* rule;
  rule = SafeCalloc (1, sizeof (ParticleRule));
  rule->type = type;
  rule->label = NULL;
  return rule;
}

ParticleRule* newLookupRule() {
  ParticleRule* rule;
  rule = newParticleRule (LookupRule);
  rule->param.lookup.matchRule = newStateMap (AbortCopyFunction, deleteParticleRule, NullPrintFunction);
  rule->param.lookup.defaultRule = NULL;
  rule->param.lookup.lowRule = NULL;
  rule->param.lookup.highRule = NULL;
  return rule;
}

ParticleRule* newCompareRule() {
  ParticleRule* rule;
  rule = newParticleRule (CompareRule);
  rule->param.compare.registerIndex = 0;
  rule->param.compare.eqRule = NULL;
  rule->param.compare.ltRule = NULL;
  rule->param.compare.gtRule = NULL;
  return rule;
}

ParticleRule* newModifyRule() {
  ParticleRule* rule;
  rule = newParticleRule (ModifyRule);
  rule->param.modify.nextRule = NULL;
  rule->param.modify.modifyType = ConserveModify;
  return rule;
}

ParticleRule* newDeliverRule() {
  ParticleRule* rule;
  rule = newParticleRule (DeliverRule);
  return rule;
}

ParticleRule* newRandomRule() {
  ParticleRule* rule;
  rule = newParticleRule (RandomRule);
  rule->param.random.passRule = NULL;
  rule->param.random.failRule = NULL;
  return rule;
}

ParticleRule* newGotoRule() {
  ParticleRule* rule;
  rule = newParticleRule (GotoRule);
  rule->param.gotoLabel = NULL;
  return rule;
}

ParticleRule* newGotoRuleTo(ParticleRule* label) {
  ParticleRule* rule;
  rule = newGotoRule();
  rule->param.gotoLabel = label;
  return rule;
}

ParticleRule* newLoadRule() {
  ParticleRule* rule;
  rule = newParticleRule (LoadRule);
  rule->param.load.n = 0;
  rule->param.load.reg = NULL;
  rule->param.load.val = NULL;
  rule->param.load.nextRule = NULL;
  return rule;
}

ParticleRule* newVectorRule() {
  ParticleRule* rule;
  rule = newParticleRule (VectorRule);
  rule->param.vector.n = 0;
  rule->param.vector.nbr = NULL;
  rule->param.vector.x = NumberOfRegisters;
  rule->param.vector.y = NumberOfRegisters;
  rule->param.vector.z = NumberOfRegisters;
  rule->param.vector.dir = NumberOfRegisters;
  rule->param.vector.inv = NumberOfRegisters;
  rule->param.vector.nextRule = NULL;
  return rule;
}

ParticleRule* newNeighborRule() {
  ParticleRule* rule;
  rule = newParticleRule (NeighborRule);
  rule->param.neighbor.n = 0;
  rule->param.neighbor.nbr = NULL;
  rule->param.neighbor.dir = NumberOfRegisters;
  rule->param.neighbor.nextRule = NULL;
  return rule;
}

ParticleRule* newFunctionRule() {
  ParticleRule* rule;
  rule = newParticleRule (FunctionRule);
  rule->param.function.schemeExpr = NULL;
  rule->param.function.passRule = NULL;
  rule->param.function.failRule = NULL;
  return rule;
}

void deleteParticleRule (void *voidRule) {
  ParticleRule *rule;
  LookupRuleParams *lookup;
  CompareRuleParams *compare;
  ModifyRuleParams *modify;
  RandomRuleParams *random;
  LoadRuleParams *load;
  VectorRuleParams *vector;
  NeighborRuleParams *neighbor;
  FunctionRuleParams *function;

  rule = (ParticleRule*) voidRule;
  SafeFreeOrNull ((void*) rule->label);

  switch (rule->type) {
  case LookupRule:
    lookup = &rule->param.lookup;
    deleteStateMap (lookup->matchRule);
    if (lookup->defaultRule)
      deleteParticleRule (lookup->defaultRule);
    if (lookup->lowRule)
      deleteParticleRule (lookup->lowRule);
    if (lookup->highRule)
      deleteParticleRule (lookup->highRule);
    break;

  case CompareRule:
    compare = &rule->param.compare;
    if (compare->eqRule)
      deleteParticleRule (compare->eqRule);
    if (compare->ltRule)
      deleteParticleRule (compare->ltRule);
    if (compare->gtRule)
      deleteParticleRule (compare->gtRule);
    break;

  case ModifyRule:
    modify = &rule->param.modify;
    if (modify->nextRule)
      deleteParticleRule (modify->nextRule);
    break;

  case RandomRule:
    random = &rule->param.random;
    if (random->passRule)
      deleteParticleRule (random->passRule);
    if (random->failRule)
      deleteParticleRule (random->failRule);
    break;

  case GotoRule:
  case DeliverRule:
    break;

  case LoadRule:
    load = &rule->param.load;
    SafeFree (load->reg);
    SafeFree (load->val);
    if (load->nextRule)
      deleteParticleRule (load->nextRule);
    break;

  case VectorRule:
    vector = &rule->param.vector;
    SafeFree (vector->nbr);
    if (vector->nextRule)
      deleteParticleRule (vector->nextRule);
    break;

  case NeighborRule:
    neighbor = &rule->param.neighbor;
    SafeFree (neighbor->nbr);
    if (neighbor->nextRule)
      deleteParticleRule (neighbor->nextRule);
    break;

  case FunctionRule:
    function = &rule->param.function;
    SafeFreeOrNull (function->schemeExpr);
    if (function->passRule)
      deleteParticleRule (function->passRule);
    if (function->failRule)
      deleteParticleRule (function->failRule);
    break;

  default:
    Abort ("Unknown rule type");
    break;
  }
  SafeFree (rule);
}

void defineSubRule (StringMap **subRuleIndex, const char* name, ParticleRule *rule, StringMap *globalSubRuleIndex) {
  if (*subRuleIndex == NULL)
    *subRuleIndex = newStringMap (AbortCopyFunction, deleteParticleRule, NullPrintFunction);
  else if (StringMapFind (*subRuleIndex, name))
    Warn ("Re-definition of subrule name %s", name);
  if (globalSubRuleIndex && StringMapFind (globalSubRuleIndex, name))
    Warn ("Local subrule name %s masks definition for pseudonymous global subrule", name);
  (void) StringMapInsert (*subRuleIndex, name, rule);
}

Neighborhood* newNeighborhood (int size) {
  Neighborhood* hood;
  hood = SafeCalloc (1, sizeof(Neighborhood));
  hood->size = size;
  hood->x = SafeCalloc (size, sizeof(signed char));
  hood->y = SafeCalloc (size, sizeof(signed char));
  hood->z = SafeCalloc (size, sizeof(signed char));
  return hood;
}

void deleteNeighborhood (Neighborhood* hood) {
  SafeFree (hood->x);
  SafeFree (hood->y);
  SafeFree (hood->z);
  SafeFree (hood);
}

int getNeighborIndex (Neighborhood* hood, signed char x, signed char y, signed char z) {
  int n;
  for (n = 0; n < hood->size; ++n)
    if (hood->x[n] == x && hood->y[n] == y && hood->z[n] == z)
      return n;
  return -1;
}

