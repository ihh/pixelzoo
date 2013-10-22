#include "rule.h"
#include "statemap.h"
#include "goal.h"

ParticleRule* newParticleRule (enum RuleType type);

ParticleRule* newParticleRule (enum RuleType type) {
  ParticleRule* rule;
  rule = SafeCalloc (1, sizeof (ParticleRule));
  rule->type = type;
  return rule;
}

ParticleRule* newLookupRule() {
  ParticleRule* rule;
  rule = newParticleRule (LookupRule);
  rule->param.lookup.matchRule = newStateMap (AbortCopyFunction, deleteParticleRule, NullPrintFunction);
  rule->param.lookup.defaultRule = NULL;
  return rule;
}

ParticleRule* newModifyRule() {
  ParticleRule* rule;
  rule = newParticleRule (ModifyRule);
  rule->param.modify.nextRule = NULL;
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

ParticleRule* newGoalRule() {
  ParticleRule* rule;
  rule = newParticleRule (GoalRule);
  rule->param.goal = NULL;
  return rule;
}

ParticleRule* newGotoRule() {
  ParticleRule* rule;
  rule = newParticleRule (GotoRule);
  rule->param.gotoLabel = NULL;
  return rule;
}

ParticleRule* newLoadRule() {
  ParticleRule* rule;
  rule = newParticleRule (LoadRule);
  rule->param.load.n = 0;
  rule->param.load.reg = NULL;
  rule->param.load.state = NULL;
  return rule;
}

void deleteParticleRule (void *voidRule) {
  ParticleRule *rule;
  LookupRuleParams *lookup;
  ModifyRuleParams *modify;
  RandomRuleParams *random;
  LoadRuleParams *load;
  Goal *goal;

  rule = (ParticleRule*) voidRule;

  switch (rule->type) {
  case LookupRule:
    lookup = &rule->param.lookup;
    deleteStateMap (lookup->matchRule);
    if (lookup->defaultRule)
      deleteParticleRule (lookup->defaultRule);
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

  case GoalRule:
    goal = rule->param.goal;
    if (goal)
      deleteGoal (goal);
    break;

  case GotoRule:
  case DeliverRule:
    break;

  case LoadRule:
    load = &rule->param.load;
    SafeFree (load->reg);
    SafeFree (load->state);
    if (load->nextRule)
      deleteParticleRule (load->nextRule);
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
