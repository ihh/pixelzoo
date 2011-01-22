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

ParticleRule* newRandomRule() {
  ParticleRule* rule;
  rule = newParticleRule (RandomRule);
  rule->param.random.passRule = NULL;
  rule->param.random.failRule = NULL;
  return rule;
}

ParticleRule* newOverloadRule() {
  ParticleRule* rule;
  rule = newParticleRule (OverloadRule);
  rule->param.overload.slowRule = NULL;
  rule->param.overload.fastRule = NULL;
  return rule;
}

ParticleRule* newGoalRule() {
  ParticleRule* rule;
  rule = newParticleRule (LookupRule);
  rule->param.goal = NULL;
  return rule;
}

void deleteParticleRule (void *voidRule) {
  ParticleRule *rule;
  LookupRuleParams *lookup;
  ModifyRuleParams *modify;
  RandomRuleParams *random;
  OverloadRuleParams *overload;
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

  case OverloadRule:
    overload = &rule->param.overload;
    if (overload->slowRule)
      deleteParticleRule (overload->slowRule);
    if (overload->fastRule)
      deleteParticleRule (overload->fastRule);
    break;

  case GoalRule:
    goal = rule->param.goal;
    if (goal)
      deleteGoal (goal);
    break;

  default:
    Abort ("Unknown rule type");
    break;
  }
  SafeFree (rule);
}
