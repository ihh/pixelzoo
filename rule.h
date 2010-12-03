#ifndef RULE_INCLUDED
#define RULE_INCLUDED

/* 32-bit cell state */
typedef unsigned long State;
#define StateMask 0xffffffff

/* 16-bit cell type */
typedef unsigned short Type;
#define TypeMask 0xffff
#define NumTypes 0x10000
#define MaxType  0xffff

/* short-range relative co-ordinate offset */
typedef struct LocalOffset {
  char x, y;
} LocalOffset;

/*
  RuleCondition describes the following conditional test:
   (randomDouble() < ignoreProb)  ||  (cell[loc] & mask  <opcode>  rhs)
*/
typedef struct RuleCondition {
  LocalOffset loc;
  State mask, rhs;
  enum ConditionalOpcode { EQ, NEQ, GT, LT, GEQ, LEQ, TRUE, FALSE } opcode;
  double ignoreProb;
} RuleCondition;

/*
  RuleOperation describes the following operation:
  if (randomDouble() >= failProb) {  cell[dest] = (cell[dest] & (StateMask ^ (mask << leftShift))) | ((((cell[src] >> rightShift) + offset) & mask) << leftShift);  }
*/
typedef struct RuleOperation {
  LocalOffset src, dest;
  unsigned char rightShift, leftShift;
  State offset, mask;
  double failProb;
} RuleOperation;

/*
  A StochasticRule (associated with a given Particle) consists of:
   a fixed number of RuleCondition's,
   a fixed number of RuleOperation's,
   a firing rate (corresponding to the relative rate that this rule will be selected out of all rules applying to this Type)
   an overloaded firing rate (the firing rate that will be used when the board is too full)
 */

/* first define the size of the condition & operation blocks */
#define NumRuleConditions 6
#define NumRuleOperations 6

/* now the struct */
typedef struct StochasticRule {
  RuleCondition cond[NumRuleConditions];
  RuleOperation op[NumRuleOperations];
  double rate, overloadRate;
} StochasticRule;

#endif /* RULE_INCLUDED */
