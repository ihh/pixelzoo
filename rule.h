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
   (randomDouble() <= ignoreProb)  ||  (cell[loc] & mask  <opcode>  rhs & mask)
*/
typedef struct RuleCondition {
  LocalOffset loc;
  State mask, rhs;
  enum ConditionalOpcode { EQ, NEQ, GT, LT, GEQ, LEQ, TRUE } opcode;
  double ignoreProb;
} RuleCondition;

/*
  RuleOperation describes the following operation:
  cell[dest] = (cell[dest] & (StateMask ^ (mask << left_shift))) | ((((cell[src] >> right_shift) + offset) & mask) << left_shift);
*/
typedef struct RuleOperation {
  LocalOffset src, dest;
  unsigned char right_shift, left_shift;
  State offset, mask;
} RuleOperation;

/*
  A StochasticRule consists of:
   a Type,
   a fixed number of RuleCondition's,
   a fixed number of RuleOperation's,
   a firing rate (corresponding to the relative rate that this rule will be selected out of all rules applying to this Type)
 */

/* first define the size of the condition & operation blocks */
#define NumRuleConditions 6
#define NumRuleOperations 6

/* now the struct */
typedef struct StochasticRule {
  Type id;
  RuleCondition cond[NumRuleConditions];
  RuleOperation op[NumRuleOperations];
  double prob;
} StochasticRule;

#endif /* RULE_INCLUDED */
