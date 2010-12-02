#ifndef RULE_INCLUDED
#define RULE_INCLUDED

/* 32-bit cell state */
typedef unsigned long State;
#define StateMask 0xffffffff

/* 16-bit cell type */
typedef unsigned short Type;
#define TypeMask 0xffff

/* short-range relative co-ordinate offset */
typedef struct Coord {
  char x, y;
} Coord;

/*
  RuleCondition describes the following conditional test:
   (cell[loc] & mask  <opcode>  rhs & mask)
*/
typedef struct RuleCondition {
  Coord loc;
  State mask, rhs;
  enum ConditionalOpcode { EQ, NEQ, GT, LT, GEQ, LEQ } opcode;
} RuleCondition;

/*
  RuleOperation describes the following operation:
  cell[dest] = (cell[dest] & (StateMask ^ (mask << left_shift))) | ((((cell[src] >> right_shift) + offset) & mask) << left_shift);
*/
typedef struct RuleOperation {
  Coord src, dest;
  unsigned char right_shift, left_shift;
  State offset, mask;
} RuleOperation;

/*
  A StochasticRule consists of:
   a Type,
   a fixed number of RuleCondition's,
   a fixed number of RuleOperation's,
   a firing probability.
 */

/* first define the size of the condition & operation blocks */
#define NumRuleConditions 6
#define NumRuleOperations 6

/* now the struct */
typedef struct StochasticRule {
  Type id;
  RuleCondition[NumRuleConditions] cond;
  RuleOperation[NumRuleOperations] op;
  double prob;
} StochasticRule;

#endif /* RULE_INCLUDED */
