#ifndef RULE_INCLUDED
#define RULE_INCLUDED

#include "stringmap.h"

/* 32-bit cell state.

   The 16-bit MSW (most significant word) of the state is the Type.
   The 16-bit LSW (least significant word) provides additional type-specific state.

   All states of type zero (i.e. MSW=0) are inert, i.e. they have zero update rate.
   State 0x00000000, where LSW=MSW=0, is inert black space.
   States with MSW=0 and 0<=LSW<=PaletteMax are inert pixels spanning the color palette.
   States with MSW=0 and LSW>PaletteMax are reserved for future applications.

   A common convention is that the state with LSW=0 is "prototypical" for that MSW,
   but this convention is not strictly enforced or required.
 */
typedef unsigned long long int State;
#define StateMask 0xffffffffffffffff
#define BitsPerState 64

/* 16-bit cell type */
typedef unsigned short int Type;
#define TypeMask    0xffff000000000000
#define TypeShift   48
#define NumTypes    0x10000
#define MaxType     0xffff
#define BitsPerType 16

/* 16-bit type-specific "variables" */
typedef unsigned long long int Vars;
#define VarMask     0x0000ffffffffffff
#define NumVars     0x1000000000000
#define BitsPerVar  48

/* type <-> state conversion macros */
#define StateType(STATE) (((STATE) & TypeMask) >> TypeShift)

/* Reserved states & types */
/* Empty (aka "void", "scenery black") */
#define EmptyState 0
#define EmptyType  0

/* Short-range relative co-ordinate offset
   1 byte each for X & Y
 */
typedef struct LocalOffset {
  char x, y;
} LocalOffset;

/*
  RuleCondition parameterizes the following conditional test:
   (randomDouble() < ignoreProb)  ||  (cell[loc] & mask  <opcode>  rhs)
*/
typedef struct RuleCondition {
  LocalOffset loc;
  State mask, rhs;
  enum ConditionalOpcode { TestEQ, TestNEQ, TestGT, TestLT, TestGEQ, TestLEQ, TestTRUE, TestFALSE } opcode;
  double ignoreProb, overloadIgnoreProb;  /* when board (or local region) is overloaded, overloadIgnoreProb will be used instead of ignoreProb */
} RuleCondition;

/*
  RuleOperation parameterizes the following operation:
  if (randomDouble() >= failProb)
    cell[dest] = (cell[dest] & (StateMask ^ (mask << leftShift))) | (((((cell[src] & preMask) >> rightShift) + offset) & mask) << leftShift);
*/
typedef struct RuleOperation {
  LocalOffset src, dest;
  unsigned char rightShift, leftShift;
  State offset, mask, preMask;
  double failProb, overloadFailProb;  /* when board (or local region) is overloaded, overloadFailProb will be used instead of failProb */
} RuleOperation;

/*
  A StochasticRule (associated with a given Particle) consists of:
   a fixed number of RuleCondition's,
   a fixed number of RuleOperation's,
   a firing rate (corresponding to the relative rate that this rule will be selected out of all rules applying to this Type)
   an overloaded firing rate (the firing rate that will be used when the board is too full)
 */

/* first define the size of the condition & operation blocks
   These sizes were chosen with reference to the following models:
   (async, NumRuleConditions=6, NumRuleOperations=6)   RNA duplex diffusion with four connected neighbors per basepair unit
   (sync,  NumRuleConditions=8, NumRuleOperations=1)   Conway's Life
 */
#define NumRuleConditions 8  /* minimum for Conway's Life */
#define NumRuleOperations 6  /* minimum for RNA diffusion model */

/* now the rule struct itself */
typedef struct StochasticRule {
  RuleCondition cond[NumRuleConditions];
  RuleOperation op[NumRuleOperations];
  double rate, overloadRate;
  /* the following indices are only important if write operations are being buffered for a synchronous update */
  unsigned char cumulativeOpSrcIndex[NumRuleOperations];  /* if cumulativeOpSrcIndex[n]=m and m>0, then rule #n uses as its "src" the "dest" value of rule #m-n */
  unsigned char cumulativeOpDestIndex[NumRuleOperations];  /* if cumulativeOpDestIndex[n]=m and m>0, then rule #n uses as its unmasked "dest" the "dest" value of rule #m-n */
} StochasticRule;

#endif /* RULE_INCLUDED */
