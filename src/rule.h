#ifndef RULE_INCLUDED
#define RULE_INCLUDED

#include "stringmap.h"

/* State: 64-bit cell state.

   The 16 most significant bits of the state form the Type.
   The 48 bits beneath this are the Vars, providing additional type-specific state.

   All states with Type=0 are inert, i.e. they have zero update rate:
    State 0x0000000000000000 is inert black space.
    States 0 <= S <= PaletteMax are inert pixels spanning the color palette.
    States PaletteMax < S < 0x0001000000000000 are reserved for future applications
     (the upper bound being the first state with Type=1).

   A common convention is that the state with Vars=0 is "prototypical" for that Type,
   but this convention is not strictly enforced or required.
 */
typedef unsigned long long int State;
typedef signed long long int StateOffset;
#define StateMask    0xffffffffffffffff
#define MaxState     0xffffffffffffffff
#define BitsPerState 64

/* Type: 16-bit cell type */
typedef unsigned short int Type;
#define TypeMask     0xffff000000000000
#define BitsPerType  16
#define MaxType      0xffff
#define NumTypes     0x10000
#define TypeShift    48

/* Vars: 48-bit type-specific variables */
typedef unsigned long long int Vars;
#define VarsMask     0x0000ffffffffffff
#define BitsPerVars  48
#define NumVars      0x1000000000000

/* Type <-> State conversion macros */
#define StateType(STATE) (((STATE) & TypeMask) >> TypeShift)

/* Reserved states & types */
/* Empty (aka "void", "scenery black") */
#define EmptyState 0
#define EmptyType  0

/* Short-range relative co-ordinate offset.
   1 byte each for X & Y.
   If X or Y is equal to TempOffset, then the resulting co-ordinate is not physically located the board,
   but is instead treated as a temporary variable (to facilitate swapping two cells, etc.)
 */
typedef struct LocalOffset {
  char x, y;
} LocalOffset;

#define TempOffset -128

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
    cell[dest] = (cell[dest] & (StateMask ^ destMask)) | (((((cell[src] & srcMask) >> rightShift) + offset) << leftShift) & destMask);
*/
typedef struct RuleOperation {
  LocalOffset src, dest;
  unsigned char rightShift, leftShift;
  State offset, srcMask, destMask;
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
  /* the following indices are important if write operations are being buffered for a synchronous update, and if "src" or "dest" uses TempOffset */
  unsigned char cumulativeOpSrcIndex[NumRuleOperations];  /* if cumulativeOpSrcIndex[n]=m and m>0, then rule #n uses as its "src" the "dest" value of rule #m-n */
  unsigned char cumulativeOpDestIndex[NumRuleOperations];  /* if cumulativeOpDestIndex[n]=m and m>0, then rule #n uses as its unmasked "dest" the "dest" value of rule #m-n */
  unsigned char writeOp[NumRuleOperations];  /* if writeOp[n]=0, then result of RuleOperation #n need not be written to the board (either because it's a temporary variable, or because it's rewritten by a later rule) */
} StochasticRule;

#endif /* RULE_INCLUDED */
