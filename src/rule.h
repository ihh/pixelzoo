#ifndef RULE_INCLUDED
#define RULE_INCLUDED

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
#define StateMask    0xffffffffffffffff
#define MaxState     0xffffffffffffffff
#define BitsPerState 64

typedef signed long long int StateOffset;

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

/* RuleFunction is one of the following functions (returning an int):

  (Lookup)
  matchRule[val]  or defaultRule, if no matchRule defined,
  where val = ((cell[orig+src] & srcMask) >> rightShift)

  (Modify)
  cell[orig+dest] = (cell[orig+dest] & (StateMask ^ destMask)) | (((writeVal) << leftShift) & destMask);
  then nextRule

  (Random)
  randRule[distrib.sample()]

  (Overload)
  if board is overloaded, slowRule; else fastRule
*/
typedef struct ParticleRule ParticleRule;

enum RuleType { LookupRule, ModifyRule, RandomRule, OverloadRule };

typedef struct LookupRuleParams {
  LocalOffset loc;
  unsigned char shift;
  State mask;
  RBTree *matchRule;
  ParticleRule *defaultRule;
} LookupRule;

typedef struct ModifyRuleParams {
  LocalOffset src, dest;
  unsigned char rightShift, leftShift;
  State srcMask, destMask, offset;
  ParticleRule *nextRule;
} LookupRule;

typedef struct RandomRuleParams {
  int nRules;
  BinDist *distrib;
  ParticleRule **rule;
} LocalOffsetRules;

typedef struct OverloadRuleParams {
  ParticleRule *slowRule, *fastRule;
} LocalOffsetRules;
  
typedef union RuleParams {
  LookupRuleParams lookup;
  ModifyRuleParams modify;
  RandomRuleParams random;
  OverloadRuleParams overload;
} RuleParams;

struct ParticleRule {
  enum RuleType type;
  RuleParams param;
};

void deleteParticleRule (ParticleRule *rule);


#endif /* RULE_INCLUDED */
