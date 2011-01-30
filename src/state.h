#ifndef STATE_INCLUDED
#define STATE_INCLUDED


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

/* read-only State's for given Type */
#define ReadOnlyStates    15
#define AddressableStates 16
#define AddressableBits   (BitsPerState * AddressableStates)

/* Messages */
typedef unsigned long long int Message;

/* Reserved states & types */
/* Empty (aka "void", "scenery black") */
#define EmptyState 0
#define EmptyType  0

#endif /* STATE_INCLUDED */
