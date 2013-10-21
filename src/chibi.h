#ifndef CHIBI_INCLUDED
#define CHIBI_INCLUDED

#include "chibi/sexp.h"
#include "chibi/eval.h"

/* Chibi Scheme helpers */
const char* sexp_string_data_wrapper (sexp x);
const char* sexp_string (sexp ctx, sexp x);


#endif /* CHIBI_INCLUDED */
