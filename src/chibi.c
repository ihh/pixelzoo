#include "chibi.h"
#include "board.h"

/* simple wrappers */
const char* sexp_string_data_wrapper (sexp x) {
  return sexp_string_data(x);
}

const char* sexp_string (sexp ctx, sexp x) {
  const char* cstr;
  sexp_gc_var1(str);
  sexp_gc_preserve1(ctx,str);
  str = sexp_write_to_string (ctx, x);
  cstr = StringNew (sexp_string_data (str));
  sexp_gc_release1(ctx);
  return cstr;
}


/* Board Scheme API
   Hacked together by doing this...
     $ chibi/tools/chibi-ffi chibi/board.stub
   ...then copying-and-pasting
 */

static sexp sexp_Board_random_stub (sexp ctx, sexp self, sexp_sint_t n, sexp arg0) {
  sexp res;
  if (! (sexp_pointerp(arg0) && (sexp_pointer_tag(arg0) == sexp_unbox_fixnum(sexp_opcode_arg1_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg1_type(self)), arg0);
  res = sexp_make_unsigned_integer(ctx, boardRandomInt32((struct Board*)sexp_cpointer_value(arg0)));
  return res;
}

static sexp sexp_Board_set_meta_stub (sexp ctx, sexp self, sexp_sint_t n, sexp arg0, sexp arg1, sexp arg2, sexp arg3, sexp arg4) {
  sexp res;
  if (! (sexp_pointerp(arg0) && (sexp_pointer_tag(arg0) == sexp_unbox_fixnum(sexp_opcode_arg1_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg1_type(self)), arg0);
  if (! sexp_exact_integerp(arg1))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, arg1);
  if (! sexp_exact_integerp(arg2))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, arg2);
  if (! sexp_exact_integerp(arg3))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, arg3);
  if (! sexp_stringp(arg4))
    return sexp_type_exception(ctx, self, SEXP_STRING, arg4);
  res = ((writeBoardMeta((struct Board*)sexp_cpointer_value(arg0), sexp_sint_value(arg1), sexp_sint_value(arg2), sexp_sint_value(arg3), sexp_string_data(arg4))), SEXP_VOID);
  return res;
}

static sexp sexp_Board_get_meta_stub (sexp ctx, sexp self, sexp_sint_t n, sexp arg0, sexp arg1, sexp arg2, sexp arg3) {
  sexp res;
  if (! (sexp_pointerp(arg0) && (sexp_pointer_tag(arg0) == sexp_unbox_fixnum(sexp_opcode_arg1_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg1_type(self)), arg0);
  if (! sexp_exact_integerp(arg1))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, arg1);
  if (! sexp_exact_integerp(arg2))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, arg2);
  if (! sexp_exact_integerp(arg3))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, arg3);
  res = sexp_c_string(ctx, readBoardMeta((struct Board*)sexp_cpointer_value(arg0), sexp_sint_value(arg1), sexp_sint_value(arg2), sexp_sint_value(arg3)), -1);
  return res;
}

static sexp sexp_Board_set_state_stub (sexp ctx, sexp self, sexp_sint_t n, sexp arg0, sexp arg1, sexp arg2, sexp arg3, sexp arg4) {
  sexp res;
  if (! (sexp_pointerp(arg0) && (sexp_pointer_tag(arg0) == sexp_unbox_fixnum(sexp_opcode_arg1_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg1_type(self)), arg0);
  if (! sexp_exact_integerp(arg1))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, arg1);
  if (! sexp_exact_integerp(arg2))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, arg2);
  if (! sexp_exact_integerp(arg3))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, arg3);
  if (! sexp_exact_integerp(arg4))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, arg4);
  res = ((writeBoardStateFunction((struct Board*)sexp_cpointer_value(arg0), sexp_sint_value(arg1), sexp_sint_value(arg2), sexp_sint_value(arg3), sexp_uint_value(arg4))), SEXP_VOID);
  return res;
}

static sexp sexp_Board_get_state_stub (sexp ctx, sexp self, sexp_sint_t n, sexp arg0, sexp arg1, sexp arg2, sexp arg3) {
  sexp res;

  // The following test fails & eventually segfaults
  if (! (sexp_pointerp(arg0) && (sexp_pointer_tag(arg0) == sexp_unbox_fixnum(sexp_opcode_arg1_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg1_type(self)), arg0);

  if (! sexp_exact_integerp(arg1))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, arg1);
  if (! sexp_exact_integerp(arg2))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, arg2);
  if (! sexp_exact_integerp(arg3))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, arg3);
  res = sexp_make_unsigned_integer(ctx, readBoardStateFunction((struct Board*)sexp_cpointer_value(arg0), sexp_sint_value(arg1), sexp_sint_value(arg2), sexp_sint_value(arg3)));
  return res;
}

static sexp sexp_Board_new_stub (sexp ctx, sexp self, sexp_sint_t n, sexp arg0, sexp arg1) {
  sexp res;
  if (! sexp_exact_integerp(arg0))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, arg0);
  if (! sexp_exact_integerp(arg1))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, arg1);
  res = sexp_make_cpointer(ctx, sexp_unbox_fixnum(sexp_opcode_return_type(self)), newBoard(sexp_sint_value(arg0), sexp_sint_value(arg1)), SEXP_FALSE, 0);
  return res;
}

static sexp sexp_deleteBoard_stub (sexp ctx, sexp self, sexp_sint_t n, sexp b) {
  if (sexp_cpointer_freep(b))
    deleteBoard(sexp_cpointer_value(b));
  return SEXP_VOID;
}

static sexp sexp_Board_get_size (sexp ctx, sexp self, sexp_sint_t n, sexp b) {
  if (! (sexp_pointerp(b) && (sexp_pointer_tag(b) == sexp_unbox_fixnum(sexp_opcode_arg1_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg1_type(self)), b);
  return sexp_make_integer(ctx, ((struct Board*)sexp_cpointer_value(b))->size);
}

static sexp sexp_Board_get_depth (sexp ctx, sexp self, sexp_sint_t n, sexp b) {
  if (! (sexp_pointerp(b) && (sexp_pointer_tag(b) == sexp_unbox_fixnum(sexp_opcode_arg1_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg1_type(self)), b);
  return sexp_make_integer(ctx, ((struct Board*)sexp_cpointer_value(b))->depth);
}

void sexp_init_lib_board (sexp ctx, sexp env) {
  sexp_gc_var4(name, tmp, op, sexp_Board_type_tag);
  sexp_gc_preserve4(ctx, name, tmp, op, sexp_Board_type_tag);

  name = sexp_c_string(ctx, "Board", -1);
  sexp_Board_type_tag = sexp_register_c_type(ctx, name, sexp_deleteBoard_stub);

  /* Commented out for now, because type predicate doesn't seem to work:
  tmp = sexp_make_type_predicate(ctx, name, sexp_Board_type_tag);
  name = sexp_intern(ctx, "Board?", 6);
  sexp_env_define(ctx, env, name, tmp);
  */

  op = sexp_define_foreign(ctx, env, "Board-depth", 1, sexp_Board_get_depth);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_Board_type_tag));
  }

  op = sexp_define_foreign(ctx, env, "Board-size", 1, sexp_Board_get_size);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_Board_type_tag));
  }

  op = sexp_define_foreign(ctx, env, "Board-random", 1, sexp_Board_random_stub);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_Board_type_tag));
  }

  op = sexp_define_foreign(ctx, env, "Board-set-meta", 5, sexp_Board_set_meta_stub);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = SEXP_VOID;
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_Board_type_tag));
    sexp_opcode_arg2_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg3_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_argn_type(op) = sexp_make_vector(ctx, sexp_make_fixnum(2), sexp_make_fixnum(SEXP_OBJECT));
    sexp_vector_set(sexp_opcode_argn_type(op), sexp_make_fixnum(0), sexp_make_fixnum(SEXP_FIXNUM));
    sexp_vector_set(sexp_opcode_argn_type(op), sexp_make_fixnum(1), sexp_make_fixnum(SEXP_STRING));
  }

  op = sexp_define_foreign(ctx, env, "Board-get-meta", 4, sexp_Board_get_meta_stub);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_STRING);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_Board_type_tag));
    sexp_opcode_arg2_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg3_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_argn_type(op) = sexp_make_vector(ctx, sexp_make_fixnum(1), sexp_make_fixnum(SEXP_OBJECT));
    sexp_vector_set(sexp_opcode_argn_type(op), sexp_make_fixnum(0), sexp_make_fixnum(SEXP_FIXNUM));
  }

  op = sexp_define_foreign(ctx, env, "Board-set-state", 5, sexp_Board_set_state_stub);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = SEXP_VOID;
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_Board_type_tag));
    sexp_opcode_arg2_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg3_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_argn_type(op) = sexp_make_vector(ctx, sexp_make_fixnum(2), sexp_make_fixnum(SEXP_OBJECT));
    sexp_vector_set(sexp_opcode_argn_type(op), sexp_make_fixnum(0), sexp_make_fixnum(SEXP_FIXNUM));
    sexp_vector_set(sexp_opcode_argn_type(op), sexp_make_fixnum(1), sexp_make_fixnum(SEXP_FIXNUM));
  }

  op = sexp_define_foreign(ctx, env, "Board-get-state", 4, sexp_Board_get_state_stub);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_Board_type_tag));
    sexp_opcode_arg2_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg3_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_argn_type(op) = sexp_make_vector(ctx, sexp_make_fixnum(1), sexp_make_fixnum(SEXP_OBJECT));
    sexp_vector_set(sexp_opcode_argn_type(op), sexp_make_fixnum(0), sexp_make_fixnum(SEXP_FIXNUM));
  }

  op = sexp_define_foreign(ctx, env, "Board-new", 2, sexp_Board_new_stub);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_Board_type_tag));
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg2_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
  }

  sexp_gc_release4(ctx);
}

