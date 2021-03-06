/* automatically generated by chibi genstubs */

#include <chibi/eval.h>

#include <unistd.h>

#include <pwd.h>

#include <sys/types.h>

static sexp sexp_getpwnam_r_stub (sexp ctx, sexp self, sexp_sint_t n, sexp arg0) {
  int err;
  struct passwd* tmp1;
  char tmp2[1024];
  struct passwd* *tmp4;
  sexp_gc_var4(res, res1, res2, res4);
  if (! sexp_stringp(arg0))
    return sexp_type_exception(ctx, self, SEXP_STRING, arg0);
  sexp_gc_preserve4(ctx, res, res1, res2, res4);
  tmp1 = calloc(1, 1 + sizeof(tmp1[0]));
  tmp4 = calloc(1, 1 + sizeof(tmp4[0]));
  err = getpwnam_r(sexp_string_data(arg0), tmp1, tmp2, 1024, tmp4);
  if (err) {
  res = SEXP_FALSE;
  } else {
  res1 = sexp_make_cpointer(ctx, sexp_unbox_fixnum(sexp_opcode_arg2_type(self)), tmp1, SEXP_FALSE, 1);
  res2 = sexp_c_string(ctx, tmp2, -1);
  res4 = sexp_make_cpointer(ctx, sexp_unbox_fixnum(sexp_vector_ref(sexp_opcode_argn_type(self), sexp_make_fixnum(1))), tmp4, SEXP_FALSE, 1);
  res = SEXP_NULL;
  sexp_push(ctx, res, res4);
  sexp_push(ctx, res, res2);
  sexp_push(ctx, res, res1);
  }
  sexp_gc_release4(ctx);
  return res;
}

static sexp sexp_getpwuid_r_stub (sexp ctx, sexp self, sexp_sint_t n, sexp arg0) {
  int err;
  struct passwd* tmp1;
  char tmp2[1024];
  struct passwd* *tmp4;
  sexp_gc_var4(res, res1, res2, res4);
  if (! sexp_exact_integerp(arg0))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, arg0);
  sexp_gc_preserve4(ctx, res, res1, res2, res4);
  tmp1 = calloc(1, 1 + sizeof(tmp1[0]));
  tmp4 = calloc(1, 1 + sizeof(tmp4[0]));
  err = getpwuid_r(sexp_uint_value(arg0), tmp1, tmp2, 1024, tmp4);
  if (err) {
  res = SEXP_FALSE;
  } else {
  res1 = sexp_make_cpointer(ctx, sexp_unbox_fixnum(sexp_opcode_arg2_type(self)), tmp1, SEXP_FALSE, 1);
  res2 = sexp_c_string(ctx, tmp2, -1);
  res4 = sexp_make_cpointer(ctx, sexp_unbox_fixnum(sexp_vector_ref(sexp_opcode_argn_type(self), sexp_make_fixnum(1))), tmp4, SEXP_FALSE, 1);
  res = SEXP_NULL;
  sexp_push(ctx, res, res4);
  sexp_push(ctx, res, res2);
  sexp_push(ctx, res, res1);
  }
  sexp_gc_release4(ctx);
  return res;
}

static sexp sexp_set_root_directory_x_stub (sexp ctx, sexp self, sexp_sint_t n, sexp arg0) {
  int err;
  sexp res;
  if (! sexp_stringp(arg0))
    return sexp_type_exception(ctx, self, SEXP_STRING, arg0);
  err = chroot(sexp_string_data(arg0));
  if (err) {
  res = SEXP_FALSE;
  } else {
  res = SEXP_TRUE;
  }
  return res;
}

static sexp sexp_create_session_stub (sexp ctx, sexp self, sexp_sint_t n) {
  sexp res;
  res = sexp_make_unsigned_integer(ctx, setsid());
  return res;
}

static sexp sexp_current_session_id_stub (sexp ctx, sexp self, sexp_sint_t n, sexp arg0) {
  sexp res;
  if (! sexp_exact_integerp(arg0))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, arg0);
  res = sexp_make_unsigned_integer(ctx, getsid(sexp_uint_value(arg0)));
  return res;
}

static sexp sexp_set_current_effective_group_id_x_stub (sexp ctx, sexp self, sexp_sint_t n, sexp arg0) {
  int err;
  sexp res;
  if (! sexp_exact_integerp(arg0))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, arg0);
  err = setegid(sexp_uint_value(arg0));
  if (err) {
  res = SEXP_FALSE;
  } else {
  res = SEXP_TRUE;
  }
  return res;
}

static sexp sexp_set_current_group_id_x_stub (sexp ctx, sexp self, sexp_sint_t n, sexp arg0) {
  int err;
  sexp res;
  if (! sexp_exact_integerp(arg0))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, arg0);
  err = setgid(sexp_uint_value(arg0));
  if (err) {
  res = SEXP_FALSE;
  } else {
  res = SEXP_TRUE;
  }
  return res;
}

static sexp sexp_set_current_effective_user_id_x_stub (sexp ctx, sexp self, sexp_sint_t n, sexp arg0) {
  int err;
  sexp res;
  if (! sexp_exact_integerp(arg0))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, arg0);
  err = seteuid(sexp_uint_value(arg0));
  if (err) {
  res = SEXP_FALSE;
  } else {
  res = SEXP_TRUE;
  }
  return res;
}

static sexp sexp_set_current_user_id_x_stub (sexp ctx, sexp self, sexp_sint_t n, sexp arg0) {
  int err;
  sexp res;
  if (! sexp_exact_integerp(arg0))
    return sexp_type_exception(ctx, self, SEXP_FIXNUM, arg0);
  err = setuid(sexp_uint_value(arg0));
  if (err) {
  res = SEXP_FALSE;
  } else {
  res = SEXP_TRUE;
  }
  return res;
}

static sexp sexp_current_effective_group_id_stub (sexp ctx, sexp self, sexp_sint_t n) {
  sexp res;
  res = sexp_make_unsigned_integer(ctx, getegid());
  return res;
}

static sexp sexp_current_effective_user_id_stub (sexp ctx, sexp self, sexp_sint_t n) {
  sexp res;
  res = sexp_make_unsigned_integer(ctx, geteuid());
  return res;
}

static sexp sexp_current_group_id_stub (sexp ctx, sexp self, sexp_sint_t n) {
  sexp res;
  res = sexp_make_unsigned_integer(ctx, getgid());
  return res;
}

static sexp sexp_current_user_id_stub (sexp ctx, sexp self, sexp_sint_t n) {
  sexp res;
  res = sexp_make_unsigned_integer(ctx, getuid());
  return res;
}

static sexp sexp_passwd_get_pw_name (sexp ctx, sexp self, sexp_sint_t n, sexp x) {
  if (! (sexp_pointerp(x) && (sexp_pointer_tag(x) == sexp_unbox_fixnum(sexp_opcode_arg1_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg1_type(self)), x);
  return sexp_c_string(ctx, ((struct passwd*)sexp_cpointer_value(x))->pw_name, -1);
}

static sexp sexp_passwd_get_pw_passwd (sexp ctx, sexp self, sexp_sint_t n, sexp x) {
  if (! (sexp_pointerp(x) && (sexp_pointer_tag(x) == sexp_unbox_fixnum(sexp_opcode_arg1_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg1_type(self)), x);
  return sexp_c_string(ctx, ((struct passwd*)sexp_cpointer_value(x))->pw_passwd, -1);
}

static sexp sexp_passwd_get_pw_uid (sexp ctx, sexp self, sexp_sint_t n, sexp x) {
  if (! (sexp_pointerp(x) && (sexp_pointer_tag(x) == sexp_unbox_fixnum(sexp_opcode_arg1_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg1_type(self)), x);
  return sexp_make_unsigned_integer(ctx, ((struct passwd*)sexp_cpointer_value(x))->pw_uid);
}

static sexp sexp_passwd_get_pw_gid (sexp ctx, sexp self, sexp_sint_t n, sexp x) {
  if (! (sexp_pointerp(x) && (sexp_pointer_tag(x) == sexp_unbox_fixnum(sexp_opcode_arg1_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg1_type(self)), x);
  return sexp_make_unsigned_integer(ctx, ((struct passwd*)sexp_cpointer_value(x))->pw_gid);
}

static sexp sexp_passwd_get_pw_gecos (sexp ctx, sexp self, sexp_sint_t n, sexp x) {
  if (! (sexp_pointerp(x) && (sexp_pointer_tag(x) == sexp_unbox_fixnum(sexp_opcode_arg1_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg1_type(self)), x);
  return sexp_c_string(ctx, ((struct passwd*)sexp_cpointer_value(x))->pw_gecos, -1);
}

static sexp sexp_passwd_get_pw_dir (sexp ctx, sexp self, sexp_sint_t n, sexp x) {
  if (! (sexp_pointerp(x) && (sexp_pointer_tag(x) == sexp_unbox_fixnum(sexp_opcode_arg1_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg1_type(self)), x);
  return sexp_c_string(ctx, ((struct passwd*)sexp_cpointer_value(x))->pw_dir, -1);
}

static sexp sexp_passwd_get_pw_shell (sexp ctx, sexp self, sexp_sint_t n, sexp x) {
  if (! (sexp_pointerp(x) && (sexp_pointer_tag(x) == sexp_unbox_fixnum(sexp_opcode_arg1_type(self)))))
    return sexp_type_exception(ctx, self, sexp_unbox_fixnum(sexp_opcode_arg1_type(self)), x);
  return sexp_c_string(ctx, ((struct passwd*)sexp_cpointer_value(x))->pw_shell, -1);
}

sexp sexp_init_library (sexp ctx, sexp self, sexp_sint_t n, sexp env, const char* version, sexp_abi_identifier_t abi) {
  sexp sexp_passwd_type_tag;
  sexp_gc_var3(name, tmp, op);
  if (!(sexp_version_compatible(ctx, version, sexp_version)
        && sexp_abi_compatible(ctx, abi, SEXP_ABI_IDENTIFIER)))
    return sexp_global(ctx, SEXP_G_ABI_ERROR);
  sexp_gc_preserve3(ctx, name, tmp, op);
  name = sexp_c_string(ctx, "passwd", -1);
  sexp_passwd_type_tag = sexp_register_c_type(ctx, name, sexp_finalize_c_type);
  tmp = sexp_make_type_predicate(ctx, name, sexp_passwd_type_tag);
  name = sexp_intern(ctx, "user?", 5);
  sexp_env_define(ctx, env, name, tmp);
  op = sexp_define_foreign(ctx, env, "user-shell", 1, sexp_passwd_get_pw_shell);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_STRING);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_passwd_type_tag));
  }
  op = sexp_define_foreign(ctx, env, "user-home", 1, sexp_passwd_get_pw_dir);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_STRING);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_passwd_type_tag));
  }
  op = sexp_define_foreign(ctx, env, "user-gecos", 1, sexp_passwd_get_pw_gecos);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_STRING);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_passwd_type_tag));
  }
  op = sexp_define_foreign(ctx, env, "user-group-id", 1, sexp_passwd_get_pw_gid);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_passwd_type_tag));
  }
  op = sexp_define_foreign(ctx, env, "user-id", 1, sexp_passwd_get_pw_uid);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_passwd_type_tag));
  }
  op = sexp_define_foreign(ctx, env, "user-password", 1, sexp_passwd_get_pw_passwd);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_STRING);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_passwd_type_tag));
  }
  op = sexp_define_foreign(ctx, env, "user-name", 1, sexp_passwd_get_pw_name);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_STRING);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_passwd_type_tag));
  }
  op = sexp_define_foreign(ctx, env, "getpwnam_r", 1, sexp_getpwnam_r_stub);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_OBJECT);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(SEXP_STRING);
    sexp_opcode_arg2_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_passwd_type_tag));
    sexp_opcode_arg3_type(op) = sexp_make_fixnum(SEXP_CHAR);
    sexp_opcode_argn_type(op) = sexp_make_vector(ctx, sexp_make_fixnum(2), sexp_make_fixnum(SEXP_OBJECT));
    sexp_vector_set(sexp_opcode_argn_type(op), sexp_make_fixnum(0), sexp_make_fixnum(SEXP_FIXNUM));
    sexp_vector_set(sexp_opcode_argn_type(op), sexp_make_fixnum(1), sexp_make_fixnum(sexp_type_tag(sexp_passwd_type_tag)));
  }
  op = sexp_define_foreign(ctx, env, "getpwuid_r", 1, sexp_getpwuid_r_stub);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_OBJECT);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg2_type(op) = sexp_make_fixnum(sexp_type_tag(sexp_passwd_type_tag));
    sexp_opcode_arg3_type(op) = sexp_make_fixnum(SEXP_CHAR);
    sexp_opcode_argn_type(op) = sexp_make_vector(ctx, sexp_make_fixnum(2), sexp_make_fixnum(SEXP_OBJECT));
    sexp_vector_set(sexp_opcode_argn_type(op), sexp_make_fixnum(0), sexp_make_fixnum(SEXP_FIXNUM));
    sexp_vector_set(sexp_opcode_argn_type(op), sexp_make_fixnum(1), sexp_make_fixnum(sexp_type_tag(sexp_passwd_type_tag)));
  }
  op = sexp_define_foreign(ctx, env, "set-root-directory!", 1, sexp_set_root_directory_x_stub);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_OBJECT);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(SEXP_STRING);
  }
  op = sexp_define_foreign(ctx, env, "create-session", 0, sexp_create_session_stub);
  op = sexp_define_foreign_opt(ctx, env, "current-session-id", 1, (sexp_proc1)sexp_current_session_id_stub, sexp_make_unsigned_integer(ctx, 0));
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
  }
  op = sexp_define_foreign(ctx, env, "set-current-effective-group-id!", 1, sexp_set_current_effective_group_id_x_stub);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_OBJECT);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
  }
  op = sexp_define_foreign(ctx, env, "set-current-group-id!", 1, sexp_set_current_group_id_x_stub);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_OBJECT);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
  }
  op = sexp_define_foreign(ctx, env, "set-current-effective-user-id!", 1, sexp_set_current_effective_user_id_x_stub);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_OBJECT);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
  }
  op = sexp_define_foreign(ctx, env, "set-current-user-id!", 1, sexp_set_current_user_id_x_stub);
  if (sexp_opcodep(op)) {
    sexp_opcode_return_type(op) = sexp_make_fixnum(SEXP_OBJECT);
    sexp_opcode_arg1_type(op) = sexp_make_fixnum(SEXP_FIXNUM);
  }
  op = sexp_define_foreign(ctx, env, "current-effective-group-id", 0, sexp_current_effective_group_id_stub);
  op = sexp_define_foreign(ctx, env, "current-effective-user-id", 0, sexp_current_effective_user_id_stub);
  op = sexp_define_foreign(ctx, env, "current-group-id", 0, sexp_current_group_id_stub);
  op = sexp_define_foreign(ctx, env, "current-user-id", 0, sexp_current_user_id_stub);
  sexp_gc_release3(ctx);
  return SEXP_VOID;
}

