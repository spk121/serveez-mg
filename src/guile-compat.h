#ifndef SVZ_GUILE_COMPAT
#define SVZ_GUILE_COMPAT

#if SCM_MAJOR_VERSION == 1 && SCM_MINOR_VERSION == 8

#define SCM_BYTEVECTOR_CONTENTS(bv) scm_i_string_chars(bv)

static SCM
scm_c_make_bytevector (size_t len)
{
  return scm_c_make_string (len, SCM_MAKE_CHAR ('\0'));
}

static void
scm_c_bytevector_set_x (SCM bv, size_t index, scm_t_uint8 value)
{
  scm_c_string_set_x (bv, index, SCM_MAKE_CHAR(value));
}

static SCM
scm_bytevector_p (SCM bv)
{
  return scm_string_p (bv);
}

static size_t
scm_c_bytevector_length (SCM bv)
{
  return scm_c_string_length (bv);
}

#endif /* guile 1.8 */

#endif /* SVZ_GUILE_COMPAT */
