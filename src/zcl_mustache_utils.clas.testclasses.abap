class ltcl_mustache_utils definition final
  for testing risk level harmless duration short.

  private section.
    methods split_string for testing.
    methods join_strings for testing.
    methods check_version_fits for testing.

endclass. "ltcl_mustache_utils

class ltcl_mustache_utils implementation.

  method split_string.

    data: lt_exp type string_table.

    append 'ABC' to lt_exp.
    append '123' to lt_exp.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_mustache_utils=>split_string(
      'ABC' && cl_abap_char_utilities=>cr_lf && '123' )
      exp = lt_exp ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_mustache_utils=>split_string(
      'ABC' && cl_abap_char_utilities=>newline && '123' )
      exp = lt_exp ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_mustache_utils=>split_string(
      iv_text = 'ABC' && 'X' && '123' iv_sep = 'X' )
      exp = lt_exp ).

  endmethod.  " split_string.

  method join_strings.

    data: lt_src type string_table.

    append 'ABC' to lt_src.
    append '123' to lt_src.

    cl_abap_unit_assert=>assert_equals(
      act = zcl_mustache_utils=>join_strings( lt_src )
      exp = 'ABC' && cl_abap_char_utilities=>newline && '123' ).

    cl_abap_unit_assert=>assert_equals(
      act = zcl_mustache_utils=>join_strings( it_tab = lt_src iv_sep = 'X' )
      exp = 'ABC' && 'X' && '123' ).

  endmethod. "join_strings

  method check_version_fits.
    cl_abap_unit_assert=>assert_true(
      zcl_mustache_utils=>check_version_fits(
        i_current_version = 'v2.2.2'
        i_required_version = 'v2.2.2' ) ).
    cl_abap_unit_assert=>assert_true(
      zcl_mustache_utils=>check_version_fits(
        i_current_version = 'v2.2.2'
        i_required_version = 'v2.1.2' ) ).
    cl_abap_unit_assert=>assert_true(
      zcl_mustache_utils=>check_version_fits(
        i_current_version = 'v2.2.2'
        i_required_version = 'v1.0.0' ) ).
    cl_abap_unit_assert=>assert_false(
      zcl_mustache_utils=>check_version_fits(
        i_current_version = 'v2.2.2'
        i_required_version = 'v2.2.3' ) ).
    cl_abap_unit_assert=>assert_false(
      zcl_mustache_utils=>check_version_fits(
        i_current_version = 'v2.2.2'
        i_required_version = 'v2.2.30' ) ).
    cl_abap_unit_assert=>assert_false(
      zcl_mustache_utils=>check_version_fits(
        i_current_version = 'v2.2.2'
        i_required_version = 'v2.3.1' ) ).
    cl_abap_unit_assert=>assert_false(
      zcl_mustache_utils=>check_version_fits(
        i_current_version = 'v2.2.2'
        i_required_version = 'v3.0.0' ) ).
  endmethod.

endclass. "ltcl_mustache_utils
