define _add_mu_token.
  append initial line to &1 assigning <token>.
  <token>-type    = &2.
  <token>-cond    = &3.
  <token>-level   = &4.
  <token>-content = &5.
end-of-definition.

class ltcl_mustache_render definition final
  for testing
  risk level
  harmless duration short.

  private section.

    methods find_value     for testing.
    methods render_section for testing.
    methods render_path for testing.

endclass.

class ltcl_mustache_render implementation.

  method find_value.

    data:
          lr            type ref to data,
          lt_data_stack type zif_mustache=>ty_ref_tt,
          ls_dummy      type zif_mustache=>ty_struc,
          lt_dummy      type zif_mustache=>ty_struc_tt,
          lv_act        type string,
          lx            type ref to zcx_mustache_error.

    ls_dummy-name = 'abc'.
    ls_dummy-val  = '123'.
    append ls_dummy to lt_dummy.

    "1----------------
    clear lt_data_stack.
    get reference of ls_dummy into lr.
    append lr to lt_data_stack.

    try .
        lv_act = zcl_mustache_render=>find_value( it_data_stack = lt_data_stack iv_name = 'NAME' ).
        cl_abap_unit_assert=>assert_equals( exp = 'abc' act = lv_act ).
      catch zcx_mustache_error into lx.
        cl_abap_unit_assert=>fail( lx->msg ).
    endtry.

    "2----------------
    clear lt_data_stack.
    get reference of lt_dummy into lr.
    append lr to lt_data_stack.

    try .
        lv_act = zcl_mustache_render=>find_value( it_data_stack = lt_data_stack iv_name = 'Abc' ).
        cl_abap_unit_assert=>assert_equals( exp = '123' act = lv_act ).
      catch zcx_mustache_error into lx.
        cl_abap_unit_assert=>fail( lx->msg ).
    endtry.

    "3----------------
    clear lt_data_stack.
    get reference of ls_dummy into lr.
    append lr to lt_data_stack.
    get reference of lt_dummy into lr.
    append lr to lt_data_stack.

    try .
        lv_act = zcl_mustache_render=>find_value( it_data_stack = lt_data_stack iv_name = 'Abc' ).
        cl_abap_unit_assert=>assert_equals( exp = '123' act = lv_act ).
        lv_act = zcl_mustache_render=>find_value( it_data_stack = lt_data_stack iv_name = 'name' ).
        cl_abap_unit_assert=>assert_equals( exp = 'abc' act = lv_act ).
      catch zcx_mustache_error into lx.
        cl_abap_unit_assert=>fail( lx->msg ).
    endtry.

    "4----------------
    data ls_struc type zcl_mustache_test=>ty_dummy.
    ls_struc-attr-age = 10.
    clear lt_data_stack.
    get reference of ls_struc into lr.
    append lr to lt_data_stack.

    try .
        lv_act = zcl_mustache_render=>find_value( it_data_stack = lt_data_stack iv_name = 'attr-age' ).
        cl_abap_unit_assert=>assert_equals( exp = '10' act = lv_act ).
      catch zcx_mustache_error into lx.
        cl_abap_unit_assert=>fail( lx->msg ).
    endtry.


  endmethod.  " find_value.

  method render_section.

    data:
          ls_statics      type zcl_mustache_render=>ty_context,
          ls_simple       type zcl_mustache_test=>ty_dummy,
          lt_complex      type zif_mustache=>ty_struc_tt,
          lv_count        type i,
          lv_idx          type i,
          iv_complex_test type abap_bool,
          lv_exp          type string,
          lv_act          type string,
          lt_act          type string_table,
          lx              type ref to zcx_mustache_error.

    zcl_mustache_test=>get_test_case( importing ev_count = lv_count ).
    zcl_mustache_test=>get_test_data( importing es_simple   = ls_simple
                                                et_complex  = lt_complex ).
    ls_statics-x_format = cl_abap_format=>e_html_text.

    do lv_count times.
      lv_idx = sy-index.
      zcl_mustache_test=>get_test_case(
        exporting
          iv_index        = lv_idx
        importing
          ev_output       = lv_exp
          et_tokens       = ls_statics-tokens
          ev_complex_test = iv_complex_test ).

      clear lt_act.
      try .
          if iv_complex_test = abap_true.
            zcl_mustache_render=>render_section(
              exporting
                is_statics = ls_statics
                i_data     = lt_complex
              changing
                ct_lines   = lt_act ).
          else.
            zcl_mustache_render=>render_section(
              exporting
                is_statics = ls_statics
                i_data     = ls_simple
              changing
                ct_lines   = lt_act ).
          endif.
          lv_act = zcl_mustache_utils=>join_strings( it_tab = lt_act iv_sep = '' ).


          cl_abap_unit_assert=>assert_equals(
            exp = lv_exp
            act = lv_act
            msg = |render_section, case { lv_idx }| ).
        catch zcx_mustache_error into lx.
          cl_abap_unit_assert=>fail( lx->msg ).
      endtry.

    enddo.

  endmethod. "render_section

  method render_path.

    data ls_test type zcl_mustache_test=>ty_test_case.
    data ls_statics type zcl_mustache_render=>ty_context.
    data ls_data type zcl_mustache_test=>ty_dummy.
    data lt_act type string_table.
    data lv_act type string.
    data lx type ref to zcx_mustache_error.
    field-symbols <token> like line of ls_test-tokens.

    ls_data-name = 'Vasya'.
    ls_data-attr-male = abap_true.
    ls_data-attr-age = 30.

    ls_test-template = '{{name}}: age {{attr-age}}'.
    _add_mu_token ls_statics-tokens zif_mustache=>c_token_type-etag        ''  1   'name'.
    _add_mu_token ls_statics-tokens zif_mustache=>c_token_type-static      ''  1   `: age `.
    _add_mu_token ls_statics-tokens zif_mustache=>c_token_type-etag        ''  1   'attr-age'.
    ls_test-output = 'Vasya: age 30'.

    try .
      zcl_mustache_render=>render_section(
        exporting
          is_statics = ls_statics
          i_data     = ls_data
        changing
          ct_lines   = lt_act ).
      lv_act = zcl_mustache_utils=>join_strings( it_tab = lt_act iv_sep = '' ).

      cl_abap_unit_assert=>assert_equals(
        exp = ls_test-output
        act = lv_act ).
    catch zcx_mustache_error into lx.
      cl_abap_unit_assert=>fail( lx->msg ).
    endtry.

    clear: ls_statics-tokens, lt_act.
    ls_test-template = '{{name}}: {{#attr-male}}Male{{/attr-male}}{{#attr-female}}Female{{/attr-female}}'.
    _add_mu_token ls_statics-tokens zif_mustache=>c_token_type-etag        ''  1   'name'.
    _add_mu_token ls_statics-tokens zif_mustache=>c_token_type-static      ''  1   `: `.
    _add_mu_token ls_statics-tokens zif_mustache=>c_token_type-section     '=' 1   'attr-male'.
    _add_mu_token ls_statics-tokens zif_mustache=>c_token_type-static      ''  2   `Male`.
    _add_mu_token ls_statics-tokens zif_mustache=>c_token_type-section     '=' 1   'attr-female'.
    _add_mu_token ls_statics-tokens zif_mustache=>c_token_type-static      ''  2   `Female`.
    ls_test-output = 'Vasya: Male'.

    try .
      zcl_mustache_render=>render_section(
        exporting
          is_statics = ls_statics
          i_data     = ls_data
        changing
          ct_lines   = lt_act ).
      lv_act = zcl_mustache_utils=>join_strings( it_tab = lt_act iv_sep = '' ).

      cl_abap_unit_assert=>assert_equals(
        exp = ls_test-output
        act = lv_act ).
    catch zcx_mustache_error into lx.
      cl_abap_unit_assert=>fail( lx->msg ).
    endtry.

  endmethod.

endclass.
