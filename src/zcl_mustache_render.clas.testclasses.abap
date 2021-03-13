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
          lx            type ref to zcx_mustache_error,
          lstatics      type zcl_mustache_render=>ty_context.

    lstatics = VALUE #( date_format = cl_abap_format=>d_iso time_format = cl_abap_format=>t_iso timestamp_format = cl_abap_format=>ts_iso timestamp_timezone = 'UTC' ).

    ls_dummy-name = 'abc'.
    ls_dummy-val  = '123'.
    append ls_dummy to lt_dummy.

    "1----------------
    clear lt_data_stack.
    get reference of ls_dummy into lr.
    append lr to lt_data_stack.

    try .
        lv_act = zcl_mustache_render=>find_value( it_data_stack = lt_data_stack iv_name = 'NAME' is_statics = lstatics ).
        cl_abap_unit_assert=>assert_equals( exp = 'abc' act = lv_act ).
      catch zcx_mustache_error into lx.
        cl_abap_unit_assert=>fail( lx->msg ).
    endtry.

    "2----------------
    clear lt_data_stack.
    get reference of lt_dummy into lr.
    append lr to lt_data_stack.

    try .
        lv_act = zcl_mustache_render=>find_value( it_data_stack = lt_data_stack iv_name = 'Abc' is_statics = lstatics ).
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
        lv_act = zcl_mustache_render=>find_value( it_data_stack = lt_data_stack iv_name = 'Abc' is_statics = lstatics ).
        cl_abap_unit_assert=>assert_equals( exp = '123' act = lv_act ).
        lv_act = zcl_mustache_render=>find_value( it_data_stack = lt_data_stack iv_name = 'name' is_statics = lstatics ).
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
        lv_act = zcl_mustache_render=>find_value( it_data_stack = lt_data_stack iv_name = 'attr-age' is_statics = lstatics ).
        cl_abap_unit_assert=>assert_equals( exp = '10' act = lv_act ).
      catch zcx_mustache_error into lx.
        cl_abap_unit_assert=>fail( lx->msg ).
    endtry.

    "5----------------
    types:
      begin of ty_attr,
        integer type i,
        date type d,
        time type t,
        timestamp type timestamp,
        timestampl type timestampl,
      end of ty_attr.
    data ls_attr type ty_attr.
    ls_attr-date = '19670625'.
    ls_attr-time = '193045'.
    ls_attr-timestamp = '19670625193045'.
    ls_attr-timestampl = '19670625193045'.
    ls_attr-integer = 123.
    clear lt_data_stack.
    get reference of ls_attr into lr.
    append lr to lt_data_stack.

    try .
        lv_act = zcl_mustache_render=>find_value( it_data_stack = lt_data_stack iv_name = 'date' is_statics = lstatics ).
        cl_abap_unit_assert=>assert_equals( exp = '1967-06-25' act = lv_act ).
      catch zcx_mustache_error into lx.
        cl_abap_unit_assert=>fail( lx->msg ).
    endtry.

    try .
        lv_act = zcl_mustache_render=>find_value( it_data_stack = lt_data_stack iv_name = 'time' is_statics = lstatics ).
        cl_abap_unit_assert=>assert_equals( exp = '19:30:45' act = lv_act ).
      catch zcx_mustache_error into lx.
        cl_abap_unit_assert=>fail( lx->msg ).
    endtry.

    try .
        lv_act = zcl_mustache_render=>find_value( it_data_stack = lt_data_stack iv_name = 'timestamp' is_statics = lstatics ).
        cl_abap_unit_assert=>assert_equals( exp = '1967-06-25T19:30:45' act = lv_act ).
      catch zcx_mustache_error into lx.
        cl_abap_unit_assert=>fail( lx->msg ).
    endtry.

    try .
        lv_act = zcl_mustache_render=>find_value( it_data_stack = lt_data_stack iv_name = 'timestampl' is_statics = lstatics ).
        cl_abap_unit_assert=>assert_equals( exp = '1967-06-25T19:30:45,0000000' act = lv_act ).
      catch zcx_mustache_error into lx.
        cl_abap_unit_assert=>fail( lx->msg ).
    endtry.

  endmethod.  " find_value.

  method render_section.

    data:
          ls_statics      type zcl_mustache_render=>ty_context,
          ls_simple       type zcl_mustache_test=>ty_dummy,
          lt_complex1     type zif_mustache=>ty_struc_tt,
          lt_complex2     type zif_mustache=>ty_struc_tt,
          lv_count        type i,
          lv_idx          type i,
          iv_complex_test type c,
          lv_exp          type string,
          lv_act          type string,
          lt_act          type string_table,
          lx              type ref to zcx_mustache_error.

    zcl_mustache_test=>get_test_case( importing ev_count = lv_count ).
    zcl_mustache_test=>get_test_data( importing es_simple   = ls_simple
                                                et_complex1  = lt_complex1
                                                et_complex2  = lt_complex2 ).
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
          if iv_complex_test = '1'.
            zcl_mustache_render=>render_section(
              exporting
                is_statics = ls_statics
                i_data     = lt_complex1
              changing
                ct_lines   = lt_act ).
          elseif iv_complex_test = '2'.
            zcl_mustache_render=>render_section(
              exporting
                is_statics = ls_statics
                i_data     = lt_complex2
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
    ls_statics-tokens = VALUE #(
      ( type = zif_mustache=>c_token_type-etag level = 1 content = `name` )
      ( type = zif_mustache=>c_token_type-static level = 1 content = `: age ` )
      ( type = zif_mustache=>c_token_type-etag level = 1 content = `attr-age` )
    ).
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
    ls_statics-tokens = VALUE #(
      ( type = zif_mustache=>c_token_type-etag level = 1 content = `name` )
      ( type = zif_mustache=>c_token_type-static level = 1 content = `: ` )
      ( type = zif_mustache=>c_token_type-section cond = '=' level = 1 content = `attr-male` )
      ( type = zif_mustache=>c_token_type-static level = 2 content = `Male` )
      ( type = zif_mustache=>c_token_type-section cond = '=' level = 1 content = `attr-female` )
      ( type = zif_mustache=>c_token_type-static level = 2 content = `Female` )
    ).
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
