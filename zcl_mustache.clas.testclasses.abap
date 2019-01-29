define _add_mu_val_rc.
  append initial line to &1 assigning <rcline>.
  <rcline>-val    = &2.
  <rcline>-rc     = &3.
end-of-definition.

class ltcl_dummy_component definition final.
  public section.
    methods render
      importing iv_value type string optional
      returning value(rv_val) type string.
endclass.

class ltcl_dummy_component implementation.
  method render.
    rv_val = 'hello world'.
  endmethod.
endclass.

class ltcl_dummy_component_norend definition final.
  public section.
    methods other.
endclass.

class ltcl_dummy_component_norend implementation.
  method other. endmethod.
endclass.

class ltcl_dummy_component_noret definition final.
  public section.
    methods render.
endclass.

class ltcl_dummy_component_noret implementation.
  method render. endmethod.
endclass.

class ltcl_dummy_component_manimp definition final.
  public section.
    methods render importing iv_value type string.
endclass.
class ltcl_dummy_component_manimp implementation.
  method render. endmethod.
endclass.

interface ltif_dummy_component.
  methods render returning value(rv_val) type string.
endinterface.

class ltcl_dummy_component_intf definition final.
  public section.
    interfaces ltif_dummy_component.
endclass.

class ltcl_dummy_component_intf implementation.
  method ltif_dummy_component~render.
    rv_val = 'hello world'.
  endmethod.
endclass.

class ltcl_mustache definition final
  for testing
  risk level harmless
  duration short.

  public section.

    constants c_nl type c value cl_abap_char_utilities=>newline.

  private section.


    methods render_negative         for testing.
    methods render_w_partials       for testing.
    methods render_tt               for testing.
    methods add_partial             for testing.
    methods render_with_data_builder for testing.
    methods render_with_object      for testing.
    methods render_oref_negative for testing.

endclass.

class ltcl_mustache implementation.

  method render_w_partials.

    data:
          lo_mustache type ref to zcl_mustache,
          lt_data     type zif_mustache=>ty_struc_tt,
          lv_exp      type string,
          lv_act      type string,
          lx          type ref to zcx_mustache_error.

    zcl_mustache_test=>get_test_data( importing et_complex  = lt_data ).
    lv_exp = 'Welcome to Shopsky' &&
             '* Boots - $99.00'   &&
             '* T-short - $49.00'.

    try .
        lo_mustache = zcl_mustache=>create(
          'Welcome to {{shop}}' &&
          '{{> items}}' ).

        lo_mustache->add_partial(
          iv_name = 'items'
          io_obj  = zcl_mustache=>create(
            '{{#items}}'              &&
            '* {{name}} - ${{price}}' &&
            '{{/items}}' ) ).

        lv_act = lo_mustache->render( lt_data ).

        cl_abap_unit_assert=>assert_equals(
          exp = lv_exp
          act = lv_act ).
      catch zcx_mustache_error into lx.
        cl_abap_unit_assert=>fail( lx->msg ).
    endtry.

  endmethod. "render_w_partials

  method render_tt.

    data:
          lo_mustache type ref to zcl_mustache,
          lt_data     type zif_mustache=>ty_struc_tt,
          lt_exp      type string_table,
          lt_act      type string_table,
          lx          type ref to zcx_mustache_error.

    zcl_mustache_test=>get_test_data( importing et_complex  = lt_data ).

    append 'Welcome to Shopsky' to lt_exp.
    append '* Boots - $99.00' to lt_exp.
    append '* T-short - $49.00' to lt_exp.

    try .
        lo_mustache = zcl_mustache=>create(
          'Welcome to {{shop}}'     && c_nl &&
          '{{#items}}'              && c_nl &&
          '* {{name}} - ${{price}}' && c_nl &&
          '{{/items}}' ).

        lt_act = lo_mustache->render_tt( lt_data ).

        cl_abap_unit_assert=>assert_equals(
          exp = lt_exp
          act = lt_act ).
      catch zcx_mustache_error into lx.
        cl_abap_unit_assert=>fail( lx->msg ).
    endtry.

  endmethod.  " render_tt.

  method render_negative.

    data:
          lt_tab      type zcl_mustache_test=>ty_tag_rc_tt,
          ls_dummy    type zcl_mustache_test=>ty_dummy,
          lo_mustache type ref to zcl_mustache,
          lv_idx      type i,
          lx          type ref to zcx_mustache_error.

    field-symbols <rcline> like line of lt_tab.

    zcl_mustache_test=>get_test_data( importing es_simple = ls_dummy ).

    _add_mu_val_rc lt_tab '{{>partial}}'      'PNF'.
    _add_mu_val_rc lt_tab '{{#obj}}!{{/obj}}' 'CRWD'.
    _add_mu_val_rc lt_tab '{{field}}'         'FNF'.

    loop at lt_tab assigning <rcline>.
      lv_idx = sy-tabix.
      clear lx.
      try.
          lo_mustache = zcl_mustache=>create( <rcline>-val ).
          lo_mustache->render( ls_dummy ).
        catch zcx_mustache_error into lx.
          assert 1 = 1.
      endtry.
      cl_abap_unit_assert=>assert_not_initial(
        act = lx
        msg = |render_negative, case { lv_idx }| ).
      cl_abap_unit_assert=>assert_equals(
        exp = <rcline>-rc
        act = lx->rc
        msg = |render_negative, case { lv_idx }| ).
    endloop.

  endmethod. "render_negative

  method add_partial.

    data:
          lo_mustache type ref to zcl_mustache,
          lo_partial  type ref to zcl_mustache,
          ls_dummy    type zcl_mustache_test=>ty_dummy,
          lx          type ref to zcx_mustache_error.

    " 1. Success
    try .
        lo_mustache = zcl_mustache=>create( 'Hello' ).
        lo_partial  = zcl_mustache=>create( 'World' ).

        lo_mustache->add_partial( iv_name = 'partial1' io_obj = lo_partial ).

        cl_abap_unit_assert=>assert_equals(
          act = lines( lo_mustache->get_partials( ) )
          exp = 1 ).
      catch zcx_mustache_error into lx.
        cl_abap_unit_assert=>fail( lx->msg ).
    endtry.

    " 2. Duplicate name
    try .
        lo_mustache->add_partial( iv_name = 'partial1' io_obj = lo_partial ).
      catch zcx_mustache_error into lx.
        assert 1 = 1.
    endtry.
    cl_abap_unit_assert=>assert_not_initial( act = lx ).
    cl_abap_unit_assert=>assert_equals( exp = 'DP' act = lx->rc ).

    " 3. Empty obj
    clear lx.
    try .
        clear lo_partial.
        lo_mustache->add_partial( iv_name = 'partial2' io_obj = lo_partial ).
      catch zcx_mustache_error into lx.
        assert 1 = 1.
    endtry.
    cl_abap_unit_assert=>assert_not_initial( act = lx ).
    cl_abap_unit_assert=>assert_equals( exp = 'PONB' act = lx->rc ).

    " 4. Recursion protection
    clear lx.
    try .
        lo_mustache = zcl_mustache=>create( 'Hello {{>partialLoop}}' ).
        lo_mustache->add_partial( iv_name = 'partialLoop' io_obj = lo_mustache ).
        lo_mustache->render( ls_dummy ).
      catch zcx_mustache_error into lx.
        assert 1 = 1.
    endtry.
    cl_abap_unit_assert=>assert_not_initial( act = lx ).
    cl_abap_unit_assert=>assert_equals( exp = 'MPD' act = lx->rc ).

  endmethod. "add_partial

  method render_with_data_builder.

    data lo_data type ref to zcl_mustache_data.
    data lv_act  type string.
    data lt_strings type string_table.
    data lo_mustache type ref to zcl_mustache.
    data lx          type ref to zcx_mustache_error.

    create object lo_data.
    append 'hello' to lt_strings.
    append 'world' to lt_strings.
    lo_data->add( iv_name = 'items' iv_val = lt_strings ).

    try .
        lo_mustache = zcl_mustache=>create(
          '{{#items}}'     && c_nl &&
          '-{{@tabline}}' && c_nl &&
          '{{/items}}' ).

        lv_act = lo_mustache->render( lo_data->get( ) ).

        cl_abap_unit_assert=>assert_equals(
          exp = '-hello' && c_nl && '-world' && c_nl
          act = lv_act ).
      catch zcx_mustache_error into lx.
        cl_abap_unit_assert=>fail( lx->msg ).
    endtry.

  endmethod.

  method render_with_object.

    data lo_data type ref to zcl_mustache_data.
    data lo_component type ref to ltcl_dummy_component.
    data lv_act  type string.
    data lo_mustache type ref to zcl_mustache.
    data lx          type ref to zcx_mustache_error.

    try .
        lo_mustache = zcl_mustache=>create( '{{tag}}' ).
      catch zcx_mustache_error into lx.
        cl_abap_unit_assert=>fail( lx->msg ).
    endtry.

    " Object
    create object lo_component.
    create object lo_data.
    lo_data->add( iv_name = 'tag' iv_val = lo_component ).
    try .
        lv_act = lo_mustache->render( lo_data->get( ) ).
        cl_abap_unit_assert=>assert_equals(
          exp = 'hello world'
          act = lv_act ).
      catch zcx_mustache_error into lx.
        cl_abap_unit_assert=>fail( lx->msg ).
    endtry.

    " Interface
    data lo_comp_if type ref to ltcl_dummy_component_intf.
    data li_comp type ref to ltif_dummy_component.
    create object lo_comp_if.
    create object lo_data.
    li_comp = lo_comp_if.
    lo_data->add( iv_name = 'tag' iv_val = li_comp ).
    try .
        lv_act = lo_mustache->render( lo_data->get( ) ).
        cl_abap_unit_assert=>assert_equals(
          exp = 'hello world'
          act = lv_act ).
      catch zcx_mustache_error into lx.
        cl_abap_unit_assert=>fail( lx->msg ).
    endtry.

  endmethod.

  method render_oref_negative.

    data lo_data type ref to zcl_mustache_data.
    data lo_mustache type ref to zcl_mustache.
    data lx          type ref to zcx_mustache_error.

    try .
        lo_mustache = zcl_mustache=>create( '{{tag}}' ).
      catch zcx_mustache_error into lx.
        cl_abap_unit_assert=>fail( lx->msg ).
    endtry.

    " no render
    data lo_comp1 type ref to ltcl_dummy_component_norend.
    clear lx.
    create object lo_comp1.
    create object lo_data.
    lo_data->add( iv_name = 'tag' iv_val = lo_comp1 ).
    try .
        lo_mustache->render( lo_data->get( ) ).
      catch zcx_mustache_error into lx.
        cl_abap_unit_assert=>assert_equals(
          exp = 'ODHR'
          act = lx->rc ).
    endtry.
    cl_abap_unit_assert=>assert_not_initial( lx ).

    " no return
    data lo_comp2 type ref to ltcl_dummy_component_noret.
    clear lx.
    create object lo_comp2.
    create object lo_data.
    lo_data->add( iv_name = 'tag' iv_val = lo_comp2 ).
    try .
        lo_mustache->render( lo_data->get( ) ).
      catch zcx_mustache_error into lx.
        cl_abap_unit_assert=>assert_equals(
          exp = 'ORRS'
          act = lx->rc ).
    endtry.
    cl_abap_unit_assert=>assert_not_initial( lx ).

    " mandatory param
    data lo_comp3 type ref to ltcl_dummy_component_manimp.
    clear lx.
    create object lo_comp3.
    create object lo_data.
    lo_data->add( iv_name = 'tag' iv_val = lo_comp3 ).
    try .
        lo_mustache->render( lo_data->get( ) ).
      catch zcx_mustache_error into lx.
        cl_abap_unit_assert=>assert_equals(
          exp = 'ORMP'
          act = lx->rc ).
    endtry.
    cl_abap_unit_assert=>assert_not_initial( lx ).

  endmethod.

endclass.
