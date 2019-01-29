*&---------------------------------------------------------------------*
*&  Include           ZMUSTACHE_UT
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       MACROS
*----------------------------------------------------------------------*

DEFINE _add_mu_token.
  APPEND INITIAL LINE TO &1 ASSIGNING <token>.
  <token>-type    = &2.
  <token>-cond    = &3.
  <token>-level   = &4.
  <token>-content = &5.
END-OF-DEFINITION.

DEFINE _add_mu_val_rc.
  APPEND INITIAL LINE TO &1 ASSIGNING <rcline>.
  <rcline>-val    = &2.
  <rcline>-rc     = &3.
END-OF-DEFINITION.

CLASS ltcl_dummy_component DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS render
      IMPORTING iv_value TYPE string OPTIONAL
      RETURNING VALUE(rv_val) TYPE string.
ENDCLASS.
CLASS ltcl_dummy_component IMPLEMENTATION.
  METHOD render.
    rv_val = 'hello world'.
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_dummy_component_norend DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS other.
ENDCLASS.
CLASS ltcl_dummy_component_norend IMPLEMENTATION.
  METHOD other.
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_dummy_component_noret DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS render.
ENDCLASS.
CLASS ltcl_dummy_component_noret IMPLEMENTATION.
  METHOD render. ENDMETHOD.
ENDCLASS.

CLASS ltcl_dummy_component_manimp DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS render IMPORTING iv_value TYPE string.
ENDCLASS.
CLASS ltcl_dummy_component_manimp IMPLEMENTATION.
  METHOD render. ENDMETHOD.
ENDCLASS.

INTERFACE ltif_dummy_component.
  METHODS render RETURNING VALUE(rv_val) TYPE string.
ENDINTERFACE.

CLASS ltcl_dummy_component_intf DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES ltif_dummy_component.
ENDCLASS.
CLASS ltcl_dummy_component_intf IMPLEMENTATION.
  METHOD ltif_dummy_component~render.
    rv_val = 'hello world'.
  ENDMETHOD.
ENDCLASS.


*----------------------------------------------------------------------*
*       CLASS ltcl_mustache
*----------------------------------------------------------------------*
CLASS ltcl_mustache DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PUBLIC SECTION.

    CONSTANTS c_nl TYPE c VALUE cl_abap_char_utilities=>newline.

  PRIVATE SECTION.


    METHODS render_negative         FOR TESTING.
    METHODS render_w_partials       FOR TESTING.
    METHODS render_tt               FOR TESTING.
    METHODS add_partial             FOR TESTING.
    METHODS render_with_data_builder FOR TESTING.
    METHODS render_with_object      FOR TESTING.
    METHODS render_oref_negative FOR TESTING.

ENDCLASS. "ltcl_mustache

CLASS ltcl_mustache IMPLEMENTATION.



  METHOD render_w_partials.

    DATA:
          lo_mustache TYPE REF TO lcl_mustache,
          lt_data     TYPE zif_mustache=>ty_struc_tt,
          lv_exp      TYPE string,
          lv_act      TYPE string,
          lx          TYPE REF TO zcx_mustache_error.

    zcl_mustache_test=>get_test_data( IMPORTING et_complex  = lt_data ).
    lv_exp = 'Welcome to Shopsky' &&
             '* Boots - $99.00'   &&
             '* T-short - $49.00'.

    TRY .
      lo_mustache = lcl_mustache=>create(
        'Welcome to {{shop}}' &&
        '{{> items}}' ).

      lo_mustache->add_partial(
        iv_name = 'items'
        io_obj  = lcl_mustache=>create(
          '{{#items}}'              &&
          '* {{name}} - ${{price}}' &&
          '{{/items}}' ) ).

      lv_act = lo_mustache->render( lt_data ).

      cl_abap_unit_assert=>assert_equals(
        exp = lv_exp
        act = lv_act ).
    CATCH zcx_mustache_error INTO lx.
      cl_abap_unit_assert=>fail( lx->msg ).
    ENDTRY.

  ENDMETHOD. "render_w_partials

  METHOD render_tt.

    DATA:
          lo_mustache TYPE REF TO lcl_mustache,
          lt_data     TYPE zif_mustache=>ty_struc_tt,
          lt_exp      TYPE string_table,
          lt_act      TYPE string_table,
          lx          TYPE REF TO zcx_mustache_error.

    zcl_mustache_test=>get_test_data( IMPORTING et_complex  = lt_data ).

    APPEND 'Welcome to Shopsky' TO lt_exp.
    APPEND '* Boots - $99.00' TO lt_exp.
    APPEND '* T-short - $49.00' TO lt_exp.

    TRY .
      lo_mustache = lcl_mustache=>create(
        'Welcome to {{shop}}'     && c_nl &&
        '{{#items}}'              && c_nl &&
        '* {{name}} - ${{price}}' && c_nl &&
        '{{/items}}' ).

      lt_act = lo_mustache->render_tt( lt_data ).

      cl_abap_unit_assert=>assert_equals(
        exp = lt_exp
        act = lt_act ).
    CATCH zcx_mustache_error INTO lx.
      cl_abap_unit_assert=>fail( lx->msg ).
    ENDTRY.

  ENDMETHOD.  " render_tt.

  METHOD render_negative.

    DATA:
          lt_tab      TYPE zcl_mustache_test=>ty_tag_rc_tt,
          ls_dummy    TYPE zcl_mustache_test=>ty_dummy,
          lo_mustache TYPE REF TO lcl_mustache,
          lv_idx      TYPE i,
          lx          TYPE REF TO zcx_mustache_error.

    FIELD-SYMBOLS <rcline> LIKE LINE OF lt_tab.

    zcl_mustache_test=>get_test_data( IMPORTING es_simple = ls_dummy ).

    _add_mu_val_rc lt_tab '{{>partial}}'      'PNF'.
    _add_mu_val_rc lt_tab '{{#obj}}!{{/obj}}' 'CRWD'.
    _add_mu_val_rc lt_tab '{{field}}'         'FNF'.

    LOOP AT lt_tab ASSIGNING <rcline>.
      lv_idx = sy-tabix.
      CLEAR lx.
      TRY.
        lo_mustache = lcl_mustache=>create( <rcline>-val ).
        lo_mustache->render( ls_dummy ).
      CATCH zcx_mustache_error INTO lx.
        ASSERT 1 = 1.
      ENDTRY.
      cl_abap_unit_assert=>assert_not_initial(
        act = lx
        msg = |render_negative, case { lv_idx }| ).
      cl_abap_unit_assert=>assert_equals(
        exp = <rcline>-rc
        act = lx->rc
        msg = |render_negative, case { lv_idx }| ).
    ENDLOOP.

  ENDMETHOD. "render_negative

  METHOD add_partial.

    DATA:
          lo_mustache TYPE REF TO lcl_mustache,
          lo_partial  TYPE REF TO lcl_mustache,
          ls_dummy    TYPE zcl_mustache_test=>ty_dummy,
          lx          TYPE REF TO zcx_mustache_error.

    " 1. Success
    TRY .
      lo_mustache = lcl_mustache=>create( 'Hello' ).
      lo_partial  = lcl_mustache=>create( 'World' ).

      lo_mustache->add_partial( iv_name = 'partial1' io_obj = lo_partial ).

      cl_abap_unit_assert=>assert_equals(
        act = lines( lo_mustache->get_partials( ) )
        exp = 1 ).
    CATCH zcx_mustache_error INTO lx.
      cl_abap_unit_assert=>fail( lx->msg ).
    ENDTRY.

    " 2. Duplicate name
    TRY .
      lo_mustache->add_partial( iv_name = 'partial1' io_obj = lo_partial ).
    CATCH zcx_mustache_error INTO lx.
      ASSERT 1 = 1.
    ENDTRY.
    cl_abap_unit_assert=>assert_not_initial( act = lx ).
    cl_abap_unit_assert=>assert_equals( exp = 'DP' act = lx->rc ).

    " 3. Empty obj
    CLEAR lx.
    TRY .
      CLEAR lo_partial.
      lo_mustache->add_partial( iv_name = 'partial2' io_obj = lo_partial ).
    CATCH zcx_mustache_error INTO lx.
      ASSERT 1 = 1.
    ENDTRY.
    cl_abap_unit_assert=>assert_not_initial( act = lx ).
    cl_abap_unit_assert=>assert_equals( exp = 'PONB' act = lx->rc ).

    " 4. Recursion protection
    CLEAR lx.
    TRY .
      lo_mustache = lcl_mustache=>create( 'Hello {{>partialLoop}}' ).
      lo_mustache->add_partial( iv_name = 'partialLoop' io_obj = lo_mustache ).
      lo_mustache->render( ls_dummy ).
    CATCH zcx_mustache_error INTO lx.
      ASSERT 1 = 1.
    ENDTRY.
    cl_abap_unit_assert=>assert_not_initial( act = lx ).
    cl_abap_unit_assert=>assert_equals( exp = 'MPD' act = lx->rc ).

  ENDMETHOD. "add_partial

  METHOD render_with_data_builder.

    DATA lo_data TYPE REF TO zcl_mustache_data.
    DATA lv_act  TYPE string.
    DATA lt_strings TYPE string_table.
    DATA lo_mustache TYPE REF TO lcl_mustache.
    DATA lx          TYPE REF TO zcx_mustache_error.

    CREATE OBJECT lo_data.
    APPEND 'hello' TO lt_strings.
    APPEND 'world' TO lt_strings.
    lo_data->add( iv_name = 'items' iv_val = lt_strings ).

    TRY .
      lo_mustache = lcl_mustache=>create(
        '{{#items}}'     && c_nl &&
        '-{{@tabline}}' && c_nl &&
        '{{/items}}' ).

      lv_act = lo_mustache->render( lo_data->get( ) ).

      cl_abap_unit_assert=>assert_equals(
        exp = '-hello' && c_nl && '-world' && c_nl
        act = lv_act ).
    CATCH zcx_mustache_error INTO lx.
      cl_abap_unit_assert=>fail( lx->msg ).
    ENDTRY.

  ENDMETHOD.

  METHOD render_with_object.

    DATA lo_data TYPE REF TO zcl_mustache_data.
    DATA lo_component TYPE REF TO ltcl_dummy_component.
    DATA lv_act  TYPE string.
    DATA lo_mustache TYPE REF TO lcl_mustache.
    DATA lx          TYPE REF TO zcx_mustache_error.

    TRY .
      lo_mustache = lcl_mustache=>create( '{{tag}}' ).
    CATCH zcx_mustache_error INTO lx.
      cl_abap_unit_assert=>fail( lx->msg ).
    ENDTRY.

    " Object
    CREATE OBJECT lo_component.
    CREATE OBJECT lo_data.
    lo_data->add( iv_name = 'tag' iv_val = lo_component ).
    TRY .
      lv_act = lo_mustache->render( lo_data->get( ) ).
      cl_abap_unit_assert=>assert_equals(
        exp = 'hello world'
        act = lv_act ).
    CATCH zcx_mustache_error INTO lx.
      cl_abap_unit_assert=>fail( lx->msg ).
    ENDTRY.

    " Interface
    DATA lo_comp_if TYPE REF TO ltcl_dummy_component_intf.
    DATA li_comp TYPE REF TO ltif_dummy_component.
    CREATE OBJECT lo_comp_if.
    CREATE OBJECT lo_data.
    li_comp = lo_comp_if.
    lo_data->add( iv_name = 'tag' iv_val = li_comp ).
    TRY .
      lv_act = lo_mustache->render( lo_data->get( ) ).
      cl_abap_unit_assert=>assert_equals(
        exp = 'hello world'
        act = lv_act ).
    CATCH zcx_mustache_error INTO lx.
      cl_abap_unit_assert=>fail( lx->msg ).
    ENDTRY.

  ENDMETHOD.

  METHOD render_oref_negative.

    DATA lo_data TYPE REF TO zcl_mustache_data.
    DATA lo_mustache TYPE REF TO lcl_mustache.
    DATA lx          TYPE REF TO zcx_mustache_error.

    TRY .
      lo_mustache = lcl_mustache=>create( '{{tag}}' ).
    CATCH zcx_mustache_error INTO lx.
      cl_abap_unit_assert=>fail( lx->msg ).
    ENDTRY.

    " no render
    DATA lo_comp1 TYPE REF TO ltcl_dummy_component_norend.
    CLEAR lx.
    CREATE OBJECT lo_comp1.
    CREATE OBJECT lo_data.
    lo_data->add( iv_name = 'tag' iv_val = lo_comp1 ).
    TRY .
      lo_mustache->render( lo_data->get( ) ).
    CATCH zcx_mustache_error INTO lx.
      cl_abap_unit_assert=>assert_equals(
        exp = 'ODHR'
        act = lx->rc ).
    ENDTRY.
    cl_abap_unit_assert=>assert_not_initial( lx ).

    " no return
    DATA lo_comp2 TYPE REF TO ltcl_dummy_component_noret.
    CLEAR lx.
    CREATE OBJECT lo_comp2.
    CREATE OBJECT lo_data.
    lo_data->add( iv_name = 'tag' iv_val = lo_comp2 ).
    TRY .
      lo_mustache->render( lo_data->get( ) ).
    CATCH zcx_mustache_error INTO lx.
      cl_abap_unit_assert=>assert_equals(
        exp = 'ORRS'
        act = lx->rc ).
    ENDTRY.
    cl_abap_unit_assert=>assert_not_initial( lx ).

    " mandatory param
    DATA lo_comp3 TYPE REF TO ltcl_dummy_component_manimp.
    CLEAR lx.
    CREATE OBJECT lo_comp3.
    CREATE OBJECT lo_data.
    lo_data->add( iv_name = 'tag' iv_val = lo_comp3 ).
    TRY .
      lo_mustache->render( lo_data->get( ) ).
    CATCH zcx_mustache_error INTO lx.
      cl_abap_unit_assert=>assert_equals(
        exp = 'ORMP'
        act = lx->rc ).
    ENDTRY.
    cl_abap_unit_assert=>assert_not_initial( lx ).

  ENDMETHOD.

ENDCLASS. "ltcl_mustache
