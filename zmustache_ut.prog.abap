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
*       CLASS ltcl_mustache_utils
*----------------------------------------------------------------------*

CLASS ltcl_mustache_utils DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS split_string FOR TESTING.
    METHODS join_strings FOR TESTING.
    methods check_version_fits for testing.

ENDCLASS. "ltcl_mustache_utils

CLASS ltcl_mustache_utils IMPLEMENTATION.

  METHOD split_string.

    DATA: lt_exp TYPE string_table.

    APPEND 'ABC' TO lt_exp.
    APPEND '123' TO lt_exp.

    cl_abap_unit_assert=>assert_equals(
      act = lcl_mustache_utils=>split_string(
      'ABC' && cl_abap_char_utilities=>cr_lf && '123' )
      exp = lt_exp ).

    cl_abap_unit_assert=>assert_equals(
      act = lcl_mustache_utils=>split_string(
      'ABC' && cl_abap_char_utilities=>newline && '123' )
      exp = lt_exp ).

    cl_abap_unit_assert=>assert_equals(
      act = lcl_mustache_utils=>split_string(
      iv_text = 'ABC' && 'X' && '123' iv_sep = 'X' )
      exp = lt_exp ).

  ENDMETHOD.  " split_string.

  METHOD join_strings.

    DATA: lt_src TYPE string_table.

    APPEND 'ABC' TO lt_src.
    APPEND '123' TO lt_src.

    cl_abap_unit_assert=>assert_equals(
      act = lcl_mustache_utils=>join_strings( lt_src )
      exp = 'ABC' && cl_abap_char_utilities=>newline && '123' ).

    cl_abap_unit_assert=>assert_equals(
      act = lcl_mustache_utils=>join_strings( it_tab = lt_src iv_sep = 'X' )
      exp = 'ABC' && 'X' && '123' ).

  ENDMETHOD. "join_strings

  method check_version_fits.
    cl_abap_unit_assert=>assert_true(
      zcl_text2tab_utils=>check_version_fits(
        i_current_version = 'v2.2.2'
        i_required_version = 'v2.2.2' ) ).
    cl_abap_unit_assert=>assert_true(
      zcl_text2tab_utils=>check_version_fits(
        i_current_version = 'v2.2.2'
        i_required_version = 'v2.1.2' ) ).
    cl_abap_unit_assert=>assert_true(
      zcl_text2tab_utils=>check_version_fits(
        i_current_version = 'v2.2.2'
        i_required_version = 'v1.0.0' ) ).
    cl_abap_unit_assert=>assert_false(
      zcl_text2tab_utils=>check_version_fits(
        i_current_version = 'v2.2.2'
        i_required_version = 'v2.2.3' ) ).
    cl_abap_unit_assert=>assert_false(
      zcl_text2tab_utils=>check_version_fits(
        i_current_version = 'v2.2.2'
        i_required_version = 'v2.2.30' ) ).
    cl_abap_unit_assert=>assert_false(
      zcl_text2tab_utils=>check_version_fits(
        i_current_version = 'v2.2.2'
        i_required_version = 'v2.3.1' ) ).
    cl_abap_unit_assert=>assert_false(
      zcl_text2tab_utils=>check_version_fits(
        i_current_version = 'v2.2.2'
        i_required_version = 'v3.0.0' ) ).
  endmethod.

ENDCLASS. "ltcl_mustache_utils

*----------------------------------------------------------------------*
*       CLASS ltcl_mustache
*----------------------------------------------------------------------*
CLASS ltcl_mustache DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_dummy,
        name TYPE string,
        am   TYPE abap_bool,
        pm   TYPE abap_bool,
        html TYPE string,
        tab  TYPE string_table,
        obj  TYPE REF TO lcl_mustache_utils,
      END OF ty_dummy,

      BEGIN OF ty_size,
        size TYPE char4,
        qty  TYPE i,
      END OF ty_size,
      ty_size_tt TYPE STANDARD TABLE OF ty_size WITH DEFAULT KEY,

      BEGIN OF ty_item,
        name  TYPE string,
        price TYPE string,
        sizes TYPE ty_size_tt,
      END OF ty_item,
      ty_item_tt TYPE STANDARD TABLE OF ty_item WITH DEFAULT KEY,

      BEGIN OF ty_tag_rc,
        val   TYPE string,
        rc    TYPE char4,
      END OF ty_tag_rc,
      ty_tag_rc_tt TYPE STANDARD TABLE OF ty_tag_rc WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_test_case,
        template     TYPE string,
        tokens       TYPE zif_mustache=>ty_token_tt,
        output       TYPE string,
        complex_test TYPE abap_bool,
      END OF ty_test_case,
      ty_test_case_tt TYPE STANDARD TABLE OF ty_test_case WITH DEFAULT KEY.

    CONSTANTS c_nl TYPE c VALUE cl_abap_char_utilities=>newline.

    CLASS-DATA gt_test_case_stash TYPE ty_test_case_tt READ-ONLY.

    CLASS-METHODS get_test_case
      IMPORTING iv_index    TYPE i OPTIONAL
      EXPORTING ev_count        TYPE i
                ev_complex_test TYPE abap_bool
                ev_template     TYPE string
                et_tokens       TYPE zif_mustache=>ty_token_tt
                ev_output       TYPE string.
    CLASS-METHODS get_test_data
      EXPORTING es_simple   TYPE ty_dummy
                et_complex  TYPE zif_mustache=>ty_struc_tt.

  PRIVATE SECTION.

    CLASS-METHODS class_setup.

    METHODS render_negative         FOR TESTING.
    METHODS render_w_partials       FOR TESTING.
    METHODS render_tt               FOR TESTING.
    METHODS add_partial             FOR TESTING.
    METHODS render_with_data_builder FOR TESTING.
    METHODS render_with_object      FOR TESTING.

ENDCLASS. "ltcl_mustache

CLASS ltcl_mustache IMPLEMENTATION.

  METHOD class_setup.

    FIELD-SYMBOLS: <t>     LIKE LINE OF gt_test_case_stash,
                   <token> LIKE LINE OF <t>-tokens.

    " Case 1
    APPEND INITIAL LINE TO gt_test_case_stash ASSIGNING <t>.
    <t>-template = 'Hello {{name}}!'.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-static      ''  1   `Hello `.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-etag        ''  1   'name'.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-static      ''  1   '!'.
    <t>-output = 'Hello Anonymous network user!'.

    " Case 2
    APPEND INITIAL LINE TO gt_test_case_stash ASSIGNING <t>.
    <t>-template = 'Hello {{name}}'.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-static      ''  1   `Hello `.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-etag        ''  1   'name'.
    <t>-output = 'Hello Anonymous network user'.

    " Case 3
    APPEND INITIAL LINE TO gt_test_case_stash ASSIGNING <t>.
    <t>-template = '{{name}} Hello'.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-etag        ''  1   'name'.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-static      ''  1   ` Hello`.
    <t>-output = 'Anonymous network user Hello'.

    " Case 4
    APPEND INITIAL LINE TO gt_test_case_stash ASSIGNING <t>.
    <t>-template = 'Good {{#pm}}afternoon{{/pm}}{{^pm}}morning{{/pm}}, {{name}}'.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-static      ''  1   `Good `.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-section     '=' 1   'pm'.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-static      ''  2   `afternoon`.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-section     '!' 1   'pm'.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-static      ''  2   `morning`.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-static      ''  1   `, `.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-etag        ''  1   'name'.
    <t>-output = 'Good afternoon, Anonymous network user'.

    " Case 5
    APPEND INITIAL LINE TO gt_test_case_stash ASSIGNING <t>.
    <t>-template = 'Good {{^am}}afternoon{{/am}}{{#am}}morning{{/am}}, {{name}}'.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-static      ''  1   `Good `.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-section     '!' 1   'am'.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-static      ''  2   `afternoon`.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-section     '=' 1   'am'.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-static      ''  2   `morning`.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-static      ''  1   `, `.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-etag        ''  1   'name'.
    <t>-output = 'Good afternoon, Anonymous network user'.

    " Case 6
    APPEND INITIAL LINE TO gt_test_case_stash ASSIGNING <t>.
    <t>-template = '{{!comment}}{{html}} {{{html}}} {{&html}}'.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-etag        ''  1   'html'.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-static      ''  1   ` `.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-utag        ''  1   'html'.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-static      ''  1   ` `.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-utag        ''  1   'html'.
    <t>-output = '&lt;tag&gt;&amp; <tag>& <tag>&'.

    " Case 7
    APPEND INITIAL LINE TO gt_test_case_stash ASSIGNING <t>.
    <t>-template = '{{pm}}{{=<* *>=}}<*pm*>{{xx}}<*={{ }}=*>{{pm}}'.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-etag        ''  1   'pm'.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-etag        ''  1   'pm'.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-static      ''  1   `{{xx}}`.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-etag        ''  1   'pm'.
    <t>-output = 'XX{{xx}}X'.

    " Case 8
    APPEND INITIAL LINE TO gt_test_case_stash ASSIGNING <t>.
    <t>-complex_test = abap_true.
    <t>-template = 'Welcome to {{shop}}'                    && "c_nl &&
                   'Our sales:'                             && "c_nl &&
                   '{{#items}}'                             && "c_nl &&
                   '* {{name}} - ${{price}}'                && "c_nl &&
                   '  sizes: {{#sizes}}{{size}},{{/sizes}}' && "c_nl &&
                   '{{/items}}'.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-static      ''  1   `Welcome to `.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-etag        ''  1   'shop'.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-static      ''  1   `Our sales:`.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-section     '=' 1   'items'.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-static      ''  2   `* `.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-etag        ''  2   'name'.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-static      ''  2   ` - $`.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-etag        ''  2   'price'.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-static      ''  2   `  sizes: `.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-section     '=' 2   'sizes'.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-etag        ''  3   'size'.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-static      ''  3   `,`.
    <t>-output   = 'Welcome to Shopsky'                     && "c_nl &&
                   'Our sales:'                             && "c_nl &&
                   '* Boots - $99.00'                       && "c_nl &&
                   '  sizes: 40,41,42,'                     && "c_nl &&
                   '* T-short - $49.00'                     && "c_nl &&
                   '  sizes: S,M,L,'.

    " Case 9 - newlines and lonely section
    APPEND INITIAL LINE TO gt_test_case_stash ASSIGNING <t>.
    <t>-complex_test = abap_true.
    <t>-template = 'Our sales:'                             && c_nl &&
                   `  {{#items}}  `                         && c_nl &&
                   '* {{name}} - ${{price}}'                && c_nl &&
                   `  {{/items}}  `.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-static      ''  1   `Our sales:`.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-static      ''  1   c_nl.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-section     '=' 1   'items'.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-static      ''  2   `* `.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-etag        ''  2   'name'.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-static      ''  2   ` - $`.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-etag        ''  2   'price'.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-static      ''  2   c_nl.
    <t>-output   = 'Our sales:'                             && c_nl &&
                   '* Boots - $99.00'                       && c_nl &&
                   '* T-short - $49.00'                     && c_nl.

    " Case 10
    APPEND INITIAL LINE TO gt_test_case_stash ASSIGNING <t>.
    <t>-template = '{{#tab}}{{@tabline}},{{/tab}}'.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-section     '=' 1   'tab'.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-etag        ''  2   '@tabline'.
    _add_mu_token <t>-tokens zif_mustache=>c_token_type-static      ''  2   `,`.
    <t>-output = 'line1,line2,'.

  ENDMETHOD.  " class_setup.

  METHOD get_test_case.

    FIELD-SYMBOLS: <t>     LIKE LINE OF gt_test_case_stash.

    IF ev_count IS REQUESTED.
      ev_count = lines( gt_test_case_stash ).
    ENDIF.

    IF iv_index IS INITIAL.
      RETURN. " Nothing else requested
    ENDIF.

    READ TABLE gt_test_case_stash INDEX iv_index ASSIGNING <t>.

    ev_complex_test = <t>-complex_test.
    ev_template     = <t>-template.
    et_tokens       = <t>-tokens.
    ev_output       = <t>-output.

  ENDMETHOD. "get_test_case

  METHOD get_test_data.

    FIELD-SYMBOLS: <data> LIKE LINE OF et_complex,
                   <tab>  TYPE ty_item_tt,
                   <item> LIKE LINE OF <tab>,
                   <size> TYPE ty_size.

    " Simple data
    es_simple-name = 'Anonymous network user'.
    es_simple-am   = abap_false.
    es_simple-pm   = abap_true.
    es_simple-html = '<tag>&'.
    CREATE OBJECT es_simple-obj.
    APPEND 'line1' TO es_simple-tab.
    APPEND 'line2' TO es_simple-tab.

    " Complex data
    CLEAR et_complex.

    APPEND INITIAL LINE TO et_complex ASSIGNING <data>.
    <data>-name = 'shop'.
    <data>-val  = 'Shopsky'.
    APPEND INITIAL LINE TO et_complex ASSIGNING <data>.
    <data>-name = 'items'.
    CREATE DATA <data>-dref TYPE ty_item_tt.

    ASSIGN <data>-dref->* TO <tab>.

    " Boots
    APPEND INITIAL LINE TO <tab> ASSIGNING <item>.
    <item>-name  = 'Boots'.
    <item>-price = '99.00'.
    APPEND INITIAL LINE TO <item>-sizes ASSIGNING <size>.
    <size>-size = '40'.
    <size>-qty  = 8.
    APPEND INITIAL LINE TO <item>-sizes ASSIGNING <size>.
    <size>-size = '41'.
    <size>-qty  = 12.
    APPEND INITIAL LINE TO <item>-sizes ASSIGNING <size>.
    <size>-size = '42'.
    <size>-qty  = 3.

    "T-short
    APPEND INITIAL LINE TO <tab> ASSIGNING <item>.
    <item>-name  = 'T-short'.
    <item>-price = '49.00'.
    APPEND INITIAL LINE TO <item>-sizes ASSIGNING <size>.
    <size>-size = 'S'.
    <size>-qty  = 15.
    APPEND INITIAL LINE TO <item>-sizes ASSIGNING <size>.
    <size>-size = 'M'.
    <size>-qty  = 23.
    APPEND INITIAL LINE TO <item>-sizes ASSIGNING <size>.
    <size>-size = 'L'.
    <size>-qty  = 18.

  ENDMETHOD. "get_test_data

  METHOD render_w_partials.

    DATA:
          lo_mustache TYPE REF TO lcl_mustache,
          lt_data     TYPE zif_mustache=>ty_struc_tt,
          lv_exp      TYPE string,
          lv_act      TYPE string,
          lx          TYPE REF TO zcx_mustache_error.

    get_test_data( IMPORTING et_complex  = lt_data ).
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

    get_test_data( IMPORTING et_complex  = lt_data ).

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
          lt_tab   TYPE ty_tag_rc_tt,
          ls_dummy TYPE ty_dummy,
          lo_mustache TYPE REF TO lcl_mustache,
          lv_idx          TYPE i,
          lx              TYPE REF TO zcx_mustache_error.

    FIELD-SYMBOLS <rcline> LIKE LINE OF lt_tab.

    get_test_data( IMPORTING es_simple = ls_dummy ).

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
          ls_dummy    TYPE ty_dummy,
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

ENDCLASS. "ltcl_mustache

CLASS ltcl_mustache_parser DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.


  PRIVATE SECTION.

    CONSTANTS c_nl TYPE c VALUE cl_abap_char_utilities=>newline.

    METHODS parse_template          FOR TESTING.
    METHODS parse_template_table    FOR TESTING.
    METHODS parse_template_negative FOR TESTING.
    METHODS parse_tag               FOR TESTING.
    METHODS parse_tag_negative      FOR TESTING.

ENDCLASS.

CLASS ltcl_mustache_parser IMPLEMENTATION.

  METHOD parse_template.

    DATA:
          lt_exp      TYPE zif_mustache=>ty_token_tt,
          lt_act      TYPE zif_mustache=>ty_token_tt,
          lv_count    TYPE i,
          lv_idx      TYPE i,
          lv_template TYPE string,
          lx          TYPE REF TO zcx_mustache_error.

    ltcl_mustache=>get_test_case( IMPORTING ev_count = lv_count ).

    DO lv_count TIMES.
      lv_idx = sy-index.
      ltcl_mustache=>get_test_case( EXPORTING iv_index    = lv_idx
                     IMPORTING ev_template = lv_template
                               et_tokens   = lt_exp ).

      TRY .
        lt_act = lcl_mustache_parser=>parse_template( lv_template ).
        cl_abap_unit_assert=>assert_equals(
          exp = lt_exp
          act = lt_act
          msg = |parse_template, case { lv_idx }| ).
      CATCH zcx_mustache_error INTO lx.
        cl_abap_unit_assert=>fail( lx->msg ).
      ENDTRY.

    ENDDO.

  ENDMETHOD.  " parse_template.

  METHOD parse_template_negative.

    DATA:
          lt_tab TYPE ltcl_mustache=>ty_tag_rc_tt,
          lx     TYPE REF TO zcx_mustache_error.

    FIELD-SYMBOLS <rcline> LIKE LINE OF lt_tab.

    _add_mu_val_rc lt_tab 'Hello {{name}!'               'CTNF'.
    _add_mu_val_rc lt_tab 'Good {{#pm}}afternoon'        'SNC'.
    _add_mu_val_rc lt_tab 'Good afternoon{{/pm}}'        'CNOS'.
    _add_mu_val_rc lt_tab 'Good {{#pm}}afternoon{{/am}}' 'CSM'.

    LOOP AT lt_tab ASSIGNING <rcline>.
      CLEAR lx.
      TRY .
        lcl_mustache_parser=>parse_template( <rcline>-val ).
      CATCH zcx_mustache_error INTO lx.
        ASSERT 1 = 1.
      ENDTRY.
      cl_abap_unit_assert=>assert_not_initial( act = lx ).
      cl_abap_unit_assert=>assert_equals( exp = <rcline>-rc act = lx->rc ).
    ENDLOOP.

  ENDMETHOD. "parse_template_negative

  METHOD parse_template_table.

    DATA:
          lt_exp      TYPE zif_mustache=>ty_token_tt,
          lt_act      TYPE zif_mustache=>ty_token_tt,
          lt_template TYPE string_table,
          lx          TYPE REF TO zcx_mustache_error.

    FIELD-SYMBOLS <token> LIKE LINE OF lt_exp.

    APPEND 'Our sales:'              TO lt_template.
    APPEND '{{#items}}'              TO lt_template.
    APPEND '* {{name}} - ${{price}}' TO lt_template.
    APPEND '{{/items}}'              TO lt_template.

    _add_mu_token lt_exp zif_mustache=>c_token_type-static      ''  1   `Our sales:`.
    _add_mu_token lt_exp zif_mustache=>c_token_type-static      ''  1   c_nl.
    _add_mu_token lt_exp zif_mustache=>c_token_type-section     '=' 1   'items'.
    _add_mu_token lt_exp zif_mustache=>c_token_type-static      ''  2   `* `.
    _add_mu_token lt_exp zif_mustache=>c_token_type-etag        ''  2   'name'.
    _add_mu_token lt_exp zif_mustache=>c_token_type-static      ''  2   ` - $`.
    _add_mu_token lt_exp zif_mustache=>c_token_type-etag        ''  2   'price'.
    _add_mu_token lt_exp zif_mustache=>c_token_type-static      ''  2   c_nl.

    TRY.
      lt_act = lcl_mustache_parser=>parse_template( it_template = lt_template ).
    CATCH zcx_mustache_error INTO lx.
      cl_abap_unit_assert=>fail( lx->msg ).
    ENDTRY.
    cl_abap_unit_assert=>assert_equals( exp = lt_exp act = lt_act ).

  ENDMETHOD.  " parse_template_table

  METHOD parse_tag.

    DATA:
          lt_exp TYPE zif_mustache=>ty_token_tt,
          lt_act TYPE zif_mustache=>ty_token_tt,
          lx TYPE REF TO zcx_mustache_error.

    FIELD-SYMBOLS <token> LIKE LINE OF lt_exp.

    TRY .
      APPEND lcl_mustache_parser=>parse_tag( 'name' ) TO lt_act.
      APPEND lcl_mustache_parser=>parse_tag( '{ name }' ) TO lt_act.
      APPEND lcl_mustache_parser=>parse_tag( '&name' ) TO lt_act.
      APPEND lcl_mustache_parser=>parse_tag( '#name' ) TO lt_act.
      APPEND lcl_mustache_parser=>parse_tag( '# name ' ) TO lt_act.
      APPEND lcl_mustache_parser=>parse_tag( '^name' ) TO lt_act.
      APPEND lcl_mustache_parser=>parse_tag( '/name' ) TO lt_act.
      APPEND lcl_mustache_parser=>parse_tag( '!name' ) TO lt_act.
      APPEND lcl_mustache_parser=>parse_tag( '= {*  *} =' ) TO lt_act.
    CATCH zcx_mustache_error INTO lx.
      cl_abap_unit_assert=>fail( lx->msg ).
    ENDTRY.

    "                    TYPE                                  COND LEV CONTENT
    _add_mu_token lt_exp zif_mustache=>c_token_type-etag        ''  0   'name'.
    _add_mu_token lt_exp zif_mustache=>c_token_type-utag        ''  0   'name'.
    _add_mu_token lt_exp zif_mustache=>c_token_type-utag        ''  0   'name'.
    _add_mu_token lt_exp zif_mustache=>c_token_type-section     '=' 0   'name'.
    _add_mu_token lt_exp zif_mustache=>c_token_type-section     '=' 0   'name'.
    _add_mu_token lt_exp zif_mustache=>c_token_type-section     '!' 0   'name'.
    _add_mu_token lt_exp zif_mustache=>c_token_type-section_end ''  0   'name'.
    _add_mu_token lt_exp zif_mustache=>c_token_type-comment     ''  0   'name'.
    _add_mu_token lt_exp zif_mustache=>c_token_type-delimiter   ''  0   '{* *}'. " Boobs :)

    cl_abap_unit_assert=>assert_equals( exp = lt_exp act = lt_act ).

  ENDMETHOD. "parse_tag

  METHOD parse_tag_negative.

    DATA:
          lt_tab TYPE ltcl_mustache=>ty_tag_rc_tt,
          lx     TYPE REF TO zcx_mustache_error.

    FIELD-SYMBOLS <rcline> LIKE LINE OF lt_tab.

    _add_mu_val_rc lt_tab ''        'ET'.
    _add_mu_val_rc lt_tab '{}'      'ET'.
    _add_mu_val_rc lt_tab '   '     'ET'.
    _add_mu_val_rc lt_tab '{   }'   'ET'.
    _add_mu_val_rc lt_tab '{name'   'MC}'.
    _add_mu_val_rc lt_tab '=name'   'MC='.
    _add_mu_val_rc lt_tab '#'       'ET'.
    _add_mu_val_rc lt_tab '=xxx='   'CDF'.
    _add_mu_val_rc lt_tab '=x x x=' 'CDF'.

    LOOP AT lt_tab ASSIGNING <rcline>.
      CLEAR lx.
      TRY .
        lcl_mustache_parser=>parse_tag( <rcline>-val ).
      CATCH zcx_mustache_error INTO lx.
        ASSERT 1 = 1.
      ENDTRY.
      cl_abap_unit_assert=>assert_not_initial( act = lx ).
      cl_abap_unit_assert=>assert_equals( exp = <rcline>-rc act = lx->rc ).
    ENDLOOP.

  ENDMETHOD. "parse_tag_negative

ENDCLASS.

CLASS ltcl_mustache_render DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.

    METHODS find_value     FOR TESTING.
    METHODS render_section FOR TESTING.
    METHODS render_oref_negative FOR TESTING.

ENDCLASS.

CLASS ltcl_mustache_render IMPLEMENTATION.

  METHOD find_value.

    DATA:
          lr            TYPE REF TO data,
          lt_data_stack TYPE zif_mustache=>ty_ref_tt,
          ls_dummy      TYPE zif_mustache=>ty_struc,
          lt_dummy      TYPE zif_mustache=>ty_struc_tt,
          lv_act        TYPE string,
          lx            TYPE REF TO zcx_mustache_error.

    ls_dummy-name = 'abc'.
    ls_dummy-val  = '123'.
    APPEND ls_dummy TO lt_dummy.

    "1----------------
    CLEAR lt_data_stack.
    GET REFERENCE OF ls_dummy INTO lr.
    APPEND lr TO lt_data_stack.

    TRY .
      lv_act = lcl_mustache_render=>find_value( it_data_stack = lt_data_stack iv_name = 'NAME' ).
      cl_abap_unit_assert=>assert_equals( exp = 'abc' act = lv_act ).
    CATCH zcx_mustache_error INTO lx.
      cl_abap_unit_assert=>fail( lx->msg ).
    ENDTRY.

    "2----------------
    CLEAR lt_data_stack.
    GET REFERENCE OF lt_dummy INTO lr.
    APPEND lr TO lt_data_stack.

    TRY .
      lv_act = lcl_mustache_render=>find_value( it_data_stack = lt_data_stack iv_name = 'Abc' ).
      cl_abap_unit_assert=>assert_equals( exp = '123' act = lv_act ).
    CATCH zcx_mustache_error INTO lx.
      cl_abap_unit_assert=>fail( lx->msg ).
    ENDTRY.

    "3----------------
    CLEAR lt_data_stack.
    GET REFERENCE OF ls_dummy INTO lr.
    APPEND lr TO lt_data_stack.
    GET REFERENCE OF lt_dummy INTO lr.
    APPEND lr TO lt_data_stack.

    TRY .
      lv_act = lcl_mustache_render=>find_value( it_data_stack = lt_data_stack iv_name = 'Abc' ).
      cl_abap_unit_assert=>assert_equals( exp = '123' act = lv_act ).
      lv_act = lcl_mustache_render=>find_value( it_data_stack = lt_data_stack iv_name = 'name' ).
      cl_abap_unit_assert=>assert_equals( exp = 'abc' act = lv_act ).
    CATCH zcx_mustache_error INTO lx.
      cl_abap_unit_assert=>fail( lx->msg ).
    ENDTRY.

  ENDMETHOD.  " find_value.

  METHOD render_section.

    DATA:
          ls_statics      TYPE lcl_mustache=>ty_context,
          ls_simple       TYPE ltcl_mustache=>ty_dummy,
          lt_complex      TYPE zif_mustache=>ty_struc_tt,
          lv_count        TYPE i,
          lv_idx          TYPE i,
          iv_complex_test TYPE abap_bool,
          lv_exp          TYPE string,
          lv_act          TYPE string,
          lt_act          TYPE string_table,
          lx              TYPE REF TO zcx_mustache_error.

    ltcl_mustache=>get_test_case( IMPORTING ev_count = lv_count ).
    ltcl_mustache=>get_test_data( IMPORTING es_simple   = ls_simple
                             et_complex  = lt_complex ).
    ls_statics-x_format = cl_abap_format=>e_html_text.

    DO lv_count TIMES.
      lv_idx = sy-index.
      ltcl_mustache=>get_test_case(
        EXPORTING
          iv_index        = lv_idx
        IMPORTING
          ev_output       = lv_exp
          et_tokens       = ls_statics-tokens
          ev_complex_test = iv_complex_test ).

      CLEAR lt_act.
      TRY .
        IF iv_complex_test = abap_true.
          lcl_mustache_render=>render_section(
            EXPORTING
              is_statics = ls_statics
              i_data     = lt_complex
            CHANGING
              ct_lines   = lt_act ).
        ELSE.
          lcl_mustache_render=>render_section(
            EXPORTING
              is_statics = ls_statics
              i_data     = ls_simple
            CHANGING
              ct_lines   = lt_act ).
        ENDIF.
        lv_act = lcl_mustache_utils=>join_strings( it_tab = lt_act iv_sep = '' ).


        cl_abap_unit_assert=>assert_equals(
          exp = lv_exp
          act = lv_act
          msg = |render_section, case { lv_idx }| ).
      CATCH zcx_mustache_error INTO lx.
        cl_abap_unit_assert=>fail( lx->msg ).
      ENDTRY.

    ENDDO.

  ENDMETHOD. "render_section

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

ENDCLASS.
