*&---------------------------------------------------------------------*
*&  Include           ZABAP_MUSTACHE_PERF_TEST
*&---------------------------------------------------------------------*

CLASS lcl_mustache_perf_test DEFINITION FINAL.

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_size,
        size TYPE char4,
        qty  TYPE i,
      END OF ty_size,
      ty_size_tt TYPE STANDARD TABLE OF ty_size WITH DEFAULT KEY,

      BEGIN OF ty_item,
        name    TYPE string,
        dummy01 TYPE char4,
        dummy02 TYPE char4,
        dummy03 TYPE char4,
        dummy04 TYPE char4,
        dummy05 TYPE char4,
        dummy06 TYPE char4,
        dummy07 TYPE char4,
        dummy08 TYPE char4,
        dummy09 TYPE char4,
        dummy10 TYPE char4,
        price   TYPE s_price,
        sizes   TYPE ty_size_tt,
      END OF ty_item,
      ty_item_tt TYPE STANDARD TABLE OF ty_item WITH DEFAULT KEY,

      ty_tab_ty_struc_tt TYPE STANDARD TABLE OF zif_mustache=>ty_struc_tt.

    CLASS-METHODS do_perf_test.

  PRIVATE SECTION.

    CLASS-METHODS prepare_data_tab
      IMPORTING iv_count  TYPE i
      EXPORTING et_items  TYPE ty_item_tt
                et_uitems TYPE ty_tab_ty_struc_tt.

ENDCLASS.

CLASS lcl_mustache_perf_test IMPLEMENTATION.

  METHOD do_perf_test.

    CONSTANTS c_iterations TYPE i VALUE 1000.
    CONSTANTS c_renders    TYPE i VALUE 10.

    DATA:
          lo_mustache TYPE REF TO lcl_mustache,
          lt_data     TYPE zif_mustache=>ty_struc_tt,
          lv_act      TYPE string,
          lv_banner   TYPE string,
          lx          TYPE REF TO zcx_mustache_error.

    DATA: lv_sta_time     TYPE timestampl,
          lv_end_time     TYPE timestampl,
          lv_diff         TYPE p DECIMALS 6.

    DATA: lr_itab TYPE REF TO data,
          lr_utab TYPE REF TO data.

    FIELD-SYMBOLS: <data> LIKE LINE OF lt_data,
                   <itab> TYPE ty_item_tt,
                   <utab> TYPE ty_tab_ty_struc_tt.

    APPEND INITIAL LINE TO lt_data ASSIGNING <data>.
    <data>-name = 'shop'.
    <data>-val  = 'Shopsky'.
    APPEND INITIAL LINE TO lt_data ASSIGNING <data>.
    <data>-name = 'items'.

    CREATE DATA lr_itab TYPE ty_item_tt.
    CREATE DATA lr_utab TYPE ty_tab_ty_struc_tt.

    ASSIGN lr_itab->* TO <itab>.
    ASSIGN lr_utab->* TO <utab>.
    prepare_data_tab(
      EXPORTING iv_count  = c_iterations
      IMPORTING et_items  = <itab>
                et_uitems = <utab> ).


    TRY .
      lo_mustache = lcl_mustache=>create(
        'Welcome to {{shop}}'                                &&
        'Our sales:'                                         &&
        '{{#items}}'                                         &&
        '* {{name}} - ${{price}}'                            &&
        '  sizes: {{#sizes}}{{size}}[{{qty}}pc], {{/sizes}}' &&
        '{{/items}}' ).

      <data>-dref = lr_itab. " Internal table

      GET TIME STAMP FIELD lv_sta_time.
      DO c_renders TIMES.
        lv_act = lo_mustache->render( lt_data ).
      ENDDO.
      GET TIME STAMP FIELD lv_end_time.
      lv_diff = lv_end_time - lv_sta_time.

      lv_banner =
        |Perf test results itab:   { shift_left( val = |{ lv_diff }| sub = ` ` )
        } sec / {                    shift_left( val = |{ c_renders }| sub = ` ` )
        } x {                        shift_left( val = |{ c_iterations }| sub = ` ` )
        } iterations|.
      WRITE: / lv_banner.

      <data>-dref = lr_utab. " Universal structure table

      GET TIME STAMP FIELD lv_sta_time.
      DO c_renders TIMES.
        lv_act = lo_mustache->render( lt_data ).
      ENDDO.
      GET TIME STAMP FIELD lv_end_time.
      lv_diff = lv_end_time - lv_sta_time.

      lv_banner =
        |Perf test results unitab: { shift_left( val = |{ lv_diff }| sub = ` ` )
        } sec / {                    shift_left( val = |{ c_renders }| sub = ` ` )
        } x {                        shift_left( val = |{ c_iterations }| sub = ` ` )
        } iterations|.
      WRITE: / lv_banner.

      " Flip renders and iterations (many renders of small chunks)
      prepare_data_tab(
        EXPORTING iv_count  = c_renders
        IMPORTING et_items  = <itab>
                  et_uitems = <utab> ).

      <data>-dref = lr_itab. " Internal table

      GET TIME STAMP FIELD lv_sta_time.
      DO c_iterations TIMES.
        lv_act = lo_mustache->render( lt_data ).
      ENDDO.
      GET TIME STAMP FIELD lv_end_time.
      lv_diff = lv_end_time - lv_sta_time.

      lv_banner =
        |Perf test results itab:   { shift_left( val = |{ lv_diff }| sub = ` ` )
        } sec / {                    shift_left( val = |{ c_iterations }| sub = ` ` )
        } x {                        shift_left( val = |{ c_renders }| sub = ` ` )
        } iterations|.
      WRITE: / lv_banner.

    CATCH zcx_mustache_error INTO lx.
      WRITE: / 'Exception:', lx->msg.
    ENDTRY.

  ENDMETHOD.  "do_perf_test

  METHOD prepare_data_tab.

    FIELD-SYMBOLS: <item> LIKE LINE OF et_items,
                   <size> LIKE LINE OF <item>-sizes,
                   <utem> LIKE LINE OF et_uitems,
                   <urec> LIKE LINE OF <utem>.

    CLEAR: et_items, et_uitems.

    DO iv_count TIMES.
      APPEND INITIAL LINE TO et_items ASSIGNING <item>.
      <item>-name  = |Boots { sy-index }|.
      <item>-price = |{ sy-index }.00|.
      APPEND INITIAL LINE TO <item>-sizes ASSIGNING <size>.
      <size>-size = '40'.
      <size>-qty  = 1.
      APPEND INITIAL LINE TO <item>-sizes ASSIGNING <size>.
      <size>-size = '41'.
      <size>-qty  = 2.
      APPEND INITIAL LINE TO <item>-sizes ASSIGNING <size>.
      <size>-size = '42'.
      <size>-qty  = 3.
      APPEND INITIAL LINE TO <item>-sizes ASSIGNING <size>.
      <size>-size = '43'.
      <size>-qty  = 4.

      APPEND INITIAL LINE TO et_uitems ASSIGNING <utem>.
      APPEND INITIAL LINE TO <utem> ASSIGNING <urec>.
      <urec>-name = 'name'.
      <urec>-val  = <item>-name.
      APPEND INITIAL LINE TO <utem> ASSIGNING <urec>.
      <urec>-name = 'price'.
      <urec>-val  = <item>-price.
      APPEND INITIAL LINE TO <utem> ASSIGNING <urec>.
      <urec>-name = 'sizes'.
      GET REFERENCE OF <item>-sizes INTO <urec>-dref.

    ENDDO.

  ENDMETHOD. "prepare_data_tab

ENDCLASS.
