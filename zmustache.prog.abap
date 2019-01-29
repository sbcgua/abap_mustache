*/--------------------------------------------------------------------------------\
*| This file is part of ABAP MUSTACHE                                             |
*|                                                                                |
*| The MIT License (MIT)                                                          |
*|                                                                                |
*| Copyright (c) 2018 Alexander Tsybulsky (atsybulsky@sbcg.com.ua)                |
*|                                                                                |
*| Permission is hereby granted, free of charge, to any person obtaining a copy   |
*| of this software and associated documentation files (the "Software"), to deal  |
*| in the Software without restriction, including without limitation the rights   |
*| to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      |
*| copies of the Software, and to permit persons to whom the Software is          |
*| furnished to do so, subject to the following conditions:                       |
*|                                                                                |
*| The above copyright notice and this permission notice shall be included in all |
*| copies or substantial portions of the Software.                                |
*|                                                                                |
*| THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     |
*| IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       |
*| FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    |
*| AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         |
*| LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  |
*| OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  |
*| SOFTWARE.                                                                      |
*\--------------------------------------------------------------------------------/
*| project homepage: https://github.com/sbcgua/abap_mustache                      |
*\--------------------------------------------------------------------------------/

**********************************************************************
* MUSTACHE LOGIC
**********************************************************************

*----------------------------------------------------------------------*
*       CLASS lcl_mustache DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_mustache DEFINITION FINAL.

  PUBLIC SECTION.

    INTERFACES zif_mustache.
    ALIASES:
      render FOR zif_mustache~render,
      render_tt FOR zif_mustache~render_tt,
      add_partial FOR zif_mustache~add_partial,
      get_partials FOR zif_mustache~get_partials,
      get_tokens FOR zif_mustache~get_tokens.


    " METHODS **********************************************************

    CLASS-METHODS create
      IMPORTING
        iv_template TYPE string       OPTIONAL
        it_template TYPE string_table OPTIONAL
        iv_x_format TYPE zif_mustache=>ty_x_format  DEFAULT cl_abap_format=>e_html_text
      PREFERRED
        PARAMETER iv_template
      RETURNING
        VALUE(ro_instance) TYPE REF TO lcl_mustache
      RAISING
        zcx_mustache_error.

    METHODS constructor
      IMPORTING
        iv_template TYPE string       OPTIONAL
        it_template TYPE string_table OPTIONAL
        iv_x_format TYPE zif_mustache=>ty_x_format  DEFAULT cl_abap_format=>e_html_text
      PREFERRED PARAMETER iv_template
      RAISING
        zcx_mustache_error.

    class-methods CHECK_VERSION_FITS
      importing
        !I_REQUIRED_VERSION type STRING
      returning
        value(R_FITS) type ABAP_BOOL .

  PRIVATE SECTION.
    DATA mt_tokens   TYPE zif_mustache=>ty_token_tt.
    DATA mt_partials TYPE zif_mustache=>ty_partial_tt.
    DATA mv_x_format TYPE zif_mustache=>ty_x_format.

ENDCLASS. "lcl_mustache

*----------------------------------------------------------------------*
*       CLASS lcl_mustache IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_mustache IMPLEMENTATION.

  METHOD create.
    CREATE OBJECT ro_instance
      EXPORTING
        iv_template = iv_template
        it_template = it_template
        iv_x_format = iv_x_format.
  ENDMETHOD. " create.

  METHOD constructor.
    mv_x_format = iv_x_format.
    mt_tokens   = zcl_mustache_parser=>parse_template(
      iv_template = iv_template
      it_template = it_template ).
  ENDMETHOD.  " constructor.

  METHOD render_tt.

    rt_tab = zcl_mustache_utils=>split_string(
      iv_sep  = cl_abap_char_utilities=>newline
      iv_text = render( i_data ) ).

  ENDMETHOD.  " render_tt.

  METHOD render.

    DATA: lt_temp    TYPE string_table,
          ls_statics TYPE zcl_mustache_render=>ty_context.

    ls_statics-tokens   = mt_tokens.
    ls_statics-partials = mt_partials.
    ls_statics-x_format = mv_x_format.

    zcl_mustache_render=>render_section(
      EXPORTING
        is_statics  = ls_statics
        i_data      = i_data
      CHANGING
        ct_lines = lt_temp ).

    rv_text = zcl_mustache_utils=>join_strings( it_tab = lt_temp iv_sep = '' ).

  ENDMETHOD.  " render.

  METHOD add_partial.

    FIELD-SYMBOLS <p> LIKE LINE OF mt_partials.

    IF io_obj IS NOT BOUND.
      zcx_mustache_error=>raise(
        msg = 'Partial object is not bound'
        rc  = 'PONB' ).
    ENDIF.

    READ TABLE mt_partials TRANSPORTING NO FIELDS WITH KEY name = iv_name.
    IF sy-subrc = 0.
      zcx_mustache_error=>raise(
        msg = |Duplicate partial '{ iv_name }'|
        rc  = 'DP' ).
    ENDIF.

    APPEND INITIAL LINE TO mt_partials ASSIGNING <p>.
    <p>-name = iv_name.
    <p>-obj  = io_obj.

  ENDMETHOD.  " add_partial

  METHOD get_partials.
    rt_partials = mt_partials.
  ENDMETHOD.

  METHOD get_tokens.
    rt_tokens = mt_tokens.
  ENDMETHOD.

  method CHECK_VERSION_FITS.

    r_fits = zcl_mustache_utils=>check_version_fits(
      i_current_version  = zif_mustache=>version
      i_required_version = i_required_version ).

  endmethod.

ENDCLASS. "lcl_mustache
