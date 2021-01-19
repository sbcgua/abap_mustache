class zcl_mustache definition
  public
  final
  create public .

  public section.

    interfaces zif_mustache .

    aliases add_partial
      for zif_mustache~add_partial .
    aliases get_partials
      for zif_mustache~get_partials .
    aliases get_tokens
      for zif_mustache~get_tokens .
    aliases render
      for zif_mustache~render .
    aliases render_tt
      for zif_mustache~render_tt .

    class-methods create
      importing
        !iv_template type string optional
        !it_template type string_table optional
        !iv_x_format type zif_mustache=>ty_x_format default cl_abap_format=>e_html_text
      preferred parameter iv_template
      returning
        value(ro_instance) type ref to zcl_mustache
      raising
        zcx_mustache_error .
    methods constructor
      importing
        !iv_template type string optional
        !it_template type string_table optional
        !iv_x_format type zif_mustache=>ty_x_format default cl_abap_format=>e_html_text
      preferred parameter iv_template
      raising
        zcx_mustache_error .
    class-methods check_version_fits
      importing
        !i_required_version type string
      returning
        value(r_fits) type abap_bool .
  protected section.
  private section.

    data mt_tokens   type zif_mustache=>ty_token_tt.
    data mt_partials type zif_mustache=>ty_partial_tt.
    data mv_x_format type zif_mustache=>ty_x_format.

ENDCLASS.



CLASS ZCL_MUSTACHE IMPLEMENTATION.


  method check_version_fits.

    r_fits = zcl_mustache_utils=>check_version_fits(
      i_current_version  = zif_mustache=>version
      i_required_version = i_required_version ).

  endmethod.


  method constructor.
    mv_x_format = iv_x_format.
    mt_tokens   = zcl_mustache_parser=>parse_template(
      iv_template = iv_template
      it_template = it_template ).
  endmethod.  " constructor.


  method create.
    create object ro_instance
      exporting
        iv_template = iv_template
        it_template = it_template
        iv_x_format = iv_x_format.
  endmethod. " create.


  method zif_mustache~add_partial.

    field-symbols <p> like line of mt_partials.

    if io_obj is not bound.
      zcx_mustache_error=>raise(
        msg = 'Partial object is not bound'
        rc  = 'PONB' ).
    endif.

    read table mt_partials transporting no fields with key name = iv_name.
    if sy-subrc = 0.
      zcx_mustache_error=>raise(
        msg = |Duplicate partial '{ iv_name }'|
        rc  = 'DP' ).
    endif.

    append initial line to mt_partials assigning <p>.
    <p>-name = iv_name.
    <p>-obj  = io_obj.

  endmethod.  " add_partial


  method zif_mustache~get_partials.
    rt_partials = mt_partials.
  endmethod.


  method zif_mustache~get_tokens.
    rt_tokens = mt_tokens.
  endmethod.


  method zif_mustache~render.

    data: lt_temp    type string_table,
          ls_statics type zcl_mustache_render=>ty_context.

    ls_statics-tokens   = mt_tokens.
    ls_statics-partials = mt_partials.
    ls_statics-x_format = mv_x_format.

    zcl_mustache_render=>render_section(
      exporting
        is_statics  = ls_statics
        i_data      = i_data
      changing
        ct_lines = lt_temp ).

    rv_text = zcl_mustache_utils=>join_strings( it_tab = lt_temp iv_sep = '' ).

  endmethod.  " render.


  method zif_mustache~render_tt.

    rt_tab = zcl_mustache_utils=>split_string(
      iv_sep  = cl_abap_char_utilities=>newline
      iv_text = render( i_data ) ).

  endmethod.  " render_tt.
ENDCLASS.
