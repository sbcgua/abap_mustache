class zcl_mustache_render definition
  public
  final
  create public .

  public section.

    types:
      begin of ty_context,
          tokens     type zif_mustache=>ty_token_tt,
          partials   type zif_mustache=>ty_partial_tt,
          x_format   type zif_mustache=>ty_x_format,
          part_depth type i,
        end of ty_context .

    " Max depth of partials recursion, feel free to change for your needs
    constants c_max_partials_depth type i value 10.         "#EC NOTEXT
    constants:
      begin of c_data_type,
          elem  type char10 value 'IFPCDNTg',
          struc type char2  value 'uv',
          table type char1  value 'h',
          oref  type char1  value 'r',
        end of c_data_type .

    class-methods render_section
      importing
        !is_statics type ty_context
        !i_data type any
        !it_data_stack type zif_mustache=>ty_ref_tt optional
        !iv_start_idx type i default 1
        !iv_path type string default '/'
      changing
        !ct_lines type string_table
      raising
        zcx_mustache_error .
    class-methods render_loop
      importing
        !is_statics type ty_context
        !it_data_stack type zif_mustache=>ty_ref_tt
        !iv_start_idx type i
        !iv_path type string
      changing
        !ct_lines type string_table
      raising
        zcx_mustache_error .
    class-methods find_value
      importing
        !iv_name type string
        !it_data_stack type zif_mustache=>ty_ref_tt
      returning
        value(rv_val) type string
      raising
        zcx_mustache_error .
    class-methods find_walker
      importing
        !iv_name type string
        !it_data_stack type zif_mustache=>ty_ref_tt
        !iv_level type i
      returning
        value(rv_ref) type ref to data
      raising
        zcx_mustache_error .
    class-methods eval_condition
      importing
        !iv_var type any
        !iv_cond type zif_mustache=>ty_token-cond
      returning
        value(rv_result) type abap_bool
      raising
        zcx_mustache_error .
    class-methods render_oref
      importing
        !io_obj type ref to object
        !iv_tag_name type string optional
      returning
        value(rv_val) type string
      raising
        zcx_mustache_error .
    class-methods class_constructor .
  protected section.
  private section.

    class-data: c_ty_struc_tt_absolute_name type abap_abstypename.
ENDCLASS.



CLASS ZCL_MUSTACHE_RENDER IMPLEMENTATION.


  method class_constructor.
    data lt_dummy type zif_mustache=>ty_struc_tt.
    c_ty_struc_tt_absolute_name = cl_abap_typedescr=>describe_by_data( lt_dummy )->absolute_name.
  endmethod. "class_constructor


  method eval_condition.

    data lv_type type c.
    data lv_bool type abap_bool.

    describe field iv_var type lv_type.

    lv_bool = boolc( iv_var is not initial
      and not ( lv_type = 'g' and iv_var = ` ` ) ). " boolc into string, see #10

    case iv_cond.
      when zif_mustache=>c_section_condition-if.
        rv_result = lv_bool.

      when zif_mustache=>c_section_condition-ifnot.
        rv_result = boolc( lv_bool = abap_false ).

      when others.
        zcx_mustache_error=>raise(
          msg = |Unknown condition '{ iv_cond }'|
          rc  = 'UC' ).
    endcase.

  endmethod.


  method find_value.

    data: lr      type ref to data,
          lv_type type c.

    field-symbols: <field> type any.

    lr = find_walker( iv_name       = iv_name
                      it_data_stack = it_data_stack
                      iv_level      = lines( it_data_stack ) ). " Start from deepest level

    assign lr->* to <field>.
    describe field <field> type lv_type.

    if lv_type ca c_data_type-elem. " Element data type
      rv_val = |{ <field> }|.
    elseif lv_type ca c_data_type-oref. " Object or interface instance
      rv_val = render_oref( iv_tag_name = iv_name io_obj = <field> ).
    else.
      zcx_mustache_error=>raise(
        msg = |Cannot convert { iv_name } to string|
        rc  = 'CCTS' ).
    endif.


  endmethod.  " find_value.


  method find_walker.

    data:
          lo_type       type ref to cl_abap_typedescr,
          lv_type       type c,
          lv_found      type abap_bool,
          lv_name_upper type string,
          lr            type ref to data.

    field-symbols: <field> type any,
                   <struc> type any,
                   <fname> type string,
                   <rec>   type zif_mustache=>ty_struc,
                   <table> type any table.

    lv_name_upper = to_upper( iv_name ).

    read table it_data_stack into lr index iv_level.
    assign lr->* to <field>.
    describe field <field> type lv_type.
    unassign <field>.

    if lv_type ca c_data_type-elem. " Element data type.
      " Assuming value can happen just at the lowest level
      " Then just ignore but don't throw an error
      " it will be found by name in higher context levels

      " Except special item @tabline
      " It assumes iterating on table of elements
      if lv_name_upper = '@TABLINE'.
        rv_ref = lr.
        lv_found = abap_true.
      endif.

    elseif lv_type ca c_data_type-struc.   " Structure
      assign lr->* to <struc>.
      assign component lv_name_upper of structure <struc> to <field>.
      if sy-subrc = 0. " Found
        get reference of <field> into rv_ref.
        lv_found = abap_true.
      endif.

    elseif lv_type ca c_data_type-table.    " Table
      lo_type = cl_abap_typedescr=>describe_by_data_ref( lr ).
      if lo_type->absolute_name <> c_ty_struc_tt_absolute_name.
        zcx_mustache_error=>raise(
          msg = |Cannot find values in tables other than of ty_struc_tt type|
          rc  = 'WTT' ).
      endif.

      assign lr->* to <table>.
      loop at <table> assigning <rec>.
        if to_upper( <rec>-name ) = lv_name_upper.
          lv_found = abap_true.
          if <rec>-dref is not initial.
            rv_ref = <rec>-dref.
          elseif <rec>-oref is not initial.
            get reference of <rec>-oref into rv_ref.
          else.
            get reference of <rec>-val into rv_ref.
          endif.
          exit.
        endif.
      endloop.

    else.                     " Anything else is unsupported
      zcx_mustache_error=>raise(
        msg = |Can find values in structures or ty_struc_tt tables only|
        rc  = 'WVT' ).
    endif.

    if lv_found <> abap_true.
      if iv_level > 1.
        " Search upper levels
        rv_ref = find_walker( iv_name       = iv_name
                              it_data_stack = it_data_stack
                              iv_level      = iv_level - 1 ).
      else.
        zcx_mustache_error=>raise(
          msg = |Field '{ iv_name }' not found in supplied data|
          rc  = 'FNF' ).
      endif.
    endif.

  endmethod. "find_walker


  method render_loop.

    data:
          lr         type ref to data,
          ls_statics type ty_context,
          lv_level   type i,
          lv_idx     type i,
          lt_buf     type string_table,
          lv_has_words        type abap_bool,
          lv_has_tags         type abap_bool,
          lv_last_content_idx type i,
          lv_val     type string.

    field-symbols: <field>   type any,
                   <partial> like line of is_statics-partials,
                   <token>   like line of is_statics-tokens.

    " Rendering loop
    loop at is_statics-tokens assigning <token> from iv_start_idx.
      lv_idx = sy-tabix.

      assert <token>-level > 0. " Protection from programming errors
      if lv_level = 0.
        lv_level = <token>-level. " Copy from first record
      endif.

      if <token>-level < lv_level. " Reached end of section
        exit.
      endif.

      if <token>-level > lv_level. " Processed by deeper sections
        continue.
      endif.

      if <token>-type <> zif_mustache=>c_token_type-static.
        lv_has_tags = abap_true.
      endif.

      case <token>-type.
        when zif_mustache=>c_token_type-static.                     " Static particle
          if <token>-content = cl_abap_char_utilities=>newline.
            if lv_has_words = abap_true or lv_has_tags = abap_false or lv_last_content_idx < lines( lt_buf ).
              " has non-space statics or did not have tags (so empty line is intended) or some tags rendered
              append lines of lt_buf to ct_lines.
              append <token>-content to ct_lines.
            endif.
            clear: lt_buf, lv_has_words, lv_has_tags, lv_last_content_idx.
          else.
            append <token>-content to lt_buf.
            lv_last_content_idx = lines( lt_buf ).
            if lv_has_words = abap_false and <token>-content cn ` `.
              lv_has_words = abap_true.
            endif.
          endif.

        when zif_mustache=>c_token_type-etag or zif_mustache=>c_token_type-utag.  " Single tag
          lv_val  = find_value( iv_name = <token>-content  it_data_stack = it_data_stack ).
          if <token>-type = zif_mustache=>c_token_type-etag and is_statics-x_format is not initial.
            lv_val = escape( val = lv_val format = is_statics-x_format ).
          endif.
          append lv_val to lt_buf.

        when zif_mustache=>c_token_type-section.
          lr = find_walker( iv_name       = <token>-content
                            it_data_stack = it_data_stack
                            iv_level      = lines( it_data_stack ) ). " Start from deepest level
          assign lr->* to <field>.

          if abap_true = eval_condition( iv_var = <field> iv_cond = <token>-cond ).
            render_section(
              exporting
                is_statics    = is_statics
                it_data_stack = it_data_stack
                iv_start_idx  = lv_idx + 1
                i_data        = <field>
                iv_path       = iv_path && <token>-content && '/'
              changing
                ct_lines      = lt_buf ).
          endif.

        when zif_mustache=>c_token_type-partial.
          if is_statics-part_depth = c_max_partials_depth.
            zcx_mustache_error=>raise(
              msg = |Max partials depth reached ({ c_max_partials_depth })|
              rc  = 'MPD' ).
          endif.

          read table is_statics-partials assigning <partial> with key name = <token>-content.
          if sy-subrc > 0.
            zcx_mustache_error=>raise(
              msg = |Partial '{ <token>-content }' not found|
              rc  = 'PNF' ).
          endif.

          ls_statics-tokens      = <partial>-obj->get_tokens( ).
          ls_statics-partials    = <partial>-obj->get_partials( ).
          ls_statics-x_format    = is_statics-x_format.
          ls_statics-part_depth  = is_statics-part_depth + 1.

          render_loop(
            exporting
              is_statics    = ls_statics
              it_data_stack = it_data_stack
              iv_start_idx  = 1 " Start from start
              iv_path       = iv_path && '>' && <token>-content && '/'
            changing
              ct_lines      = lt_buf ).

        when others.
          assert 0 = 1. " Cannot reach, programming error
      endcase.

    endloop.

    append lines of lt_buf to ct_lines.

  endmethod.  " render_loop.


  method render_oref.

    data lo_type type ref to cl_abap_objectdescr.
    data ls_meth like line of lo_type->methods.
    data ls_param like line of ls_meth-parameters.
    data lv_meth_found type abap_bool.
    data lv_if type abap_methname.
    data lv_if_meth type abap_methname.

    lo_type ?= cl_abap_objectdescr=>describe_by_object_ref( io_obj ).

    loop at lo_type->methods into ls_meth.
      split ls_meth-name at '~' into lv_if lv_if_meth.
      if lv_if = 'RENDER' and lv_if_meth is initial or lv_if_meth = 'RENDER'.
        lv_meth_found = abap_true.
        exit.
      endif.
    endloop.

    if lv_meth_found = abap_false.
      zcx_mustache_error=>raise(
        msg = |Object does not have render method for tag '{ iv_tag_name }'|
        rc  = 'ODHR' ).
    endif.

    read table ls_meth-parameters transporting no fields
      with key parm_kind = 'I' is_optional = ''.
    if sy-subrc is initial.
      zcx_mustache_error=>raise(
        msg = |Object render() has mandatory params for tag '{ iv_tag_name }'|
        rc  = 'ORMP' ).
    endif.

    read table ls_meth-parameters into ls_param
      with key parm_kind = cl_abap_objectdescr=>returning type_kind = cl_abap_typedescr=>typekind_string.
    if sy-subrc is not initial.
      zcx_mustache_error=>raise(
        msg = |Object render() does not return string for tag '{ iv_tag_name }'|
        rc  = 'ORRS' ).
    endif.

    data lt_ptab type abap_parmbind_tab.
    data ls_ptab like line of lt_ptab.

    ls_ptab-name = ls_param-name.
    ls_ptab-kind = cl_abap_objectdescr=>receiving.
    get reference of rv_val into ls_ptab-value.
    insert ls_ptab into table lt_ptab.

    call method io_obj->(ls_meth-name)
      parameter-table lt_ptab.

  endmethod.


  method render_section.

    data: lr            type ref to data,
          lv_type       type c,
          lv_unitab     type abap_bool,
          lt_data_stack like it_data_stack.

    describe field i_data type lv_type.

    if lv_type ca c_data_type-table
      and cl_abap_typedescr=>describe_by_data( i_data )->absolute_name = c_ty_struc_tt_absolute_name.
      lv_unitab = abap_true.
    elseif lv_type = cl_abap_typedescr=>typekind_oref.
      data lo_data type ref to zcl_mustache_data.
      data ls_data_struc type zif_mustache=>ty_struc_tt.

      try .
        lo_data ?= i_data.
      catch cx_sy_move_cast_error ##NO_HANDLER.
        zcx_mustache_error=>raise(
          msg = |Cannot render section { iv_path }, wrong data item type|
          rc  = 'CRWD' ).
      endtry.

      ls_data_struc = lo_data->get( ).
      get reference of ls_data_struc into lr.
      lv_unitab = abap_true.
    endif.

    if lv_type ca c_data_type-struc
       or lv_type ca c_data_type-elem
       or lv_unitab = abap_true.

      " Update context
      lt_data_stack = it_data_stack.
      if lr is initial. " Not assigned above
        get reference of i_data into lr.
      endif.
      append lr to lt_data_stack.

      " Input is a structure - one time render
      render_loop(
        exporting
          is_statics    = is_statics
          it_data_stack = lt_data_stack
          iv_start_idx  = iv_start_idx
          iv_path       = iv_path
        changing
          ct_lines      = ct_lines ).

    elseif lv_type ca c_data_type-table.

      " Input is a table - iterate
      field-symbols <table>   type any table.
      field-symbols <tabline> type any.
      assign i_data to <table>.
      loop at <table> assigning <tabline>.
        render_section(
          exporting
            is_statics    = is_statics
            it_data_stack = it_data_stack
            iv_start_idx  = iv_start_idx
            i_data        = <tabline>
            iv_path       = iv_path
        changing
          ct_lines        = ct_lines ).
      endloop.

    else.
      zcx_mustache_error=>raise(
        msg = |Cannot render section { iv_path }, wrong data item type|
        rc  = 'CRWD' ).
    endif.

  endmethod.  " render_section.
ENDCLASS.
