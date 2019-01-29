class zcl_mustache_parser definition
  public
  final
  create public .

  public section.

    class-methods parse_template
      importing
        !iv_template type string optional
        !it_template type string_table optional
      preferred parameter iv_template
      returning
        value(rt_tokens) type zif_mustache=>ty_token_tt
      raising
        zcx_mustache_error .
    class-methods tokenize
      importing
        !iv_template type string
      exporting
        !et_tokens type zif_mustache=>ty_token_tt
        !ev_lonely_section type abap_bool
      raising
        zcx_mustache_error .
    class-methods parse_tag
      importing
        !iv_chunk type string
      returning
        value(rv_token) type zif_mustache=>ty_token
      raising
        zcx_mustache_error .
    class-methods build_token_structure
      changing
        !ct_tokens type zif_mustache=>ty_token_tt
      raising
        zcx_mustache_error .
  protected section.
  private section.
ENDCLASS.



CLASS ZCL_MUSTACHE_PARSER IMPLEMENTATION.


  method build_token_structure.

    data:
          lv_idx    type i,
          lv_level  type i value 1,
          lt_stack  type string_table.

    field-symbols: <token>        like line of ct_tokens,
                   <section_name> like line of lt_stack.

    " Validate, build levels, remove comments
    loop at ct_tokens assigning <token>.
      <token>-level = lv_level.
      lv_idx        = sy-tabix.

      case <token>-type.
        when zif_mustache=>c_token_type-comment.
          delete ct_tokens index lv_idx. " Ignore comments

        when zif_mustache=>c_token_type-delimiter.
          delete ct_tokens index lv_idx. " They served their purpose in tokenize

        when zif_mustache=>c_token_type-section.
          lv_level = lv_level + 1.
          append <token>-content to lt_stack.

        when zif_mustache=>c_token_type-section_end.
          lv_level = lv_level - 1.
          if lv_level < 1.
            zcx_mustache_error=>raise(
              msg = |Closing of non-opened section: { <token>-content }|
              rc  = 'CNOS' ).
          endif.

          read table lt_stack index lines( lt_stack ) assigning <section_name>.
          if <section_name> <> <token>-content.
            zcx_mustache_error=>raise(
              msg = |Closing section mismatch: { <section_name> } ({ lv_level })|
              rc  = 'CSM' ).
          endif.

          delete lt_stack index lines( lt_stack ). " Remove, not needed anymore
          delete ct_tokens index lv_idx. " No further need for section close tag

        when others.
          assert 1 = 1. " Do nothing
      endcase.
    endloop.

    if lv_level > 1.
      read table lt_stack index lines( lt_stack ) assigning <section_name>.
      zcx_mustache_error=>raise(
        msg = |Section not closed: { <section_name> } ({ lv_level })|
        rc  = 'SNC' ).
    endif.

  endmethod.  " build_token_structure.


  method parse_tag.

    data: lv_sigil type string,
          lv_tail  type c,
          lv_cnt   type i,
          lv_param type string.

    lv_param = iv_chunk.

    if strlen( lv_param ) = 0.
      zcx_mustache_error=>raise(
        msg = 'Empty tag'
        rc  = 'ET' ).
    endif.

    if lv_param(1) ca '#^/=!>&{'. " Get tag type
      lv_sigil = iv_chunk(1).
      shift lv_param by 1 places.
    endif.

    if strlen( lv_param ) = 0.
      zcx_mustache_error=>raise(
        msg = 'Empty tag'
        rc  = 'ET' ).
    endif.

    if lv_sigil ca '={'. " Check closing part of tag type
      lv_tail = substring( val = lv_param  off = strlen( lv_param ) - 1  len = 1 ).
      if lv_sigil = '=' and lv_tail <> '='.
        zcx_mustache_error=>raise(
          msg = 'Missing closing ='
          rc  = 'MC=' ).
      endif.
      if lv_sigil = '{' and lv_tail <> '}'.
        zcx_mustache_error=>raise(
          msg = 'Missing closing }'
          rc  = 'MC}' ).
      endif.
      lv_param = substring( val = lv_param  len = strlen( lv_param ) - 1 ).
    endif.

    " Trip spaces
    shift lv_param right deleting trailing space.
    shift lv_param left deleting leading space.

    if strlen( lv_param ) = 0.
      zcx_mustache_error=>raise(
        msg = 'Empty tag'
        rc  = 'ET' ).
    endif.

    case lv_sigil.
      when ''.
        rv_token-type = zif_mustache=>c_token_type-etag.
      when '&' or '{'.
        rv_token-type = zif_mustache=>c_token_type-utag.
      when '#'.
        rv_token-type = zif_mustache=>c_token_type-section.
        rv_token-cond = zif_mustache=>c_section_condition-if.
      when '^'.
        rv_token-type = zif_mustache=>c_token_type-section.
        rv_token-cond = zif_mustache=>c_section_condition-ifnot.
      when '/'.
        rv_token-type = zif_mustache=>c_token_type-section_end.
      when '='.
        rv_token-type = zif_mustache=>c_token_type-delimiter.
        condense lv_param. " Remove unnecessary internal spaces too
        find all occurrences of ` ` in lv_param match count lv_cnt.
        if lv_cnt <> 1. " Must contain one separating space
          zcx_mustache_error=>raise(
            msg = |Change of delimiters failed: '{ lv_param }'|
            rc  = 'CDF' ).
        endif.
      when '!'. " Comment
        rv_token-type = zif_mustache=>c_token_type-comment.
      when '>'.
        rv_token-type = zif_mustache=>c_token_type-partial.
      when others.
        assert 0 = 1. " Cannot reach, programming error
    endcase.

    rv_token-content = lv_param.

  endmethod.  " parse_tag.


  method parse_template.

    data: lt_strings        type string_table,
          lv_lonely_section type abap_bool,
          lv_first_line     type abap_bool value abap_true,
          ls_newline_token  like line of rt_tokens,
          lt_token_portion  like rt_tokens.

    field-symbols <line> like line of lt_strings.

    assert not ( iv_template is not initial and lines( it_template ) > 0 ).

    if lines( it_template ) > 0.
      loop at it_template assigning <line>.
        append lines of zcl_mustache_utils=>split_string( <line> ) to lt_strings.
      endloop.
    else. " iv_template, which can also be empty then
      lt_strings = zcl_mustache_utils=>split_string( iv_template ).
    endif.

    ls_newline_token-type    = zif_mustache=>c_token_type-static.
    ls_newline_token-content = cl_abap_char_utilities=>newline.

    loop at lt_strings assigning <line>.
      if lv_first_line = abap_false and lv_lonely_section = abap_false.
        " Don't insert before first line and after lonely sections
        append ls_newline_token to rt_tokens.
      endif.

      tokenize(
        exporting iv_template       = <line>
        importing et_tokens         = lt_token_portion
                  ev_lonely_section = lv_lonely_section ).
      append lines of lt_token_portion to rt_tokens.

      clear lv_first_line.
    endloop.

    build_token_structure( changing ct_tokens = rt_tokens ).

  endmethod.  " parse_template.


  method tokenize.

    data:
          lv_otag  type string value '{{',
          lv_ctag  type string value '}}',
          lv_intag type abap_bool,
          lv_cur   type i,
          lv_len   type i,
          lv_off   type i.

    field-symbols <token> like line of et_tokens.

    clear et_tokens.
    ev_lonely_section = abap_true.
    lv_len            = strlen( iv_template ).

    do.

      if lv_intag = abap_true.  " Inside of a tag
        find first occurrence of lv_ctag
          in section offset lv_cur of iv_template
          match offset lv_off.

        if sy-subrc > 0. " Closing tag not found !
          zcx_mustache_error=>raise(
            msg = 'Closing }} not found'
            rc  = 'CTNF' ).
        endif.

        if lv_ctag = '}}' and lv_len - lv_off > 2 and iv_template+lv_off(3) = '}}}'.
          lv_off = lv_off + 1. " Crutch for {{{x}}} (to find internal closing '}')
        endif.

        append initial line to et_tokens assigning <token>.
        <token> = parse_tag( substring( val = iv_template
                                        off = lv_cur
                                        len = lv_off - lv_cur ) ).

        if not ( <token>-type = zif_mustache=>c_token_type-section or <token>-type = zif_mustache=>c_token_type-section_end ).
          " Any tag other than section makes it not lonely
          clear ev_lonely_section.
        endif.

        if <token>-type = zif_mustache=>c_token_type-delimiter. " Change open/close tags
          split <token>-content at ` ` into lv_otag lv_ctag.
        endif.

        lv_intag = abap_false.
        lv_cur   = lv_off + strlen( lv_ctag ).

      else.                     " In static text part
        find first occurrence of lv_otag
          in section offset lv_cur of iv_template
          match offset lv_off.

        if sy-subrc > 0. " No more tags found
          lv_off = strlen( iv_template ).
        else. " Open tag found
          lv_intag = abap_true.
        endif.

        if lv_off > lv_cur. " Append unempty static token
          append initial line to et_tokens assigning <token>.
          <token>-type    = zif_mustache=>c_token_type-static.
          <token>-content = substring( val = iv_template
                                       off = lv_cur
                                       len = lv_off - lv_cur ).

          if ev_lonely_section = abap_true and <token>-content cn ` `.
            " Non space static section resets makes section not lonely
            clear ev_lonely_section.
          endif.
        endif.

        if lv_intag = abap_true. " Tag was found
          lv_cur   = lv_off + strlen( lv_otag ).
        else.
          exit. " End of template - exit loop
        endif.
      endif.

    enddo.

    if ev_lonely_section = abap_true.
      " Delete all static content at lonely section line
      delete et_tokens where type = zif_mustache=>c_token_type-static.
    endif.

  endmethod.  " tokenize.
ENDCLASS.
