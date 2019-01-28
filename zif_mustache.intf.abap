interface zif_mustache
  public .

  constants version type string value '2.0.0'. " Package version
  constants homepage type string value 'https://github.com/sbcgua/abap_mustache'.

  types:
    ty_x_format  like cl_abap_format=>e_html_text,
    ty_ref_tt    type standard table of ref to data with default key.

  types:  " Mustache token
    begin of ty_token,
      type    type char2,
      cond    type char2,
      level   type i,
      content type string,
    end of ty_token,
    ty_token_tt type standard table of ty_token with key type cond level.

  types:  " Universal input data structure
    begin of ty_struc,
      name type string,
      val  type string,
      dref type ref to data,
      oref type ref to object,
    end of ty_struc,
    ty_struc_tt type standard table of ty_struc with key name.

  types:  " Partials
    begin of ty_partial,
      name type string,
      obj  type ref to zif_mustache,
    end of ty_partial,
    ty_partial_tt type standard table of ty_partial with key name.

  constants:
    begin of c_token_type,
      static      type ty_token-type value '_',   " static content
      comment     type ty_token-type value '*',   " comment
      etag        type ty_token-type value 'et',  " escaped tag
      utag        type ty_token-type value 'ut',  " unescaped tag
      section     type ty_token-type value 'ss',  " section
      section_end type ty_token-type value 'se',  " end of section
      partial     type ty_token-type value 'pa',  " partial / include
      delimiter   type ty_token-type value 'dc',  " change of delimiters
    end of c_token_type.

  constants:
    begin of c_section_condition,
      if          type ty_token-cond value '=',   " if
      ifnot       type ty_token-cond value '!',   " unless
    end of c_section_condition.

    methods render
      importing
        i_data         type any
      returning
        value(rv_text) type string
      raising
        zcx_mustache_error.

    methods render_tt
      importing
        i_data         type any
      returning
        value(rt_tab)  type string_table
      raising
        zcx_mustache_error.

    methods add_partial
      importing
        iv_name type string
        io_obj  type ref to zif_mustache
      raising
        zcx_mustache_error.

    methods get_partials
      returning
        value(rt_partials) type ty_partial_tt.

    methods get_tokens
      returning
        value(rt_tokens) type ty_token_tt.

endinterface.
