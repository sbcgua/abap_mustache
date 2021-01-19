class ZCL_MUSTACHE_UTILS definition
  public
  final
  create public .

public section.

  class-methods SPLIT_STRING
    importing
      !IV_TEXT type STRING
      !IV_SEP type CLIKE optional
    returning
      value(RT_TAB) type STRING_TABLE .
  class-methods JOIN_STRINGS
    importing
      !IT_TAB type STRING_TABLE
      !IV_SEP type CLIKE optional
    returning
      value(RV_TEXT) type STRING .
  class-methods CHECK_VERSION_FITS
    importing
      !I_REQUIRED_VERSION type STRING
      !I_CURRENT_VERSION type STRING
    returning
      value(R_FITS) type ABAP_BOOL .
  protected section.
  private section.
ENDCLASS.



CLASS ZCL_MUSTACHE_UTILS IMPLEMENTATION.


  method check_version_fits.

    types:
      begin of ty_version,
        major type numc4,
        minor type numc4,
        patch type numc4,
      end of ty_version.

    data ls_cur_ver type ty_version.
    data ls_req_ver type ty_version.
    data lv_buf type string.

    lv_buf = i_current_version.
    shift lv_buf left deleting leading 'v'.
    split lv_buf at '.' into ls_cur_ver-major ls_cur_ver-minor ls_cur_ver-patch.

    lv_buf = i_required_version.
    shift lv_buf left deleting leading 'v'.
    split lv_buf at '.' into ls_req_ver-major ls_req_ver-minor ls_req_ver-patch.

    if ls_req_ver-major <= ls_cur_ver-major.
      if ls_req_ver-minor <= ls_cur_ver-minor.
        if ls_req_ver-patch <= ls_cur_ver-patch.
          r_fits = abap_true.
        endif.
      endif.
    endif.

  endmethod.


  method join_strings.

    data lv_sep type string.

    if iv_sep is not supplied.
      lv_sep = cl_abap_char_utilities=>newline.
    else.
      lv_sep = iv_sep.
    endif.

    concatenate lines of it_tab into rv_text separated by lv_sep.

  endmethod. "join_strings


  method split_string.

    if iv_sep is not initial.
      split iv_text at iv_sep into table rt_tab.
    else.
      find first occurrence of cl_abap_char_utilities=>cr_lf in iv_text.
      if sy-subrc = 0.
        split iv_text at cl_abap_char_utilities=>cr_lf into table rt_tab.
      else.
        split iv_text at cl_abap_char_utilities=>newline into table rt_tab.
      endif.
    endif.

  endmethod.  " split_string.
ENDCLASS.
