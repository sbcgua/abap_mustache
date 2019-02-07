class ZCL_MUSTACHE_DATA definition
  public
  final
  create public .

public section.

  class-methods CREATE_FOR
    importing
      !IV_NAME type STRING
      !IV_VAL type ANY
    returning
      value(RO_DATA) type ref to ZCL_MUSTACHE_DATA .
  class-methods GET_FOR
    importing
      !IV_NAME type STRING
      !IV_VAL type ANY
    returning
      value(RT_DATA) type ZIF_MUSTACHE=>TY_STRUC_TT .
  methods ADD
    importing
      !IV_NAME type STRING
      !IV_VAL type ANY .
  methods GET
    returning
      value(RT_DATA) type ZIF_MUSTACHE=>TY_STRUC_TT .
  private section.
    data mt_data type zif_mustache=>ty_struc_tt.
ENDCLASS.



CLASS ZCL_MUSTACHE_DATA IMPLEMENTATION.


  method add.

    data lo_type type ref to cl_abap_datadescr.
    data ls_data like line of mt_data.

    ls_data-name = iv_name.

    lo_type ?= cl_abap_typedescr=>describe_by_data( iv_val ).

    case lo_type->kind.
      when cl_abap_typedescr=>kind_class or cl_abap_typedescr=>kind_intf.
        " ls_data-oref = iv_val. ???
      when cl_abap_typedescr=>kind_elem.
        ls_data-val = |{ iv_val }|.
      when cl_abap_typedescr=>kind_ref.
        case lo_type->type_kind.
          when cl_abap_typedescr=>typekind_dref.
            ls_data-dref = iv_val.
          when cl_abap_typedescr=>typekind_oref.
            ls_data-oref = iv_val.
        endcase.
      when cl_abap_typedescr=>kind_struct.
        get reference of iv_val into ls_data-dref.
      when cl_abap_typedescr=>kind_table.
        field-symbols <tab> type any table.
        create data ls_data-dref type handle lo_type.
        assign ls_data-dref->* to <tab>.
        <tab> = iv_val.
    endcase.

    append ls_data to mt_data.

  endmethod.


  method create_for.
    create object ro_data.
    ro_data->add( iv_name = iv_name iv_val = iv_val ).
  endmethod.


  method get.
    rt_data = mt_data.
  endmethod.


  method get_for.
    data lo_temp type ref to zcl_mustache_data.
    create object lo_temp.
    lo_temp->add( iv_name = iv_name iv_val = iv_val ).
    rt_data = lo_temp->get( ).
  endmethod.
ENDCLASS.
