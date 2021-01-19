class ltcl_mustache_data definition final
  for testing risk level harmless duration short.

  private section.

    methods test for testing.
    methods test_tab_copy for testing.
    methods gen_tab_data
      returning value(ro_data) type ref to zcl_mustache_data.

endclass.

class ltcl_mustache_data implementation.

  method gen_tab_data.

    data lt_strtab type string_table.
    append 'Hello' to lt_strtab.
    append 'World' to lt_strtab.

    create object ro_data.

    " Table is local, so a copy must be created inside
    ro_data->add( iv_name = 'T' iv_val = lt_strtab ).

  endmethod.

  method test.

    data lo_data type ref to zcl_mustache_data.
    data lt_strtab type string_table.
    data lt_exp type zif_mustache=>ty_struc_tt.

    field-symbols <e> like line of lt_exp.

    append 'Hello' to lt_strtab.
    append 'World' to lt_strtab.

    create object lo_data.

    lo_data->add( iv_name = 'A' iv_val = 'B' ).
    lo_data->add( iv_name = 'O' iv_val = lo_data ).

    append initial line to lt_exp assigning <e>.
    <e>-name = 'A'.
    <e>-val  = 'B'.

    append initial line to lt_exp assigning <e>.
    <e>-name = 'O'.
    <e>-oref = lo_data.

    cl_abap_unit_assert=>assert_equals( exp = lt_exp act = lo_data->get( ) ).

  endmethod.

  method test_tab_copy.

    data lo_data type ref to zcl_mustache_data.
    lo_data = gen_tab_data( ).

    data lt_exptab type string_table.
    append 'Hello' to lt_exptab.
    append 'World' to lt_exptab.

    data lt_act type zif_mustache=>ty_struc_tt.
    lt_act = lo_data->get( ).

    field-symbols <e> like line of lt_act.
    field-symbols <tab> type any table.
    cl_abap_unit_assert=>assert_equals( exp = 1 act = lines( lt_act ) ).
    read table lt_act assigning <e> index 1.
    cl_abap_unit_assert=>assert_equals( exp = 'T' act = <e>-name ).
    assign <e>-dref->* to <tab>.
    cl_abap_unit_assert=>assert_equals( exp = lt_exptab act = <tab> ).

  endmethod.

endclass.
