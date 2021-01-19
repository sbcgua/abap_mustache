class ltcl_error_test definition final for testing
  duration short
  risk level harmless.
  private section.
    methods raise for testing.
endclass.

class ltcl_error_test implementation.
  method raise.

    data lx type ref to zcx_mustache_error.

    try .
      zcx_mustache_error=>raise( msg = 'Hello' rc = 'XX' ).
    catch zcx_mustache_error into lx.
      cl_abap_unit_assert=>assert_equals( exp = 'Hello' act = lx->msg ).
      cl_abap_unit_assert=>assert_equals( exp = 'XX' act = lx->rc ).
    endtry.
    cl_abap_unit_assert=>assert_not_initial( lx ).

  endmethod.
endclass.
