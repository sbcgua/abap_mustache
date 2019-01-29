*&---------------------------------------------------------------------*
*& Report  ZABAP_MUSTACHE_RUNNER
*&---------------------------------------------------------------------*
*& Sample program on how to use abap_mustache
*&---------------------------------------------------------------------*

report zabap_mustache_runner.
**********************************************************************

include zmustache.
include zmustache_perf_test.

**********************************************************************
start-of-selection.

types:
  begin of ty_footer,
    user like sy-uname,
    wish type string,
  end of ty_footer.

data lo_mustache type ref to lcl_mustache.
data ls_data2    type ty_footer.
data lv_greeting type string.
data lv_footer   type string.

**********************************************************************
* Example with ty_struc_tt table - a universal structure table
**********************************************************************

data lt_data1    type zif_mustache=>ty_struc_tt.    " Universal structure
field-symbols <l> like line of lt_data1.

append initial line to lt_data1 assigning <l>.
<l>-name = 'user'.
<l>-val  = sy-uname.                      " Direct value

" Value like reference is possible
" Intended to be used for structures and tables
*get reference of sy-uname into <l>-ref.

append initial line to lt_data1 assigning <l>.
<l>-name = 'pm'.
<l>-val  = boolc( sy-uzeit > '130000' ).  " Direct value

" 7.40 style would look much nicer of course
*lt_data1 = value #(
*  ( name = 'user' val = sy-uname )
*  ( name = 'pm'   val = boolc( sy-uzeit > '130000' ) ) ).

" Parse and render template
lo_mustache = lcl_mustache=>create(
  'Good {{#pm}}afternoon{{/pm}}{{^pm}}morning{{/pm}}, {{user}} !' ).
lv_greeting = lo_mustache->render( lt_data1 ).

**********************************************************************
* Example with regular structure
**********************************************************************

" Regular structures are also possible and probably are the primary way
" ty_struc_tt is mostly intended to be the root node, collecting several data structures
" in one place (as references - ty_struc_tt-ref).

ls_data2-user = sy-uname.
if sy-uzeit+5(1) mod 2 = 1.
  ls_data2-wish = 'fun'.
else.
  ls_data2-wish = 'a nice day'.
endif.

lo_mustache = lcl_mustache=>create(
  'Have {{wish}}, {{user}} !' ).
lv_footer = lo_mustache->render( ls_data2 ).

**********************************************************************
* Output
**********************************************************************

write: / lv_greeting.
write: / .
write: / 'This is just a demo program with the most simple example.'.
write: / 'In order to use abap mustache include zmustache and zmustache_ut into your project.'.
write: / 'To learn more about abap_mustache and how to use it,'.
write: / 'please visit', 'https://github.com/sbcgua/abap_mustache' color = 6 inverse on.
write: / .
write: / lv_footer.

**********************************************************************
* Performance test
**********************************************************************

uline.
write / .
lcl_mustache_perf_test=>do_perf_test( ).
