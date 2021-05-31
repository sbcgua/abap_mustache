class zcl_mustache_test definition
  public
  final
  create public
  for testing .


  public section.

    constants c_nl type c value cl_abap_char_utilities=>newline.

    types:
      begin of ty_dummy,
        name type string,
        am   type abap_bool,
        pm   type abap_bool,
        html type string,
        tab  type string_table,
        obj  type ref to zcl_mustache_utils,
        begin of attr,
          age    type i,
          male   type abap_bool,
          female type abap_bool,
        end of attr,
      end of ty_dummy,

      begin of ty_size,
        size type char4,
        qty  type i,
      end of ty_size,
      ty_size_tt type standard table of ty_size with default key,

      begin of ty_item,
        name  type string,
        price type string,
        sizes type ty_size_tt,
      end of ty_item,
      ty_item_tt type standard table of ty_item with default key,

      begin of ty_tag_rc,
        val   type string,
        rc    type char4,
      end of ty_tag_rc,
      ty_tag_rc_tt type standard table of ty_tag_rc with default key,

      begin of ty_test_case,
        template     type string,
        tokens       type zif_mustache=>ty_token_tt,
        output       type string,
        complex_test type abap_bool,
      end of ty_test_case,
      ty_test_case_tt type standard table of ty_test_case with default key.


    class-methods get_test_case
      importing iv_index    type i optional
      exporting ev_count        type i
                ev_complex_test type c
                ev_template     type string
                et_tokens       type zif_mustache=>ty_token_tt
                ev_output       type string.
    class-methods get_test_data
      exporting es_simple   type ty_dummy
                et_complex1 type zif_mustache=>ty_struc_tt
                et_complex2 type zif_mustache=>ty_struc_tt.

    class-methods class_constructor.


  protected section.
  private section.
    class-data gt_test_case_stash type ty_test_case_tt.

ENDCLASS.



CLASS ZCL_MUSTACHE_TEST IMPLEMENTATION.


  method class_constructor.

    field-symbols: <t>     like line of gt_test_case_stash,
                   <token> like line of <t>-tokens.

    " Case 1
    append initial line to gt_test_case_stash assigning <t>.
    <t>-template = 'Hello {{name}}!'.
    <t>-tokens = VALUE #(
      ( type = zif_mustache=>c_token_type-static level = 1 content = `Hello ` )
      ( type = zif_mustache=>c_token_type-etag level = 1 content = `name` )
      ( type = zif_mustache=>c_token_type-static level = 1 content = `!` )
    ).
    <t>-output = 'Hello Anonymous network user!'.

    " Case 2
    append initial line to gt_test_case_stash assigning <t>.
    <t>-template = 'Hello {{name}}'.
    <t>-tokens = VALUE #(
      ( type = zif_mustache=>c_token_type-static level = 1 content = `Hello ` )
      ( type = zif_mustache=>c_token_type-etag level = 1 content = `name` )
    ).
    <t>-output = 'Hello Anonymous network user'.

    " Case 3
    append initial line to gt_test_case_stash assigning <t>.
    <t>-template = '{{name}} Hello'.
    <t>-tokens = VALUE #(
      ( type = zif_mustache=>c_token_type-etag level = 1 content = `name` )
      ( type = zif_mustache=>c_token_type-static level = 1 content = ` Hello` )
    ).
    <t>-output = 'Anonymous network user Hello'.

    " Case 4
    append initial line to gt_test_case_stash assigning <t>.
    <t>-template = 'Good {{#pm}}afternoon{{/pm}}{{^pm}}morning{{/pm}}, {{name}}'.
    <t>-tokens = VALUE #(
      ( type = zif_mustache=>c_token_type-static level = 1 content = `Good ` )
      ( type = zif_mustache=>c_token_type-section cond = zif_mustache=>c_section_condition-if level = 1 content = `pm` )
      ( type = zif_mustache=>c_token_type-static level = 2 content = `afternoon` )
      ( type = zif_mustache=>c_token_type-section cond = zif_mustache=>c_section_condition-ifnot level = 1 content = `pm` )
      ( type = zif_mustache=>c_token_type-static level = 2 content = `morning` )
      ( type = zif_mustache=>c_token_type-static level = 1 content = `, ` )
      ( type = zif_mustache=>c_token_type-etag level = 1 content = `name` )
    ).
    <t>-output = 'Good afternoon, Anonymous network user'.

    " Case 5
    append initial line to gt_test_case_stash assigning <t>.
    <t>-template = 'Good {{^am}}afternoon{{/am}}{{#am}}morning{{/am}}, {{name}}'.
    <t>-tokens = VALUE #(
      ( type = zif_mustache=>c_token_type-static level = 1 content = `Good ` )
      ( type = zif_mustache=>c_token_type-section cond = zif_mustache=>c_section_condition-ifnot level = 1 content = `am` )
      ( type = zif_mustache=>c_token_type-static level = 2 content = `afternoon` )
      ( type = zif_mustache=>c_token_type-section cond = zif_mustache=>c_section_condition-if level = 1 content = `am` )
      ( type = zif_mustache=>c_token_type-static level = 2 content = `morning` )
      ( type = zif_mustache=>c_token_type-static level = 1 content = `, ` )
      ( type = zif_mustache=>c_token_type-etag level = 1 content = `name` )
    ).
    <t>-output = 'Good afternoon, Anonymous network user'.

    " Case 6
    append initial line to gt_test_case_stash assigning <t>.
    <t>-template = '{{!comment}}{{html}} {{{html}}} {{&html}}'.
    <t>-tokens = VALUE #(
      ( type = zif_mustache=>c_token_type-etag level = 1 content = `html` )
      ( type = zif_mustache=>c_token_type-static level = 1 content = ` ` )
      ( type = zif_mustache=>c_token_type-utag level = 1 content = `html` )
      ( type = zif_mustache=>c_token_type-static level = 1 content = ` ` )
      ( type = zif_mustache=>c_token_type-utag level = 1 content = `html` )
    ).
    <t>-output = '&lt;tag&gt;&amp; <tag>& <tag>&'.

    " Case 7
    append initial line to gt_test_case_stash assigning <t>.
    <t>-template = '{{pm}}{{=<* *>=}}<*pm*>{{xx}}<*={{ }}=*>{{pm}}'.
    <t>-tokens = VALUE #(
      ( type = zif_mustache=>c_token_type-etag level = 1 content = `pm` )
      ( type = zif_mustache=>c_token_type-etag level = 1 content = `pm` )
      ( type = zif_mustache=>c_token_type-static level = 1 content = `{{xx}}` )
      ( type = zif_mustache=>c_token_type-etag level = 1 content = `pm` )
    ).
    <t>-output = 'XX{{xx}}X'.

    " Case 8
    append initial line to gt_test_case_stash assigning <t>.
    <t>-complex_test = '1'.
    <t>-template = 'Welcome to {{shop}}'                    && "c_nl &&
                   'Our sales:'                             && "c_nl &&
                   '{{#items}}'                             && "c_nl &&
                   '* {{name}} - ${{price}}'                && "c_nl &&
                   '  sizes: {{#sizes}}{{size}},{{/sizes}}' && "c_nl &&
                   '{{/items}}'.
    <t>-tokens = VALUE #(
      ( type = zif_mustache=>c_token_type-static level = 1 content = `Welcome to ` )
      ( type = zif_mustache=>c_token_type-etag level = 1 content = `shop` )
      ( type = zif_mustache=>c_token_type-static level = 1 content = `Our sales:` )
      ( type = zif_mustache=>c_token_type-section cond = zif_mustache=>c_section_condition-if level = 1 content = `items` )
      ( type = zif_mustache=>c_token_type-static level = 2 content = `* ` )
      ( type = zif_mustache=>c_token_type-etag level = 2 content = `name` )
      ( type = zif_mustache=>c_token_type-static level = 2 content = ` - $` )
      ( type = zif_mustache=>c_token_type-etag level = 2 content = `price` )
      ( type = zif_mustache=>c_token_type-static level = 2 content = `  sizes: ` )
      ( type = zif_mustache=>c_token_type-section cond = zif_mustache=>c_section_condition-if level = 2 content = `sizes` )
      ( type = zif_mustache=>c_token_type-etag level = 3 content = `size` )
      ( type = zif_mustache=>c_token_type-static level = 3 content = `,` )
    ).
    <t>-output   = 'Welcome to Shopsky'                     && "c_nl &&
                   'Our sales:'                             && "c_nl &&
                   '* Boots - $99.00'                       && "c_nl &&
                   '  sizes: 40,41,42,'                     && "c_nl &&
                   '* T-short - $49.00'                     && "c_nl &&
                   '  sizes: S,M,L,'.

    " Case 9 - newlines and lonely section
    append initial line to gt_test_case_stash assigning <t>.
    <t>-complex_test = '1'.
    <t>-template = 'Our sales:'                             && c_nl &&
                   `  {{#items}}  `                         && c_nl &&
                   '* {{name}} - ${{price}}'                && c_nl &&
                   `  {{/items}}  `.
    <t>-tokens = VALUE #(
      ( type = zif_mustache=>c_token_type-static level = 1 content = `Our sales:` )
      ( type = zif_mustache=>c_token_type-static level = 1 content = c_nl )
      ( type = zif_mustache=>c_token_type-section cond = zif_mustache=>c_section_condition-if level = 1 content = `items` )
      ( type = zif_mustache=>c_token_type-static level = 2 content = `* ` )
      ( type = zif_mustache=>c_token_type-etag level = 2 content = `name` )
      ( type = zif_mustache=>c_token_type-static level = 2 content = ` - $` )
      ( type = zif_mustache=>c_token_type-etag level = 2 content = `price` )
      ( type = zif_mustache=>c_token_type-static level = 2 content = c_nl )
    ).
    <t>-output   = 'Our sales:'                             && c_nl &&
                   '* Boots - $99.00'                       && c_nl &&
                   '* T-short - $49.00'                     && c_nl.

    " Case 10
    append initial line to gt_test_case_stash assigning <t>.
    <t>-template = '{{#tab}}{{@tabline}},{{/tab}}'.
    <t>-tokens = VALUE #(
      ( type = zif_mustache=>c_token_type-section cond = zif_mustache=>c_section_condition-if level = 1 content = `tab` )
      ( type = zif_mustache=>c_token_type-etag level = 2 content = `@tabline` )
      ( type = zif_mustache=>c_token_type-static level = 2 content = `,` )
    ).
    <t>-output = 'line1,line2,'.

    " Case 11 - empty table, -first/-last
    append initial line to gt_test_case_stash assigning <t>.
    <t>-complex_test = '2'.
    <t>-template =
       'Our sales:'                             && c_nl &&
       '{{#items}}'                             && c_nl &&
       '* {{name}} - ${{price}}'                && c_nl &&
       '  sizes: {{#sizes}}{{^@first}}, {{/@first}}{{size}}{{#@last}}.{{/@last}}{{/sizes}}{{^sizes}}all sold out{{/sizes}}' && c_nl &&
       '{{/items}}'.
    <t>-tokens = VALUE #(
      ( type = zif_mustache=>c_token_type-static level = 1 content = `Our sales:` )
      ( type = zif_mustache=>c_token_type-static level = 1 content = c_nl )
      ( type = zif_mustache=>c_token_type-section cond = zif_mustache=>c_section_condition-if level = 1 content = `items` )
      ( type = zif_mustache=>c_token_type-static level = 2 content = `* ` )
      ( type = zif_mustache=>c_token_type-etag level = 2 content = `name` )
      ( type = zif_mustache=>c_token_type-static level = 2 content = ` - $` )
      ( type = zif_mustache=>c_token_type-etag level = 2 content = `price` )
      ( type = zif_mustache=>c_token_type-static level = 2 content = c_nl )

      ( type = zif_mustache=>c_token_type-static level = 2 content = `  sizes: ` )
      ( type = zif_mustache=>c_token_type-section cond = zif_mustache=>c_section_condition-if level = 2 content = `sizes` )
      ( type = zif_mustache=>c_token_type-section cond = zif_mustache=>c_section_condition-ifnot level = 3 content = `@first` )
      ( type = zif_mustache=>c_token_type-static level = 4 content = `, ` )
      ( type = zif_mustache=>c_token_type-etag level = 3 content = `size` )
      ( type = zif_mustache=>c_token_type-section cond = zif_mustache=>c_section_condition-if level = 3 content = `@last` )
      ( type = zif_mustache=>c_token_type-static level = 4 content = `.` )


      ( type = zif_mustache=>c_token_type-section cond = zif_mustache=>c_section_condition-ifnot level = 2 content = `sizes` )
      ( type = zif_mustache=>c_token_type-static level = 3 content = `all sold out` )

      ( type = zif_mustache=>c_token_type-static level = 2 content = c_nl )

    ).
    <t>-output   = 'Our sales:'                            && c_nl &&
                   '* 3-Hole - $100'                       && c_nl &&
                   '  sizes: 37, 40, 42.'                  && c_nl &&
                   '* 6-Hole - $200'                       && c_nl &&
                   '  sizes: all sold out'                 && c_nl.
  endmethod.  " class_setup.


  method get_test_case.

    field-symbols: <t>     like line of gt_test_case_stash.

    if ev_count is requested.
      ev_count = lines( gt_test_case_stash ).
    endif.

    if iv_index is initial.
      return. " Nothing else requested
    endif.

    read table gt_test_case_stash index iv_index assigning <t>.

    ev_complex_test = <t>-complex_test.
    ev_template     = <t>-template.
    et_tokens       = <t>-tokens.
    ev_output       = <t>-output.

  endmethod. "get_test_case


  method get_test_data.

    field-symbols: <data> like line of et_complex1,
                   <tab>  type ty_item_tt,
                   <item> like line of <tab>,
                   <size> type ty_size.

    " Simple data
    es_simple-name = 'Anonymous network user'.
    es_simple-am   = abap_false.
    es_simple-pm   = abap_true.
    es_simple-html = '<tag>&'.
    create object es_simple-obj.
    append 'line1' to es_simple-tab.
    append 'line2' to es_simple-tab.

    " Complex data
    clear et_complex1.

    append initial line to et_complex1 assigning <data>.
    <data>-name = 'shop'.
    <data>-val  = 'Shopsky'.
    append initial line to et_complex1 assigning <data>.
    <data>-name = 'items'.
    create data <data>-dref type ty_item_tt.

    assign <data>-dref->* to <tab>.

    " Boots
    append initial line to <tab> assigning <item>.
    <item>-name  = 'Boots'.
    <item>-price = '99.00'.
    append initial line to <item>-sizes assigning <size>.
    <size>-size = '40'.
    <size>-qty  = 8.
    append initial line to <item>-sizes assigning <size>.
    <size>-size = '41'.
    <size>-qty  = 12.
    append initial line to <item>-sizes assigning <size>.
    <size>-size = '42'.
    <size>-qty  = 3.

    "T-short
    append initial line to <tab> assigning <item>.
    <item>-name  = 'T-short'.
    <item>-price = '49.00'.
    append initial line to <item>-sizes assigning <size>.
    <size>-size = 'S'.
    <size>-qty  = 15.
    append initial line to <item>-sizes assigning <size>.
    <size>-size = 'M'.
    <size>-qty  = 23.
    append initial line to <item>-sizes assigning <size>.
    <size>-size = 'L'.
    <size>-qty  = 18.

    " Complexer data
    et_complex2 = VALUE #( ( name = 'shop' val  = 'Shopsky' ) ( name = 'items' dref = NEW ty_item_tt(
      ( name = '3-Hole' price = '100' sizes = VALUE #( ( size = '37' qty = 2 ) ( size = '40' qty = 3 ) ( size = '42' qty = 4 ) ) )
      ( name = '6-Hole' price = '200' )
    ) ) ).
  endmethod. "get_test_data
ENDCLASS.
