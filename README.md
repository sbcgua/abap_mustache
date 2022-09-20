# ABAP Mustache

Mustache template engine for ABAP. It implements the [original mustache spec](https://mustache.github.io/mustache.5.html) plus some additions (mainly in plans now :) Mustache is a logic-less template syntax. It is good for generation of HTML or any other text stuff.  

## Versions

The library has a Steampunk (740+) compatible version, thanks to @DerGuteWolf. Currently it lives in the [steampunk](https://github.com/sbcgua/abap_mustache/tree/steampunk) branch. This version, at the time of publication, is somewhat slower compared to the original (master) due to limited whitelisted features of the steampunk (see the [PR comments](https://github.com/sbcgua/abap_mustache/pull/7#issuecomment-797714683)), but it has own advantages and **may become the default in near future**. **If you are using the library in older environments, please notify in the issues to take it into account**

## Example

Let's assume you have some data like this:

```abap
types: 
  begin of ty_shop_item
    name  type string,
    price type s_price, 
  end of ty_shop_item,
  
  ty_shop_item_tt type standard table of ty_shop_item,

  begin of ty_shop,
    shop_name type string,
    items     type ty_shop_item_tt, 
  end of ty_shop.
```
Create an instance of abap mustache class, feed the desired template to it and then render with your data:
```abap
data lo_mustache type ref to lcl_mustache.
data lv_text     type string.

" c_nl is a shortcut for newline char, e.g. from cl_abap_char_utilities
lo_mustache = lcl_mustache=>create(
    'Welcome to {{shop_name}}!' && c_nl && 
    '{{#items}}'                && c_nl &&
    '* {{name}} - ${{price}}'   && c_nl &&
    '{{/items}}' ).

lv_text = lo_mustache->render( ls_my_data ). " ls_my_data type ty_shop 
``` 
As the result (`lv_text`) you'll get some output like this (assuming you filled the `ls_my_data` appropriately):
```
Welcome to My Super Store
* Boots - $99.00
* T-Short - $49.00
* Shirts - $59.00
```

## Supported input and technical details

### Data structure

* Input data may be a structure or a table of any type - it is detected automatically. For a table the template is repeated for each line. The depth of the data is not limited for the moment (so can be table in struc in table in struc ...).
* In addition, there is a special table type `lcl_mustache=>ty_struc_tt` which processed as a universal structure. Field `name` corresponds to tag name (case **insesitive**) and the value may be put either to `val` field or as a reference to `dref`. The latter can be then a reference to a structure or a table. So the above example may would look like:

```abap
data lt_uni_data type lcl_mustache=>ty_struc_tt.
field-symbols <i> like line of lt_uni_data.

append initial line to lt_uni_data assigning <i>.
<i>-name = 'shop_name'.
<i>-val  = 'My Super Store'.

append initial line to lt_uni_data assigning <i>.
<i>-name = 'items'.
get reference of lt_items to <i>-dref. " lt_items is the ty_shop_item_tt filled elsewhere
...
lv_text = lo_mustache->render( lt_uni_data ).
```  

* The `lcl_mustache=>ty_struc_tt` is mainly intended to be the root structure. Supposedly it is convenient to prepare parts of data as regular structures and tables. However, combining them all together at the top level in yet another dedicated structure may be cumbersome. This is where `lcl_mustache=>ty_struc_tt` may serve well. Although you are free to choose your own way of course.

### Templates    

* Template may be a string or a table of strings (`type string_table`)

```abap
lo_mustache = lcl_mustache=>create( lv_template ). " iv_template = lv_template
* OR
lo_mustache = lcl_mustache=>create( it_template = lt_template_tab ). " TABLE of strings
```

* Output can be received as a string or a table of string (`type string_table`)
```abap
lv_text = lo_mustache->render( ls_my_data ).
* OR
lt_text_tab = lo_mustache->render_tt( ls_my_data ). " TABLE
```

* Template as table is supposed to be separated by newlines. So in case of string rendering newline char will be inserted between template lines. In case of `render_tt` newline char is not inserted (still if any template line contain line breaks it will be split into several output table lines).
* Input string template is split by newline chars for internal processing. Both `LF` and `CRLF` separators are supported (detected automatically). 

### Partials

The class supports partials (see example below). So you may prepare your templates in convenient and logical sections. And reuse them if necessary.

```abap
lo_mustache = lcl_mustache=>create(
    'Welcome to {{shop_name}}!' && c_nl && 
    '{{>items_template}}' ).  " << Calling partial !

lo_partial  = lcl_mustache=>create(
    '{{#items}}'                && c_nl &&
    '* {{name}} - ${{price}}'   && c_nl &&
    '{{/items}}' ).

lo_mustache->add_partial(     " Register partial
  iv_name = 'items_template' 
  io_obj  = lo_partial ).

lv_text = lo_mustache->render( ls_my_data ). " ls_my_data type ty_shop 
``` 

### More

Further information can be found in [wiki](https://github.com/sbcgua/abap_mustache/wiki). There are also some notes on performance.

## Installation

* The best option to install is to clone the repository to your SAP system using [abapGit](https://github.com/larshp/abapGit) tool.
* Alternatively, create `zmustache` and `zmustache_ut` includes and fill with the content of `zmustache.prog.abap` and `zmustache_ut.prog.abap` respectively. `zmustache_ut` is the unit tests include, so you might potentially skip it (just remove the references to `ltcl_mustache` from the first include). 

## Plans

Here are some stuff I might be implementing with time:

* Some stuff inspired by [Handlebars](http://handlebarsjs.com/), in particular:
  * inline partials (within one template)
  * `if/else`
  * `each/else`
  * `with` and maybe path features (not sure) 
  * section parameters
  * maybe some sort of helpers
* calling object methods for parts of data
* references to object attributes (maybe, if will be needed)

Still some real usage statistics would be valuable to decide on this or that feature. 

## Contribution

Contribution is always welcomed :)

## License

The code is licensed under MIT License. Please see [LICENSE](/LICENSE) for details.
