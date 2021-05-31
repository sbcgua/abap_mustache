class ZCX_MUSTACHE_ERROR definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  data RC type CHAR4 read-only .
  data MSG type STRING read-only .

  methods CONSTRUCTOR
    importing
      !TEXTID like TEXTID optional
      !PREVIOUS like PREVIOUS optional
      !RC type CHAR4 optional
      !MSG type STRING optional .
  class-methods RAISE
    importing
      !MSG type CLIKE
      !RC type CHAR4
    raising
      ZCX_MUSTACHE_ERROR .
protected section.
private section.
ENDCLASS.



CLASS ZCX_MUSTACHE_ERROR IMPLEMENTATION.


method CONSTRUCTOR ##ADT_SUPPRESS_GENERATION.
  CALL METHOD SUPER->CONSTRUCTOR
    EXPORTING
      TEXTID = TEXTID
      PREVIOUS = PREVIOUS
  .
  me->RC = RC .
  me->MSG = MSG .
endmethod.


method raise.
  raise exception type zcx_mustache_error
    exporting
      msg = msg
      rc  = rc.
endmethod.
ENDCLASS.
