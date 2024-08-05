class ZCX_DB_ERROR definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_DYN_MSG .
  interfaces IF_T100_MESSAGE .

  aliases DEFAULT_TEXTID
    for IF_T100_MESSAGE~DEFAULT_TEXTID .
  aliases MSGTY
    for IF_T100_DYN_MSG~MSGTY .
  aliases MSGV1
    for IF_T100_DYN_MSG~MSGV1 .
  aliases MSGV2
    for IF_T100_DYN_MSG~MSGV2 .
  aliases MSGV3
    for IF_T100_DYN_MSG~MSGV3 .
  aliases MSGV4
    for IF_T100_DYN_MSG~MSGV4 .
  aliases T100KEY
    for IF_T100_MESSAGE~T100KEY .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MSGTY type SYMSGTY optional
      !MSGV1 type SYMSGV optional
      !MSGV2 type SYMSGV optional
      !MSGV3 type SYMSGV optional
      !MSGV4 type SYMSGV optional .
protected section.
private section.
ENDCLASS.



CLASS ZCX_DB_ERROR IMPLEMENTATION.


  method CONSTRUCTOR.
CALL METHOD SUPER->CONSTRUCTOR
EXPORTING
PREVIOUS = PREVIOUS
.
me->MSGTY = MSGTY .
me->MSGV1 = MSGV1 .
me->MSGV2 = MSGV2 .
me->MSGV3 = MSGV3 .
me->MSGV4 = MSGV4 .
clear me->textid.
if textid is initial.
  IF_T100_MESSAGE~T100KEY = IF_T100_MESSAGE=>DEFAULT_TEXTID.
else.
  IF_T100_MESSAGE~T100KEY = TEXTID.
endif.
  endmethod.
ENDCLASS.
