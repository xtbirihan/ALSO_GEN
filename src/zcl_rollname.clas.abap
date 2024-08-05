class ZCL_ROLLNAME definition
  public
  final
  create public .

public section.

  interfaces /SAPCND/IF_EX_ROLLNAME .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ROLLNAME IMPLEMENTATION.


  METHOD /SAPCND/IF_EX_ROLLNAME~ATTRIBUTE_CONVERSION.
**********************************************************************
*& Key           : RM-230222
*& Request No.   : GAP-10 – “GR Dummy HU creation”
**********************************************************************
*& Description (short)
*& Populate field ZPHU_LGTYP for HU Label printing
**********************************************************************

    e_was_executed = abap_true.
    e_result       = 0.

  ENDMETHOD.


  METHOD /sapcnd/if_ex_rollname~authority_check ##NEEDED.
**********************************************************************
*& Key           : RM-230222
*& Request No.   : GAP-10 – “GR Dummy HU creation”
**********************************************************************
*& Description (short)
*& Populate field ZPHU_LGTYP for HU Label printing
**********************************************************************

    " Empty Implementation to avoid dumps

  ENDMETHOD.


  METHOD /SAPCND/IF_EX_ROLLNAME~DEFAULT_VALUE_SUGGESTION.
**********************************************************************
*& Key           : RM-230222
*& Request No.   : GAP-10 – “GR Dummy HU creation”
**********************************************************************
*& Description (short)
*& Populate field ZPHU_LGTYP for HU Label printing
**********************************************************************

    e_was_executed = abap_true.
    e_result       = 0.

  ENDMETHOD.


  METHOD /sapcnd/if_ex_rollname~dequeue ##NEEDED.
**********************************************************************
*& Key           : RM-230222
*& Request No.   : GAP-10 – “GR Dummy HU creation”
**********************************************************************
*& Description (short)
*& Populate field ZPHU_LGTYP for HU Label printing
**********************************************************************

    " Empty Implementation to avoid dumps

  ENDMETHOD.


  METHOD /sapcnd/if_ex_rollname~disable_badi_buffer ##NEEDED.
**********************************************************************
*& Key           : RM-230222
*& Request No.   : GAP-10 – “GR Dummy HU creation”
**********************************************************************
*& Description (short)
*& Populate field ZPHU_LGTYP for HU Label printing
**********************************************************************

    " Empty Implementation to avoid dumps

  ENDMETHOD.


  METHOD /sapcnd/if_ex_rollname~enqueue ##NEEDED.
**********************************************************************
*& Key           : RM-230222
*& Request No.   : GAP-10 – “GR Dummy HU creation”
**********************************************************************
*& Description (short)
*& Populate field ZPHU_LGTYP for HU Label printing
**********************************************************************

    " Empty Implementation to avoid dumps

  ENDMETHOD.


  METHOD /sapcnd/if_ex_rollname~f4_help ##NEEDED.
**********************************************************************
*& Key           : RM-230222
*& Request No.   : GAP-10 – “GR Dummy HU creation”
**********************************************************************
*& Description (short)
*& Populate field ZPHU_LGTYP for HU Label printing
**********************************************************************

    " Empty Implementation to avoid dumps

  ENDMETHOD.


  METHOD /SAPCND/IF_EX_ROLLNAME~FIELD_CHECK.
**********************************************************************
*& Key           : RM-230222
*& Request No.   : GAP-10 – “GR Dummy HU creation”
**********************************************************************
*& Description (short)
*& Populate field ZPHU_LGTYP for HU Label printing
**********************************************************************

    e_was_executed = abap_true.
    e_result       = 0.

  ENDMETHOD.


  METHOD /sapcnd/if_ex_rollname~ranges_conversion ##NEEDED.
**********************************************************************
*& Key           : RM-230222
*& Request No.   : GAP-10 – “GR Dummy HU creation”
**********************************************************************
*& Description (short)
*& Populate field ZPHU_LGTYP for HU Label printing
**********************************************************************

    " Empty Implementation to avoid dumps

  ENDMETHOD.


  METHOD /sapcnd/if_ex_rollname~short_text_retrieval ##NEEDED.
**********************************************************************
*& Key           : RM-230222
*& Request No.   : GAP-10 – “GR Dummy HU creation”
**********************************************************************
*& Description (short)
*& Populate field ZPHU_LGTYP for HU Label printing
**********************************************************************

    " Empty Implementation to avoid dumps

  ENDMETHOD.
ENDCLASS.
