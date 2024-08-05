class ZCL_PRNT_CCAT_HU definition
  public
  final
  create public .

public section.

  interfaces /SCWM/IF_EX_PRNT_CCAT_HU .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.

  constants C_ZPHU_LGTYP type /SAPCND/FIELDNAME value 'ZPHU_LGTYP' ##NO_TEXT.
  constants C_ZPHU_LGPLA type /SAPCND/FIELDNAME value 'ZPHU_LGPLA' ##NO_TEXT.
  constants C_ZPHU_LGBER type /SAPCND/FIELDNAME value 'ZPHU_LGBER' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_PRNT_CCAT_HU IMPLEMENTATION.


  METHOD /scwm/if_ex_prnt_ccat_hu~change.
**********************************************************************
*& Key           : RM-230222
*& Request No.   : GAP-10 – “GR Dummy HU creation”
**********************************************************************
*& Description (short)
*& Populate field ZPHU_LGTYP for HU Label printing
**********************************************************************
*"----------------------------------------------------------------------
*"  BREAK-POINT IDs: ZCG_PRINTHU
*"----------------------------------------------------------------------

    DATA:
      lo_ao_hu     TYPE REF TO /scwm/cl_hu_ppf,
      lt_huhdr     TYPE /scwm/tt_huhdr_int,
      ls_attribute TYPE /sapcnd/det_attrib_value.

    BREAK-POINT ID zcg_printhu.

* Get persistent data
    TRY.
        lo_ao_hu ?= io_context_hu->appl. "ref to object -> /scwm/cl_hu_ppf
        DATA(lv_huident) = lo_ao_hu->get_huident( ).
      CATCH cx_sy_move_cast_error.
    ENDTRY.

    IF NOT lo_ao_hu IS BOUND.
      RETURN.
    ENDIF.

    "read the HU and the content
    CALL FUNCTION '/SCWM/HU_READ'
      EXPORTING
        iv_appl    = wmegc_huappl_wme " Constant value WME
        iv_lgnum   = iv_lgnum
        iv_huident = lv_huident
      IMPORTING
        et_huhdr   = lt_huhdr
      EXCEPTIONS
        deleted    = 1
        not_found  = 2
        error      = 3
        OTHERS     = 4.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    ASSIGN lt_huhdr[ huident = lv_huident ] TO FIELD-SYMBOL(<ls_huhdr>).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT ct_request ASSIGNING FIELD-SYMBOL(<ls_request>).
      READ TABLE <ls_request>-item_attributes ASSIGNING FIELD-SYMBOL(<ls_attributes>)
                                              WITH KEY fieldname = c_zphu_lgtyp.
      IF sy-subrc = 0.
        DELETE TABLE <ls_request>-item_attributes FROM <ls_attributes>.
        ls_attribute-fieldname = c_zphu_lgtyp.
        ls_attribute-value     = <ls_huhdr>-lgtyp.
        INSERT ls_attribute INTO TABLE <ls_request>-item_attributes.
      ENDIF.

      READ TABLE <ls_request>-item_attributes ASSIGNING <ls_attributes>
                                        WITH KEY fieldname = c_zphu_lgpla.
      IF sy-subrc = 0.
        DELETE TABLE <ls_request>-item_attributes FROM <ls_attributes>.
        ls_attribute-fieldname = c_zphu_lgpla.
        ls_attribute-value     = <ls_huhdr>-lgpla.
        INSERT ls_attribute INTO TABLE <ls_request>-item_attributes.
      ENDIF.

      READ TABLE <ls_request>-item_attributes ASSIGNING <ls_attributes>
                                        WITH KEY fieldname = c_zphu_lgber.
      IF sy-subrc = 0.
        DELETE TABLE <ls_request>-item_attributes FROM <ls_attributes>.
        ls_attribute-fieldname = c_zphu_lgber.
        ls_attribute-value     = <ls_huhdr>-lgber.
        INSERT ls_attribute INTO TABLE <ls_request>-item_attributes.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
