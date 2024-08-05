CLASS zcl_crud_scwm_wo_rsrc_ty DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS select_multi_by_who
      IMPORTING
        !iv_lgnum            TYPE /scwm/lgnum
        it_who_selopt        TYPE rseloption
      RETURNING
        VALUE(rt_wo_rsrc_ty) TYPE /scwm/tt_wo_rsrc_ty.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CRUD_SCWM_WO_RSRC_TY IMPLEMENTATION.


  METHOD select_multi_by_who.
********************************************************************
*& Key          : BSUGAREV-Jan 12, 2024
*& Request No.  : GAP-076 Outbound Priority settings
********************************************************************
*& Description  :
*&
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    SELECT * FROM /scwm/wo_rsrc_ty
      INTO TABLE @rt_wo_rsrc_ty
     WHERE lgnum = @iv_lgnum
       AND who  IN @it_who_selopt.
    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS it_who_selopt sy-uzeit.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
