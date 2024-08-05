CLASS zcl_crud_scwm_whohu DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS select_multi_by_who
      IMPORTING
        !iv_lgnum       TYPE /scwm/lgnum
        iv_who          TYPE /scwm/de_who
      RETURNING
        VALUE(rt_whohu) TYPE /scwm/tt_whohu.

    CLASS-METHODS select_single_by_huid
      IMPORTING
        !iv_lgnum       TYPE /scwm/lgnum
        iv_who          TYPE /scwm/de_who
        iv_huid         TYPE /scwm/de_whohuid
      RETURNING
        VALUE(rs_whohu) TYPE /scwm/whohu.

    CLASS-METHODS select_multi_by_huid
      IMPORTING
        !iv_lgnum       TYPE /scwm/lgnum
        it_huid         TYPE /scwm/tt_whohuid
      RETURNING
        VALUE(rt_whohu) TYPE /scwm/tt_whohu.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CRUD_SCWM_WHOHU IMPLEMENTATION.


  METHOD select_multi_by_huid.
********************************************************************
*& Key          : <BSUGAREV>-31.08.2023 16:07:37
*& Request No.  :
********************************************************************
*& Description
*&
********************************************************************
    BREAK-POINT ID zcg_db_crud.


    SELECT * FROM /scwm/whohu
      INTO TABLE @rt_whohu
       FOR ALL ENTRIES IN @it_huid
     WHERE lgnum = @iv_lgnum
       AND huid  = @it_huid-huid.
    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS '/SCWM/WHOHU' sy-datum sy-uzeit.
    ENDIF.
  ENDMETHOD.


  METHOD select_multi_by_who.
********************************************************************
*& Key          : <BSUGAREV>-14.07.2023 10:07:37
*& Request No.  :
********************************************************************
*& Description
*&
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    SELECT * FROM /scwm/whohu
      INTO TABLE @rt_whohu
     WHERE lgnum = @iv_lgnum
       AND who   = @iv_who.
    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS '/SCWM/WHOHU' sy-datum sy-uzeit.
    ENDIF.
  ENDMETHOD.


  METHOD select_single_by_huid.
********************************************************************
*& Key          : <BSUGAREV>-14.07.2023 10:07:37
*& Request No.  :
********************************************************************
*& Description
*&
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    SELECT SINGLE * FROM /scwm/whohu
      INTO @rs_whohu
     WHERE lgnum = @iv_lgnum
       AND who   = @iv_who
       AND huid  = @iv_huid.
    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS '/SCWM/WHOHU' sy-datum sy-uzeit.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
