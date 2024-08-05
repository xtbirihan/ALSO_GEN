CLASS zcl_crud_marm DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES tt_marm TYPE STANDARD TABLE OF marm WITH EMPTY KEY.

    CLASS-METHODS select_single_by_key
      IMPORTING
        iv_matnr         TYPE marm-matnr
        iv_uom           TYPE marm-meinh
      RETURNING
        VALUE(rs_result) TYPE marm.

    CLASS-METHODS select_multi_range
      IMPORTING
        it_matnr         TYPE rseloption
        it_uom           TYPE rseloption
      RETURNING
        VALUE(rt_result) TYPE tt_marm.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_CRUD_MARM IMPLEMENTATION.


  METHOD select_multi_range.
********************************************************************
*& Key          : <BSUGAREV>-Nov 14, 2023
*& Request No.  :
********************************************************************
*& Description
*&
*&
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    SELECT * FROM marm
      INTO TABLE @rt_result
     WHERE matnr IN @it_matnr
       AND meinh IN @it_uom.
    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS 'MARM' sy-datum sy-uzeit.
    ENDIF.
  ENDMETHOD.


  METHOD select_single_by_key.
********************************************************************
*& Key          : <BSUGAREV>-Nov 14, 2023
*& Request No.  :
********************************************************************
*& Description
*&
*&
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    SELECT SINGLE * FROM marm
      INTO @rs_result
     WHERE matnr = @iv_matnr
       AND meinh = @iv_uom.
    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS 'MARM' sy-datum sy-uzeit.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
