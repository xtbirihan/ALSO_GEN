CLASS zcl_crud_scwm_t303s DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS select_multi
      IMPORTING
        !iv_lgnum       TYPE /scwm/lgnum
        !it_lgtyp_r     TYPE /scwm/tt_lgtyp_r OPTIONAL
        !it_lptyp_r     TYPE /scwm/tt_lptyp_r OPTIONAL
      RETURNING
        VALUE(rt_t303s) TYPE /scwm/tt_t303s.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CRUD_SCWM_T303S IMPLEMENTATION.


  METHOD select_multi.
********************************************************************
*& Key          : <BSUGAREV>-28.04.2023 20:07:37
*& Request No.  :
********************************************************************
*& Description
*&
********************************************************************
    STATICS:
      sv_lgnum TYPE /scwm/lgnum,
      st_t303s TYPE /scwm/tt_t303s.

    BREAK-POINT ID zcg_db_crud.

    IF iv_lgnum <> sv_lgnum.
      sv_lgnum = iv_lgnum.

      CLEAR: st_t303s.

      SELECT * FROM /scwm/t303s
        INTO TABLE st_t303s
       WHERE lgnum = iv_lgnum
       ORDER BY PRIMARY KEY.
    ENDIF.

    LOOP AT st_t303s ASSIGNING FIELD-SYMBOL(<ls_t303s>) WHERE lgnum = iv_lgnum
                                                          AND lgtyp IN it_lgtyp_r
                                                          AND lptyp IN it_lptyp_r.
      APPEND <ls_t303s> TO rt_t303s.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
