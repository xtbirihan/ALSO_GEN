class ZCL_PARAM definition
  public
  final
  create public .

public section.

  interfaces ZIF_PARAM_CONST .

  class-methods GET_PARAMETER
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_PROCESS type ZDE_PARAM_ID1
      !IV_PARAMETER type ZDE_PARAM_ID2
    exporting
      !EV_CONSTANT type ZDE_PARAM_LOW
      !ET_LIST type ZTT_PARAM_LIST
      !ET_RANGE type RSELOPTION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_PARAM IMPLEMENTATION.


  METHOD get_parameter.

    BREAK-POINT ID zcg_param_framework.

    CLEAR: ev_constant, et_list, et_range.

    "Generic buffering is in play for both tables below
    SELECT *
        FROM ztparam_mast
        INTO TABLE @DATA(lt_param_mast)
        WHERE lgnum = @iv_lgnum
          AND id1 = @iv_process
          AND id2 = @iv_parameter.

    IF sy-subrc <> 0.
      SELECT *
          FROM ztparam_cust
          INTO TABLE @DATA(lt_param_cust)
          WHERE lgnum = @iv_lgnum
            AND id1 = @iv_process
            AND id2 = @iv_parameter.

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      lt_param_mast = CORRESPONDING #( BASE ( lt_param_mast ) lt_param_cust ).
    ENDIF.

    "Fill in requested return values
    LOOP AT lt_param_mast ASSIGNING FIELD-SYMBOL(<ls_parameter>).
      IF sy-tabix = 1 AND ev_constant IS REQUESTED.
        ev_constant = <ls_parameter>-low.
      ENDIF.

      IF et_list IS REQUESTED.
        APPEND <ls_parameter>-low TO et_list.
      ENDIF.

      IF et_range IS REQUESTED.
        APPEND VALUE #( sign = <ls_parameter>-sign
                        option = <ls_parameter>-opt
                        low = <ls_parameter>-low
                        high = <ls_parameter>-high ) TO et_range.
      ENDIF.

    ENDLOOP.

    "Delete empty ranges
    IF et_range IS REQUESTED.
      DELETE et_range
        WHERE sign IS INITIAL
           OR option IS INITIAL.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
