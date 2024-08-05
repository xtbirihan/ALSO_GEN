CLASS zcl_switch DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_switch_const .

    TYPES:
      "Field names which should be provided by the caller,
      "for the wanted 'combination'
      BEGIN OF ts_req_f ,
        field TYPE zde_field,
      END OF ts_req_f .
    TYPES:
      BEGIN OF tth_req_f ,
        field TYPE zde_field,
      END OF tth_req_f .

    DATA gv_lgnum TYPE /scwm/lgnum READ-ONLY .
    DATA gv_devid TYPE zde_devid READ-ONLY VALUE 'PSE001' ##NO_TEXT.


    CLASS-METHODS get_switch_state
      IMPORTING
        !iv_lgnum       TYPE /scwm/lgnum
        !iv_devid       TYPE zde_devid
        !it_fields      TYPE ztt_switch_fields OPTIONAL
      RETURNING
        VALUE(rv_state) TYPE boole_d .
  PROTECTED SECTION.

private section.

  types TS_INSTANCE type ref to ZCL_SWITCH .
  types:
    tt_instance TYPE STANDARD TABLE OF ts_instance .

  data GV_NOT_EXISTING_CUSTOMIZING type ABAP_BOOL value ABAP_FALSE ##NO_TEXT.
  data GS_SFW_ACT type ZTSWITCH_ACT .
  data GT_CRITERIA type ZTT_SWITCH_CRIT .
  class-data ST_INSTANCE type TT_INSTANCE .

  class-methods GET_INSTANCE
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_DEVID type ZDE_DEVID
    returning
      value(RO_INSTANCE) type ref to ZCL_SWITCH .
  methods CONSTRUCTOR
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_DEVID type ZDE_DEVID .
  methods GET_STATE
    importing
      !IT_FIELDS type ZTT_SWITCH_FIELDS optional
    returning
      value(RV_STATE) type BOOLE_D .
ENDCLASS.



CLASS ZCL_SWITCH IMPLEMENTATION.


  METHOD constructor.

    BREAK-POINT ID zcg_switch_framework.

    gv_devid = iv_devid.
    gv_lgnum = iv_lgnum.

    "Take make table with act/inact status for current instance
    "(import parameters)
    SELECT SINGLE *
      FROM ztswitch_act
      INTO @gs_sfw_act
      WHERE lgnum = @iv_lgnum
        AND devid = @iv_devid.

    IF sy-subrc = 0.
      "Take the second table with the additional criteria if provided (not obligotary)
      SELECT *
        FROM ztswitch_crit
        INTO TABLE @gt_criteria
        WHERE lgnum = @iv_lgnum
          AND devid = @iv_devid
          AND xactive = @abap_true.                       "#EC CI_SUBRC
    ELSE.
      gv_not_existing_customizing = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD get_instance.
    FIELD-SYMBOLS:
                   <ls_instance> LIKE LINE OF st_instance.

    "take already loaded object for this switch parameters if exist
    LOOP AT st_instance ASSIGNING <ls_instance>.
      IF iv_lgnum = <ls_instance>->gv_lgnum AND iv_devid = <ls_instance>->gv_devid.
        ro_instance = <ls_instance>.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF ro_instance IS BOUND.
      RETURN.
    ENDIF.

    "Create instance and add it to the global object table
    ro_instance = NEW #( iv_lgnum = iv_lgnum iv_devid = iv_devid ).

    IF ro_instance->gv_not_existing_customizing = abap_false.
      APPEND ro_instance TO st_instance.
    ELSE.
      CLEAR ro_instance.
    ENDIF.
  ENDMETHOD.


  METHOD get_state.
    DATA: lt_fields     TYPE ztt_switch_fields,
          lt_req_fields TYPE HASHED TABLE OF tth_req_f WITH UNIQUE KEY field,
          ls_req_fields TYPE ts_req_f.

    IF gs_sfw_act-xactive = abap_false.
      rv_state = abap_false.
      RETURN.
    ENDIF.

    "Fields with the additional configuration
    "if provided by the caller
    lt_fields = it_fields.

    "Set the act/inact Status from the main table
    rv_state = gs_sfw_act-xactive.

    "When there is no additional configuration,
    "we will just return the above assigned status and exit here
    IF gt_criteria IS INITIAL.
      IF it_fields IS NOT INITIAL.
        rv_state = abap_false.
      ENDIF.
      RETURN.
    ENDIF.

    "Additional implementation to make possible that the code is execute
    "only for single user/s
    ASSIGN gt_criteria[ KEY active COMPONENTS
                        field = zif_switch_const~c_user_name
                        xactive = abap_true ]
                        TO FIELD-SYMBOL(<ls_criteria>).

    "When active 'UNAME' is found, this mean that the switch should be executed
    "only for the mentioned user otherwise return not active state
    IF sy-subrc = 0 AND
       <ls_criteria>-field_value = sy-uname.
      rv_state = abap_true.
      RETURN.
    ENDIF.

    " Loop the provided fields from the caller
*    LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_fields>)
*        GROUP BY <ls_fields>-combination.
    LOOP AT gt_criteria ASSIGNING FIELD-SYMBOL(<ls_crit>) GROUP BY <ls_crit>-combination.

      "For every COMBINATION we took every field name once
      "because we want later to check if the caller provide
      "values for all fields in the combination group
      CLEAR lt_req_fields.

*      LOOP AT gt_criteria ASSIGNING FIELD-SYMBOL(<ls_crit>)
*        USING KEY group_field
*        WHERE combination = <ls_fields>-combination
*          AND field NE zif_switch_const~c_user_name.
*
*        ls_req_fields-field = <ls_crit>-field.
*        COLLECT ls_req_fields INTO lt_req_fields.
*      ENDLOOP.
      LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_fields>).
        ls_req_fields-field = <ls_fields>-field.
        COLLECT ls_req_fields INTO lt_req_fields.
      ENDLOOP.

      "Loop all fields from the current combination
      LOOP AT lt_fields INTO DATA(ls_fld).

        "Check if the value of the current field have match in the criteria table
        IF line_exists( gt_criteria[ KEY value COMPONENTS
                                        field = ls_fld-field
                                        field_value = ls_fld-field_value ] ).
          rv_state = abap_true.
          "when all fields for one combination have match
          "the lt_req_fields at the end should be empty
          DELETE lt_req_fields WHERE field = ls_fld-field.
        ELSE.
          "We expect that in one combination group all fields should match,
          "So if single one did not match set inactive status
          rv_state = abap_false.
          EXIT.
        ENDIF.

      ENDLOOP.

      "If all needed fields are provided by the caller, than this should be empty
      DESCRIBE TABLE lt_req_fields LINES DATA(lv_req_lines).

      IF lv_req_lines = 0 AND rv_state = abap_true.
        "When one combination group match, this is enough to exit with active status
        rv_state = abap_true.
        EXIT.
      ELSE.
        "Set inactive state for current combination and check the next one if exist
        rv_state = abap_false.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_switch_state.

    BREAK-POINT ID zcg_switch_framework.

    " Create object from already existing reference or create new one
    DATA(lo_switch) = zcl_switch=>get_instance(
      iv_lgnum = iv_lgnum
      iv_devid = iv_devid ).

    IF lo_switch IS BOUND.
      rv_state = lo_switch->get_state( it_fields = it_fields ).
    ELSE.
      "If there is No existing configuration for provided IV_LGNUM and iv_devid
      " return inactive status
      rv_state = abap_false.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
