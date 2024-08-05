class ZCL_SWITCH_CHECKS definition
  public
  final
  create public .

public section.

  interfaces zif_switch_const .

  types:
    BEGIN OF ts_param_tab,
        absolute_name TYPE abap_abstypename,
        value         TYPE char40,
      END OF ts_param_tab .
  types:
    tth_param_tab TYPE HASHED TABLE OF ts_param_tab WITH UNIQUE KEY absolute_name value .

  class-methods CLASS_CONSTRUCTOR .
  class-methods CHECK_ACTIONS_CREATE_NEW
    importing
      !IS_ACT type ZTSWITCH_ACT
    raising
      ZCX_FRAMEWORK_CHECK .
  class-methods CHECK_CRITERIA_CREATE_NEW
    importing
      !IS_CRIT type ZTSWITCH_CRIT
    raising
      ZCX_FRAMEWORK_CHECK .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA: st_parameter_md        TYPE tth_param_tab,
                sv_devid_absolute_name TYPE abap_abstypename,
                sv_field_absolute_name TYPE abap_abstypename.
ENDCLASS.



CLASS ZCL_SWITCH_CHECKS IMPLEMENTATION.


  METHOD check_actions_create_new.

    BREAK-POINT ID zcg_switch_framework.

    IF is_act-lgnum IS INITIAL
      OR is_act-devid IS INITIAL.
      MESSAGE e002 INTO DATA(lv_msg_dummy).
      RAISE EXCEPTION TYPE zcx_framework_check.
    ENDIF.

    "Process ID
    IF NOT line_exists( st_parameter_md[ absolute_name = sv_devid_absolute_name
                                         value         = is_act-devid ] ).
      MESSAGE e003 WITH is_act-devid INTO lv_msg_dummy.
      RAISE EXCEPTION TYPE zcx_framework_check.
    ENDIF.

    "Check description
    IF is_act-descr IS INITIAL.
      MESSAGE w004 WITH is_act-devid INTO lv_msg_dummy.
      RAISE EXCEPTION TYPE zcx_framework_check.
    ENDIF.
  ENDMETHOD.


  METHOD check_criteria_create_new.

    BREAK-POINT ID zcg_switch_framework.

    IF is_crit-lgnum IS INITIAL
      OR is_crit-devid IS INITIAL
      OR is_crit-combination IS INITIAL
      OR is_crit-field IS INITIAL.
      MESSAGE e002 INTO DATA(lv_msg_dummy).
      RAISE EXCEPTION TYPE zcx_framework_check.
    ENDIF.

    "Field name
    IF NOT line_exists( st_parameter_md[ absolute_name = sv_field_absolute_name
                                         value         = is_crit-field ] ).
      MESSAGE e005 WITH is_crit-field INTO lv_msg_dummy.
      RAISE EXCEPTION TYPE zcx_framework_check.
    ENDIF.
  ENDMETHOD.


  METHOD class_constructor.
    DATA:
      lo_typedesc_const TYPE REF TO cl_abap_typedescr,
      lo_classdesc      TYPE REF TO cl_abap_classdescr,
      lv_const_name     TYPE string,
      ls_act            TYPE ztswitch_act,
      ls_crit           TYPE ztswitch_crit.

    FIELD-SYMBOLS:
    <ls_val>       TYPE data.

    BREAK-POINT ID zcg_switch_framework.

    CLEAR: st_parameter_md.

    "Get all attributes of the class (incl. interface)
    lo_classdesc = CAST cl_abap_classdescr( cl_abap_classdescr=>describe_by_object_ref(
                                            NEW zcl_switch_checks( ) ) ).

    LOOP AT lo_classdesc->attributes ASSIGNING FIELD-SYMBOL(<ls_attribute>).
      IF NOT (      <ls_attribute>-is_interface = abap_true
                AND <ls_attribute>-is_constant = abap_true ).
        CONTINUE.
      ENDIF.
      lv_const_name = <ls_attribute>-name.
      REPLACE '~' IN lv_const_name WITH '=>'.

      ASSIGN (lv_const_name) TO <ls_val>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      lo_typedesc_const = cl_abap_datadescr=>describe_by_data( <ls_val> ).
      COLLECT VALUE ts_param_tab( absolute_name = lo_typedesc_const->absolute_name
                                  value = CONV #( <ls_val> ) )
                                  INTO st_parameter_md.
    ENDLOOP.

    "Determine types of ID fields
    lo_typedesc_const = cl_abap_datadescr=>describe_by_data( ls_act-devid ).
    sv_devid_absolute_name = lo_typedesc_const->absolute_name.

    lo_typedesc_const = cl_abap_datadescr=>describe_by_data( ls_crit-field ).
    sv_field_absolute_name = lo_typedesc_const->absolute_name.
  ENDMETHOD.
ENDCLASS.
