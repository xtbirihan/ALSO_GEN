class ZCL_PARAM_CHECK definition
  public
  final
  create public .

public section.

  interfaces ZIF_PARAM_CONST .

  types:
    BEGIN OF ts_param_tab,
        absolute_name TYPE abap_abstypename,
        value         TYPE char40,
      END OF ts_param_tab .
  types:
    tth_param_tab TYPE HASHED TABLE OF ts_param_tab WITH UNIQUE KEY absolute_name value .

  constants C_MARK_IND type CHAR1 value 'M' ##NO_TEXT.
  constants C_CONTFLAG_APPL type ZDE_CONTFLAG value 'A' ##NO_TEXT.

  class-methods CLASS_CONSTRUCTOR .
  class-methods CHECK_PARAM_TRAN_CREATE_NEW
    importing
      !IS_PARC type ZMV_PARAM_CUST
    raising
      ZCX_FRAMEWORK_CHECK .
  class-methods CHECK_PARAM_NO_TRAN_CREATE_NEW
    importing
      !IS_PARMD type ZMV_PARAM_MAST
    raising
      ZCX_FRAMEWORK_CHECK .
  class-methods CHECK_PARAM_DEFINE_CREATE_NEW
    importing
      !IS_PARDF type ZMV_PARAM_DEF
    raising
      ZCX_FRAMEWORK_CHECK .
  class-methods CHECK_SUB_PARAM_RECORD_EXISTS
    importing
      !IS_PARDF type ZTPARAM_DEF
    raising
      ZCX_FRAMEWORK_CHECK .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA: st_parameter_md      TYPE tth_param_tab,
                sv_id1_absolute_name TYPE abap_abstypename,
                sv_id2_absolute_name TYPE abap_abstypename.
ENDCLASS.



CLASS ZCL_PARAM_CHECK IMPLEMENTATION.


  METHOD check_param_define_create_new.
    BREAK-POINT ID zcg_param_framework.

    IF is_pardf-id1 IS INITIAL
      OR is_pardf-id2 IS INITIAL.
      MESSAGE e002 INTO DATA(lv_msg_dummy).
      RAISE EXCEPTION TYPE zcx_framework_check.
    ENDIF.

    "Process ID
    IF NOT line_exists( st_parameter_md[ absolute_name = sv_id1_absolute_name
                                         value = is_pardf-id1 ] ).
      MESSAGE e002 WITH is_pardf-id1 INTO lv_msg_dummy.
      RAISE EXCEPTION TYPE zcx_framework_check.
    ENDIF.

    "Parameter ID
    IF NOT line_exists( st_parameter_md[ absolute_name = sv_id2_absolute_name
                                         value = is_pardf-id2 ] ).
      MESSAGE e003 WITH is_pardf-id2  INTO lv_msg_dummy.
      RAISE EXCEPTION TYPE zcx_framework_check.
    ENDIF.

    "Check description
    IF is_pardf-remark IS INITIAL.
      MESSAGE w006 WITH is_pardf-id1 is_pardf-id2  INTO lv_msg_dummy.
      RAISE EXCEPTION TYPE zcx_framework_check.
    ENDIF.
  ENDMETHOD.


  METHOD check_param_no_tran_create_new.
    BREAK-POINT ID zcg_param_framework.

    IF is_parmd-id1 IS INITIAL
      OR is_parmd-id2 IS INITIAL
      OR is_parmd-lgnum IS INITIAL
      OR is_parmd-seqno IS INITIAL.
      MESSAGE e002 INTO DATA(lv_msg_dummy).
      RAISE EXCEPTION TYPE zcx_framework_check.
    ENDIF.

    SELECT SINGLE contflag FROM ztparam_def
      INTO @DATA(lv_contflag)
      WHERE lgnum = @is_parmd-lgnum
        AND id1 = @is_parmd-id1
        AND id2 = @is_parmd-id2.

    IF sy-subrc  <> 0.
      MESSAGE e001 WITH is_parmd-id1 is_parmd-id2  INTO lv_msg_dummy.
      RAISE EXCEPTION TYPE zcx_framework_check.
    ENDIF.

    IF lv_contflag <> zcl_param_check=>c_contflag_appl.
      MESSAGE e007 WITH is_parmd-id1 is_parmd-id2  INTO lv_msg_dummy.
      RAISE EXCEPTION TYPE zcx_framework_check.
    ENDIF.
  ENDMETHOD.


  METHOD check_param_tran_create_new.
    BREAK-POINT ID zcg_param_framework.

    IF is_parc-id1 IS INITIAL
      OR is_parc-id2 IS INITIAL
      OR is_parc-lgnum IS INITIAL
      OR is_parc-seqno IS INITIAL.
      MESSAGE e002 INTO DATA(lv_msg_dummy).
      RAISE EXCEPTION TYPE zcx_framework_check.
    ENDIF.

    SELECT @abap_true
        FROM ztparam_def
        INTO @DATA(lv_existance_check) ##NEEDED
        UP TO 1 ROWS
        WHERE id1 = @is_parc-id1
          AND id2 = @is_parc-id2.
    ENDSELECT.

    IF sy-subrc  <> 0.
      MESSAGE e001 WITH is_parc-id1 is_parc-id2 INTO lv_msg_dummy.
      RAISE EXCEPTION TYPE zcx_framework_check.
    ENDIF.
  ENDMETHOD.


  METHOD check_sub_param_record_exists.

    BREAK-POINT ID zcg_param_framework.

    IF is_pardf IS INITIAL.
      RETURN.
    ENDIF.

    " check if sub object exists in master data table
    SELECT  @abap_true
      FROM ztparam_mast
    INTO TABLE @DATA(lv_parameter_mast)
    WHERE lgnum = @is_pardf-lgnum
      AND id1   = @is_pardf-id1
      AND id2   = @is_pardf-id2.

    IF sy-subrc = 0.
      MESSAGE e005 WITH is_pardf-id1 is_pardf-id2 INTO DATA(lv_msg_dummy).
      RAISE EXCEPTION TYPE zcx_framework_check.
    ENDIF.

    " check if sub record exists in cust. data table
    SELECT @abap_true
      FROM ztparam_cust
    INTO TABLE @DATA(lv_parameter_cust)
     WHERE lgnum = @is_pardf-lgnum
       AND id1    = @is_pardf-id1
       AND id2    = @is_pardf-id2.

    IF sy-subrc = 0.
      MESSAGE e006 WITH is_pardf-id1 is_pardf-id2 INTO lv_msg_dummy.
      RAISE EXCEPTION TYPE zcx_framework_check.
    ENDIF.

  ENDMETHOD.


  METHOD class_constructor.
    DATA:
      lo_typedesc_const TYPE REF TO cl_abap_typedescr,
      lo_classdesc      TYPE REF TO cl_abap_classdescr,
      lv_const_name     TYPE string,
      ls_padf           TYPE ztparam_def.

    FIELD-SYMBOLS:
    <ls_val>       TYPE data.

    BREAK-POINT ID zcg_param_framework.

    CLEAR: st_parameter_md.

    "Get all attributes of the class (incl. interface)
    lo_classdesc = CAST cl_abap_classdescr( cl_abap_classdescr=>describe_by_object_ref(
                                            NEW zcl_param_check( ) ) ).

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
    lo_typedesc_const = cl_abap_datadescr=>describe_by_data( ls_padf-id1 ).
    sv_id1_absolute_name = lo_typedesc_const->absolute_name.

    lo_typedesc_const = cl_abap_datadescr=>describe_by_data( ls_padf-id2 ).
    sv_id2_absolute_name = lo_typedesc_const->absolute_name.
  ENDMETHOD.
ENDCLASS.
