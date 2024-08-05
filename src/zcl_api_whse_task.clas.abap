CLASS zcl_api_whse_task DEFINITION
  PUBLIC
  INHERITING FROM /scwm/cl_api_whse_task
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-DATA:
      mo_inst TYPE REF TO zcl_api_whse_task.
    CLASS-METHODS:
      get_instance
        EXPORTING
          eo_inst TYPE REF TO zcl_api_whse_task.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_API_WHSE_TASK IMPLEMENTATION.


  METHOD get_instance.
    IF mo_inst IS NOT BOUND.
      mo_inst = NEW zcl_api_whse_task( ).
    ENDIF.
    eo_inst = mo_inst.
  ENDMETHOD.
ENDCLASS.
