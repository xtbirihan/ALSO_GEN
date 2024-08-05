CLASS zcl_api_handling_unit DEFINITION
  PUBLIC
  INHERITING FROM /scwm/cl_api_handling_unit
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_hu_packtyp,
        huid    TYPE /scwm/de_whohuid,
        packtyp TYPE /scwm/s_huhdr_int-zz_packtyp,
      END OF ty_hu_packtyp .
    TYPES:
      tt_hu_packtyp TYPE STANDARD TABLE OF ty_hu_packtyp WITH EMPTY KEY .

    METHODS get_hu_packtyp
      IMPORTING
        iv_lgnum         TYPE /scwm/lgnum
        !it_whohu        TYPE /scwm/tt_whohu_int
      RETURNING
        VALUE(rt_result) TYPE tt_hu_packtyp .

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_API_HANDLING_UNIT IMPLEMENTATION.


  METHOD get_hu_packtyp.
********************************************************************
*& Key          : <BSUGAREV>-Sep 1, 2023
*& Request No.  :
********************************************************************
*& Description  : find packing type for HUID. Provided HUID from WHO
*&    must match HUIDs in the tasks
********************************************************************
    CONSTANTS: c_single_pc TYPE /scwm/ltap_vsolm VALUE '1'.
    DATA: lt_ordim_o TYPE /scwm/tt_ordim_o.

    IF lines( it_whohu ) = 0.
      RETURN.
    ENDIF.

    CALL FUNCTION '/SCWM/TO_READ_MULT'
      EXPORTING
        iv_lgnum    = iv_lgnum
        ir_who      = VALUE rseloption( FOR <l> IN it_whohu ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = <l>-who ) )
      IMPORTING
        et_ordim_o  = lt_ordim_o
      EXCEPTIONS
        wrong_input = 1
        not_found   = 2
        OTHERS      = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO DATA(lv_msg).
      RETURN.
    ENDIF.

    DATA(lo_pmat) = NEW zcl_packmmat_algo( iv_lgnum ).
    DATA(lt_pmats_totes) = lo_pmat->get_pmat_totes( ).
    DATA(lt_pmats_cartons) = lo_pmat->get_pmat_carton( ).

    LOOP AT lt_ordim_o ASSIGNING FIELD-SYMBOL(<ls_dummy>) GROUP BY ( huid = <ls_dummy>-huid )
                       ASSIGNING FIELD-SYMBOL(<lt_tasks>).

      DATA(lv_docid_delta) = VALUE /scwm/de_docid( ).
      DATA(lv_pack_type)   = VALUE zde_packtyp( ).
      DATA(lv_multi_dlv)   = VALUE boole_d( ).
      DATA(lv_single_piece_to) = abap_true.

      LOOP AT GROUP <lt_tasks> ASSIGNING FIELD-SYMBOL(<ls_task>).

        IF lv_docid_delta IS INITIAL.
          lv_docid_delta = <ls_task>-rdocid.
        ENDIF.

        IF lv_docid_delta <> <ls_task>-rdocid.
          lv_multi_dlv = abap_true.
        ENDIF.

        IF <ls_task>-vsolm <> c_single_pc.
          lv_single_piece_to = abap_false.
        ENDIF.
      ENDLOOP.

      DATA(ls_whohu) = VALUE #( it_whohu[ huid = <ls_task>-huid ] OPTIONAL ).

      CHECK ls_whohu IS NOT INITIAL.

      IF lv_multi_dlv = abap_true AND lv_single_piece_to = abap_false.
        rt_result = VALUE #( BASE rt_result ( huid = ls_whohu-huid  packtyp = zif_wme_c=>gs_hupacktyp-slo ) ).

      ELSEIF lv_multi_dlv = abap_true AND lv_single_piece_to = abap_true.
        rt_result = VALUE #( BASE rt_result ( huid = ls_whohu-huid  packtyp = zif_wme_c=>gs_hupacktyp-spo ) ).

      ELSEIF lv_multi_dlv = abap_false AND line_exists( lt_pmats_cartons[ matid = ls_whohu-pmat_guid ] ).
        rt_result = VALUE #( BASE rt_result ( huid = ls_whohu-huid  packtyp = zif_wme_c=>gs_hupacktyp-mc ) ).

      ELSEIF lv_multi_dlv = abap_false AND line_exists( lt_pmats_totes[ matid = ls_whohu-pmat_guid ] ).
        rt_result = VALUE #( BASE rt_result ( huid = ls_whohu-huid  packtyp = zif_wme_c=>gs_hupacktyp-tote ) ).

      ELSE.
        rt_result = VALUE #( BASE rt_result ( huid = ls_whohu-huid  packtyp = zif_wme_c=>gs_hupacktyp-sc ) ).

      ENDIF.

    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
