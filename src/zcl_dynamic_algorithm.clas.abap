class ZCL_DYNAMIC_ALGORITHM definition
  public
  final
  create public .

public section.

  methods CALC_BINTYP_CAPACITY
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_ENTITLED type /SCWM/DE_ENTITLED
      !IV_MATID type /SCWM/DE_MATID
      !IT_LGTYP type /SCWM/TT_LGTYP_R
      !IS_MAT_LGTYP type /SCWM/S_MATERIAL_LGTYP optional
    exporting
      !ET_LPTYP_MAXQTY type ZCL_CRUD_ZTLPTYP_MAXQTY=>TT_LPTYP_MAXQTY
      !ET_LGTYP_FAILED type /SCWM/TT_LGTYP
      !ET_BAPIRET type BAPIRETTAB .
  methods CALC_WITHOUT_LGTYP_DATA
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_ENTITLED type /SCWM/DE_ENTITLED
      !IV_MATID type /SCWM/DE_MATID
      !IT_LGTYP type /SCWM/TT_LGTYP_R
      !IS_MAT_LGTYP type /SCWM/S_MATERIAL_LGTYP optional
    exporting
      !ET_LPTYP_MAXQTY type ZCL_CRUD_ZTLPTYP_MAXQTY=>TT_LPTYP_MAXQTY
      !ET_LGTYP_FAILED type /SCWM/TT_LGTYP
      !ET_BAPIRET type BAPIRETTAB .
  PROTECTED SECTION.
private section.
    ##NEEDED
  data MV_MSG type STRING .

  methods ADD_MSG_TO_BAPIRET
    changing
      !CT_BAPIRET type BAPIRETTAB .
  methods GET_LPTYP_DIM
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IT_T303S type /SCWM/TT_T303S
    returning
      value(RT_LPTYP_DIM) type /SCWM/TT_T303 .
ENDCLASS.



CLASS ZCL_DYNAMIC_ALGORITHM IMPLEMENTATION.


  METHOD add_msg_to_bapiret.
********************************************************************
*& Key          : BSUGAREV-Jan 17, 2024
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  :
*&
*&
********************************************************************
    ct_bapiret = VALUE #( BASE ct_bapiret ( id = sy-msgid type = sy-msgty number = sy-msgno
                          message_v1 = sy-msgv1 message_v2 = sy-msgv2 message_v3 = sy-msgv3 message_v4 = sy-msgv4 ) ).
  ENDMETHOD.


  METHOD calc_bintyp_capacity.
********************************************************************
*& Key          : BSUGAREV-Jan 17, 2024
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA: lv_matnr            TYPE /scwm/de_matnr,
          lv_calc_qty         TYPE /scwm/de_maxqty,
          lt_ps_keys          TYPE /scwm/tt_ps_header_key,
          lt_packspec_content TYPE /scwm/tt_packspec_nested,
          lt_mat_lgtyp        TYPE /scwm/tt_material_lgtyp,
          lt_mat_uom          TYPE /scwm/tt_material_uom,
          ls_mat_global       TYPE /scwm/s_material_global.

    CLEAR: et_lgtyp_failed, et_lptyp_maxqty.

    MESSAGE i009(zmc_ztlptyp_maxqty) INTO mv_msg.
    add_msg_to_bapiret( CHANGING ct_bapiret = et_bapiret ).

    DATA(lo_conv) = /scmb/cl_md_access_mdl=>get_md_access( ).

    DATA(lt_lgtyps_in) = it_lgtyp.

    DATA(lt_t303s) = zcl_crud_scwm_t303s=>select_multi(
      iv_lgnum   = iv_lgnum
      it_lgtyp_r = lt_lgtyps_in ).

    " determine materials storage data, we are interested from direct replenishment indicator
    TRY.
        CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
          EXPORTING
            iv_matid      = iv_matid
            iv_entitled   = iv_entitled
            iv_lgnum      = iv_lgnum
            it_lgtyp      = VALUE /scwm/tt_lgtyp( FOR <l> IN lt_t303s ( <l>-lgtyp ) )
          IMPORTING
            es_mat_global = ls_mat_global
            et_mat_uom    = lt_mat_uom
            et_mat_lgtyp  = lt_mat_lgtyp.

        SORT lt_mat_lgtyp BY lgtyp.
        DELETE ADJACENT DUPLICATES FROM lt_mat_lgtyp COMPARING lgtyp.
      CATCH /scwm/cx_md ##NO_HANDLER.
    ENDTRY.

    IF lines( lt_mat_lgtyp ) = 0
      AND is_mat_lgtyp IS INITIAL.
      NEW /scwm/cl_ui_stock_fields( )->get_matkey_by_id(
        EXPORTING
          iv_matid = iv_matid
        IMPORTING
          ev_matnr = lv_matnr ).

      MESSAGE w006(zmc_ztlptyp_maxqty) WITH lv_matnr INTO mv_msg.
      add_msg_to_bapiret( CHANGING ct_bapiret = et_bapiret ).

      RETURN.

    ELSEIF is_mat_lgtyp IS NOT INITIAL.

      "in case this method has been called from the slotting transaction
      "get data from is_mat_lgtyp
      "because the result from read_material_multiple could be empty
      "while there is no storage type data for the material yet
      APPEND VALUE /scwm/s_material_lgtyp( matid = is_mat_lgtyp-matid
                                             entitled = is_mat_lgtyp-entitled
                                             lgtyp = is_mat_lgtyp-lgtyp
                                             lgnum = is_mat_lgtyp-lgnum ) TO lt_mat_lgtyp.

    ENDIF.

    LOOP AT lt_mat_lgtyp ASSIGNING FIELD-SYMBOL(<ls_locprod>).
      IF <ls_locprod>-zz1_dirrpl_stt IS NOT INITIAL.

        IF lv_matnr IS INITIAL.
          NEW /scwm/cl_ui_stock_fields( )->get_matkey_by_id(
            EXPORTING
              iv_matid = iv_matid
            IMPORTING
              ev_matnr = lv_matnr ).
        ENDIF.

        MESSAGE i007(zmc_ztlptyp_maxqty) WITH lv_matnr <ls_locprod>-lgtyp INTO mv_msg.
        add_msg_to_bapiret( CHANGING ct_bapiret = et_bapiret ).

        APPEND <ls_locprod>-lgtyp TO et_lgtyp_failed.
        DELETE lt_mat_lgtyp.
      ENDIF.
    ENDLOOP.

    IF lines( lt_mat_lgtyp ) = 0.
      RETURN.
    ENDIF.

    zcl_crud_ztlptyp_defqty=>select_multi_by_lgnum_lptyp(
      EXPORTING
        iv_lgnum          = iv_lgnum
        it_lptyp_r        = VALUE #( FOR <lptyp> IN lt_t303s
                                     FOR <lp> IN lt_mat_lgtyp WHERE ( lgtyp = <lptyp>-lgtyp )
                                     ( sign   = wmegc_sign_inclusive
                                       option = wmegc_option_eq
                                       low    = <lptyp>-lptyp ) )
      IMPORTING
        et_ztlptyp_defqty = DATA(lt_ztlptyp_defqty) ).


    CALL FUNCTION '/SCWM/API_PACKSPEC_GETLIST'
      EXPORTING
        is_content_query = VALUE /scwm/s_ps_content_query( matid = VALUE #( ( matid = iv_matid ) ) )
        it_status_rng    = VALUE rseloption( ( sign   = wmegc_sign_inclusive
                                               option = wmegc_option_eq
                                               low = /scwm/cl_ppelipak_cntl_const=>gc_status_active ) )
      IMPORTING
        et_ps_keys       = lt_ps_keys.

    CALL FUNCTION '/SCWM/PS_PACKSPEC_GET'
      EXPORTING
        iv_guid_ps          = VALUE #( lt_ps_keys[ 1 ]-guid_ps OPTIONAL )
        iv_read_elements    = abap_true
        iv_no_buffer        = abap_false
      IMPORTING
        et_packspec_content = lt_packspec_content
      EXCEPTIONS
        error               = 1
        OTHERS              = 2.
    IF sy-subrc <> 0.
      " Implement suitable error handling here
    ENDIF.

    TRY.
        DATA(lv_qty_per_tote) = VALUE #( lt_packspec_content[ 1 ]-levels[ quancla = zif_wme_c=>gs_quancla-totes ]-trgqty ).
        DATA(lv_uom_per_tote) = VALUE #( lt_packspec_content[ 1 ]-levels[ quancla = zif_wme_c=>gs_quancla-totes ]-total_unit ).
      CATCH cx_sy_itab_line_not_found ##NO_HANDLER.
    ENDTRY.

    IF lv_qty_per_tote = 0.
      MESSAGE w008(zmc_ztlptyp_maxqty) WITH '' INTO mv_msg.
      add_msg_to_bapiret( CHANGING ct_bapiret = et_bapiret ).

      RETURN.
    ENDIF.

    DATA(lt_t303) = get_lptyp_dim( iv_lgnum = iv_lgnum
                                   it_t303s = lt_t303s ).

    " Use MC height (Z0x)
    "Always to use the smallest available Z* UOM
    DELETE lt_mat_uom WHERE meinh = ls_mat_global-meins.
    IF lines( lt_mat_uom ) = 0.
      MESSAGE e028(zmc_out) WITH ls_mat_global-matnr INTO mv_msg.
      add_msg_to_bapiret( CHANGING ct_bapiret = et_bapiret ).

      RETURN.
    ENDIF.

    SORT lt_mat_uom BY umrez ASCENDING.

    DATA(lv_height_mc)     = lt_mat_uom[ 1 ]-hoehe.
    DATA(lv_height_uom_mc) = lt_mat_uom[ 1 ]-meabm.
    " Use product height (PC)
    TRY.
        DATA(lv_height_pr)     = VALUE #( lt_packspec_content[ 1 ]-levels[ quancla = zif_wme_c=>gs_quancla-totes ]-max_height ).
        DATA(lv_height_uom_pr) = VALUE #( lt_packspec_content[ 1 ]-levels[ quancla = zif_wme_c=>gs_quancla-totes ]-unit_max_lwh ).
      CATCH cx_sy_itab_line_not_found ##NO_HANDLER.
    ENDTRY.

    IF lv_height_pr > lv_height_mc.
      DATA(lv_height_chk) = lv_height_pr.
      DATA(lv_height_uom_chk) = lv_height_uom_pr.
    ELSE.
      lv_height_chk = lv_height_mc.
      lv_height_uom_chk = lv_height_uom_mc.
    ENDIF.

    LOOP AT lt_t303s ASSIGNING FIELD-SYMBOL(<ls_dummy>) GROUP BY ( lgtyp = <ls_dummy>-lgtyp )
                     ASSIGNING FIELD-SYMBOL(<ls_bintyps>).

      LOOP AT GROUP <ls_bintyps> ASSIGNING FIELD-SYMBOL(<ls_bintyp>).
        CLEAR: lv_calc_qty.

        DATA(ls_lptyp_defqty) = REF #( lt_ztlptyp_defqty[ lgnum = iv_lgnum
                                                          lptyp = <ls_bintyp>-lptyp ] OPTIONAL ).
        CHECK ls_lptyp_defqty IS NOT INITIAL AND ls_lptyp_defqty->defqty > 0.

        DATA(lv_height_per_tote) = VALUE #( lt_t303[ lptyp = <ls_bintyp>-lptyp ]-max_height OPTIONAL ).
        DATA(lv_height_uom_per_tote) = VALUE #( lt_t303[ lptyp = <ls_bintyp>-lptyp ]-unit_lwh OPTIONAL ).

        IF lv_height_uom_per_tote <> lv_height_uom_chk.
          " update Height
          TRY.
              DATA(lv_height_conv) = lo_conv->prod_quan_conversion(
                iv_prodid   = iv_matid
                iv_uom_from = lv_height_uom_chk
                iv_uom_to   = lv_height_uom_per_tote
                iv_quan     = CONV #( lv_height_chk ) ).
              ##NO_HANDLER
            CATCH /scmb/cx_md_access.
          ENDTRY.
        ENDIF.

        IF lv_height_conv > lv_height_per_tote.
          lv_calc_qty = 0.
        ELSE.
          lv_calc_qty = lv_qty_per_tote * ls_lptyp_defqty->defqty.
        ENDIF.

        INSERT VALUE #( lgnum = iv_lgnum
                        lgtyp = <ls_bintyp>-lgtyp
                        lptyp = <ls_bintyp>-lptyp
                        matid = iv_matid
                        max_qty = lv_calc_qty
                        uom = lv_uom_per_tote ) INTO TABLE et_lptyp_maxqty.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD calc_without_lgtyp_data.
********************************************************************
*& Key          : AAHMEDOV-Jan 22, 2024
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  : This method will be used to calculate the
*& maximum quantity ber storage bin type in case there is no
*& storage type data for the product yet
********************************************************************
    DATA: lv_matnr            TYPE /scwm/de_matnr,
          lv_calc_qty         TYPE /scwm/de_maxqty,
          lt_ps_keys          TYPE /scwm/tt_ps_header_key,
          lt_packspec_content TYPE /scwm/tt_packspec_nested,
          lt_mat_lgtyp        TYPE /scwm/tt_material_lgtyp,
          lt_mat_uom          TYPE /scwm/tt_material_uom,
          ls_mat_global       TYPE /scwm/s_material_global.

    CLEAR: et_lgtyp_failed, et_lptyp_maxqty.

    MESSAGE i009(zmc_ztlptyp_maxqty) INTO mv_msg.
    add_msg_to_bapiret( CHANGING ct_bapiret = et_bapiret ).

    DATA(lo_conv) = /scmb/cl_md_access_mdl=>get_md_access( ).

    DATA(lt_lgtyps_in) = it_lgtyp.

    DATA(lt_t303s) = zcl_crud_scwm_t303s=>select_multi(
      iv_lgnum   = iv_lgnum
      it_lgtyp_r = lt_lgtyps_in ).

    " determine materials storage data, we are interested from direct replenishment indicator
    TRY.
        CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
          EXPORTING
            iv_matid      = iv_matid
            iv_entitled   = iv_entitled
            iv_lgnum      = iv_lgnum
            it_lgtyp      = VALUE /scwm/tt_lgtyp( FOR <l> IN lt_t303s ( <l>-lgtyp ) )
          IMPORTING
            es_mat_global = ls_mat_global
            et_mat_uom    = lt_mat_uom.
*            et_mat_lgtyp  = lt_mat_lgtyp.

*        SORT lt_mat_lgtyp BY lgtyp.
*        DELETE ADJACENT DUPLICATES FROM lt_mat_lgtyp COMPARING lgtyp.
      CATCH /scwm/cx_md ##NO_HANDLER.
    ENDTRY.

*    IF lines( lt_mat_lgtyp ) = 0
*      AND is_mat_lgtyp IS INITIAL.
*      NEW /scwm/cl_ui_stock_fields( )->get_matkey_by_id(
*        EXPORTING
*          iv_matid = iv_matid
*        IMPORTING
*          ev_matnr = lv_matnr ).
*
*      MESSAGE w006(zmc_ztlptyp_maxqty) WITH lv_matnr INTO mv_msg.
*      add_msg_to_bapiret( CHANGING ct_bapiret = et_bapiret ).
*
*      RETURN.
*
*    ELSEIF is_mat_lgtyp IS NOT INITIAL.
*
*      "in case this method has been called from the slotting transaction
*      "get data from is_mat_lgtyp
*      "because the result from read_material_multiple could be empty
*      "while there is no storage type data for the material yet
*      APPEND VALUE /scwm/s_material_lgtyp( matid = is_mat_lgtyp-matid
*                                             entitled = is_mat_lgtyp-entitled
*                                             lgtyp = is_mat_lgtyp-lgtyp
*                                             lgnum = is_mat_lgtyp-lgnum ) TO lt_mat_lgtyp.
*
*    ENDIF.

*    LOOP AT lt_mat_lgtyp ASSIGNING FIELD-SYMBOL(<ls_locprod>).
*      IF <ls_locprod>-zz1_dirrpl_stt IS NOT INITIAL.
*
*        IF lv_matnr IS INITIAL.
*          NEW /scwm/cl_ui_stock_fields( )->get_matkey_by_id(
*            EXPORTING
*              iv_matid = iv_matid
*            IMPORTING
*              ev_matnr = lv_matnr ).
*        ENDIF.
*
*        MESSAGE i007(zmc_ztlptyp_maxqty) WITH lv_matnr <ls_locprod>-lgtyp INTO mv_msg.
*        add_msg_to_bapiret( CHANGING ct_bapiret = et_bapiret ).
*
*        APPEND <ls_locprod>-lgtyp TO et_lgtyp_failed.
*        DELETE lt_mat_lgtyp.
*      ENDIF.
*    ENDLOOP.

*    IF lines( lt_mat_lgtyp ) = 0.
*      RETURN.
*    ENDIF.

    zcl_crud_ztlptyp_defqty=>select_multi_by_lgnum_lptyp(
      EXPORTING
        iv_lgnum          = iv_lgnum
        it_lptyp_r        = VALUE #( FOR <lptyp> IN lt_t303s
*                                     FOR <lp> IN lt_mat_lgtyp WHERE ( lgtyp = <lptyp>-lgtyp )
                                     ( sign   = wmegc_sign_inclusive
                                       option = wmegc_option_eq
                                       low    = <lptyp>-lptyp ) )
      IMPORTING
        et_ztlptyp_defqty = DATA(lt_ztlptyp_defqty) ).


    CALL FUNCTION '/SCWM/API_PACKSPEC_GETLIST'
      EXPORTING
        is_content_query = VALUE /scwm/s_ps_content_query( matid = VALUE #( ( matid = iv_matid ) ) )
        it_status_rng    = VALUE rseloption( ( sign   = wmegc_sign_inclusive
                                               option = wmegc_option_eq
                                               low = /scwm/cl_ppelipak_cntl_const=>gc_status_active ) )
      IMPORTING
        et_ps_keys       = lt_ps_keys.

    CALL FUNCTION '/SCWM/PS_PACKSPEC_GET'
      EXPORTING
        iv_guid_ps          = VALUE #( lt_ps_keys[ 1 ]-guid_ps OPTIONAL )
        iv_read_elements    = abap_true
        iv_no_buffer        = abap_false
      IMPORTING
        et_packspec_content = lt_packspec_content
      EXCEPTIONS
        error               = 1
        OTHERS              = 2.
    IF sy-subrc <> 0.
      " Implement suitable error handling here
    ENDIF.

    TRY.
        DATA(lv_qty_per_tote) = VALUE #( lt_packspec_content[ 1 ]-levels[ quancla = zif_wme_c=>gs_quancla-totes ]-trgqty ).
        DATA(lv_uom_per_tote) = VALUE #( lt_packspec_content[ 1 ]-levels[ quancla = zif_wme_c=>gs_quancla-totes ]-total_unit ).
      CATCH cx_sy_itab_line_not_found ##NO_HANDLER.
    ENDTRY.

    IF lv_qty_per_tote = 0.
      MESSAGE w008(zmc_ztlptyp_maxqty) WITH '' INTO mv_msg.
      add_msg_to_bapiret( CHANGING ct_bapiret = et_bapiret ).

      RETURN.
    ENDIF.

    DATA(lt_t303) = get_lptyp_dim( iv_lgnum = iv_lgnum
                                   it_t303s = lt_t303s ).

    " Use MC height (Z0x)
    "Always to use the smallest available Z* UOM
    DELETE lt_mat_uom WHERE meinh = ls_mat_global-meins.
    IF lines( lt_mat_uom ) = 0.
      MESSAGE e028(zmc_out) WITH ls_mat_global-matnr INTO mv_msg.
      add_msg_to_bapiret( CHANGING ct_bapiret = et_bapiret ).

      RETURN.
    ENDIF.

    SORT lt_mat_uom BY umrez ASCENDING.

    DATA(lv_height_mc)     = lt_mat_uom[ 1 ]-hoehe.
    DATA(lv_height_uom_mc) = lt_mat_uom[ 1 ]-meabm.
    " Use product height (PC)
    TRY.
        DATA(lv_height_pr)     = VALUE #( lt_packspec_content[ 1 ]-levels[ quancla = zif_wme_c=>gs_quancla-totes ]-max_height ).
        DATA(lv_height_uom_pr) = VALUE #( lt_packspec_content[ 1 ]-levels[ quancla = zif_wme_c=>gs_quancla-totes ]-unit_max_lwh ).
      CATCH cx_sy_itab_line_not_found ##NO_HANDLER.
    ENDTRY.

    IF lv_height_pr > lv_height_mc.
      DATA(lv_height_chk) = lv_height_pr.
      DATA(lv_height_uom_chk) = lv_height_uom_pr.
    ELSE.
      lv_height_chk = lv_height_mc.
      lv_height_uom_chk = lv_height_uom_mc.
    ENDIF.

    LOOP AT lt_t303s ASSIGNING FIELD-SYMBOL(<ls_dummy>) GROUP BY ( lgtyp = <ls_dummy>-lgtyp )
                     ASSIGNING FIELD-SYMBOL(<ls_bintyps>).

      LOOP AT GROUP <ls_bintyps> ASSIGNING FIELD-SYMBOL(<ls_bintyp>).
        CLEAR: lv_calc_qty.

        DATA(ls_lptyp_defqty) = REF #( lt_ztlptyp_defqty[ lgnum = iv_lgnum
                                                          lptyp = <ls_bintyp>-lptyp ] OPTIONAL ).
        CHECK ls_lptyp_defqty IS NOT INITIAL AND ls_lptyp_defqty->defqty > 0.

        DATA(lv_height_per_tote) = VALUE #( lt_t303[ lptyp = <ls_bintyp>-lptyp ]-max_height OPTIONAL ).
        DATA(lv_height_uom_per_tote) = VALUE #( lt_t303[ lptyp = <ls_bintyp>-lptyp ]-unit_lwh OPTIONAL ).

        IF lv_height_uom_per_tote <> lv_height_uom_chk.
          " update Height
          TRY.
              DATA(lv_height_conv) = lo_conv->prod_quan_conversion(
                iv_prodid   = iv_matid
                iv_uom_from = lv_height_uom_chk
                iv_uom_to   = lv_height_uom_per_tote
                iv_quan     = CONV #( lv_height_chk ) ).
              ##NO_HANDLER
            CATCH /scmb/cx_md_access.
          ENDTRY.
        ENDIF.

        IF lv_height_conv > lv_height_per_tote.
          lv_calc_qty = 0.
        ELSE.
          lv_calc_qty = lv_qty_per_tote * ls_lptyp_defqty->defqty.
        ENDIF.

        INSERT VALUE #( lgnum = iv_lgnum
                        lgtyp = <ls_bintyp>-lgtyp
                        lptyp = <ls_bintyp>-lptyp
                        matid = iv_matid
                        max_qty = lv_calc_qty
                        uom = lv_uom_per_tote ) INTO TABLE et_lptyp_maxqty.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_lptyp_dim.
********************************************************************
*& Key          : BSUGAREV-Jan 17, 2024
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA:
          ls_wme_def TYPE /scwm/s_t340d.

    DATA(ls_selopt_bintype) = VALUE rseloption( FOR <bintype> IN it_t303s
                                             WHERE ( lptyp IS NOT INITIAL )
                                                      ( sign   = wmegc_sign_inclusive
                                                        option = wmegc_option_eq
                                                        low    = <bintype>-lptyp  ) ).

    SELECT * FROM /scwm/t303
        INTO TABLE @rt_lptyp_dim
       WHERE lgnum = @iv_lgnum
         AND lptyp IN @ls_selopt_bintype.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION '/SCWM/T340D_READ_SINGLE'
      EXPORTING
        iv_lgnum  = iv_lgnum
      IMPORTING
        es_t340d  = ls_wme_def
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT rt_lptyp_dim ASSIGNING FIELD-SYMBOL(<ls_dim>).
      IF <ls_dim>-unit_lwh IS INITIAL.
        <ls_dim>-unit_lwh = ls_wme_def-dist_uom.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
