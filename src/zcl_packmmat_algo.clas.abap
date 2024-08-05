class ZCL_PACKMMAT_ALGO definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_pmats_incl_excl,
        pmat     TYPE /scwm/de_pmat,
        inc_excl TYPE	tvarv_sign,
      END OF ty_pmats_incl_excl .
  types:
    tt_pmats_incl_excl TYPE STANDARD TABLE OF ty_pmats_incl_excl WITH EMPTY KEY .
  types:
    BEGIN OF ty_mc_type,
        matid   TYPE /scwm/s_matid_matnr-matid,
        matnr   TYPE /scwm/s_matid_matnr-matnr,
        nonconv TYPE boole_d,
      END OF ty_mc_type .
  types:
    tt_mc_type TYPE STANDARD TABLE OF ty_mc_type WITH EMPTY KEY .

  methods CONSTRUCTOR
    importing
      !IV_LGNUM type /SCWM/LGNUM .
  methods FILTER_PMATS_BY_BP
    importing
      !IV_SPED type BOOLE_D optional
      !IS_BP type ZSTR_SHIP_BP
      !IT_PMATS type /SCWM/TT_MATID_MATNR
    returning
      value(RT_PMATS) type /SCWM/TT_PMAT_GUID .
  methods GET_MC_VOLUME
    importing
      !IV_MATID type /SCWM/DE_PMATID
      !IV_FROM_AUOM type /SCWM/DE_UNIT optional
    returning
      value(RS_RESULT) type /SCWM/S_QUAN .
  methods GET_PMAT_BY_MATID
    importing
      !IT_MATIDS type /SCMB/MDL_MATID_TAB
    returning
      value(RT_PACKSPEC_CONTENT) type /SCWM/TT_PACKSPEC_NESTED .
  methods GET_PMAT_CARTON
    returning
      value(RT_PMAT) type TT_MC_TYPE .
  methods GET_PMAT_PALLET_SPED
    returning
      value(RT_PMAT) type /SCWM/TT_MATID_MATNR .
  methods GET_PMAT_PLANNED_SHIPPING
    returning
      value(RT_PMAT) type /SCWM/TT_MATID_MATNR .
  methods GET_PMAT_TOTES
    returning
      value(RT_PMAT) type /SCWM/TT_MATID_MATNR .
  methods UPDATE_PMAT_DIMENSIONS
    changing
      !CT_PACKMAT type /SCWM/TT_WHO_PMAT .
  methods GET_PMAT_TOTES_PICK
    returning
      value(RT_PMAT) type /SCWM/TT_MATID_MATNR .
  PROTECTED SECTION.

  PRIVATE SECTION.

    DATA mv_lgnum TYPE /scwm/lgnum .

    METHODS get_pmats_by_pack_profile
      IMPORTING
        !iv_pack_prof  TYPE /scwm/pak_com_i-pak_rule
      RETURNING
        VALUE(rt_pmat) TYPE /scwm/tt_matid_matnr .
ENDCLASS.



CLASS ZCL_PACKMMAT_ALGO IMPLEMENTATION.


  METHOD constructor.
    mv_lgnum = iv_lgnum.
  ENDMETHOD.


  METHOD filter_pmats_by_bp.
    DATA: lv_pmat_include  TYPE /scwm/de_matid,
          lv_pmat_exclude  TYPE /scwm/de_matid,
          lt_pmat_excl_all TYPE rseloption.

    " get pack material filter for business parters
    IF iv_sped = abap_true.
      DATA(lt_bp_pmats) = zcl_crud_ztout_sped_pmat=>select_multi_by_lgnum( mv_lgnum ).
    ELSE.
      DATA(lt_bp_pmats_temp) = zcl_crud_ztout_ship_pmat=>select_multi_by_lgnum( mv_lgnum ).

      MOVE-CORRESPONDING lt_bp_pmats_temp TO lt_bp_pmats.
    ENDIF.

    " remove pack materials which BP do not match to the supplied ones
    LOOP AT lt_bp_pmats ASSIGNING FIELD-SYMBOL(<ls_ship_mat>).

      IF <ls_ship_mat>-carrier <> is_bp-carrier AND
         <ls_ship_mat>-ship_to <> is_bp-ship_to AND
         <ls_ship_mat>-sold_to <> is_bp-sold_to.

        DELETE lt_bp_pmats.
      ENDIF.

    ENDLOOP.

    " filter packaging materials
    LOOP AT it_pmats ASSIGNING FIELD-SYMBOL(<ls_pmat>).

      CLEAR: lv_pmat_include, lv_pmat_exclude.

      LOOP AT lt_bp_pmats ASSIGNING FIELD-SYMBOL(<ls_bp_pmat>) WHERE pmat = <ls_pmat>-matnr.
        CASE <ls_bp_pmat>-inc_excl.
          WHEN wmegc_sign_inclusive.
            lv_pmat_include = <ls_pmat>-matid.

          WHEN wmegc_sign_exclusive.
            lv_pmat_exclude = <ls_pmat>-matid.

        ENDCASE.
      ENDLOOP.

      " if there are mixed entries for the same material, they will not be used
      CHECK lv_pmat_include IS INITIAL OR lv_pmat_exclude IS INITIAL.

      IF lv_pmat_include IS NOT INITIAL.
        rt_pmats = VALUE #( BASE rt_pmats ( pmatid = lv_pmat_include ) ).
      ENDIF.

      IF lv_pmat_exclude IS NOT INITIAL.
        lt_pmat_excl_all = VALUE #( BASE lt_pmat_excl_all ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = lv_pmat_exclude ) ).
      ENDIF.
    ENDLOOP.

    " if rt_result = 0 that means that no entries in ztout_sped_pmat are found with INCL/EXCL = "I"
    IF lines( rt_pmats ) = 0.
      " Supplied list of pack materials is filtered by pack mats in customizing.
      rt_pmats = VALUE #( FOR <l> IN it_pmats ( pmatid = <l>-matid ) ).

      IF lines( lt_pmat_excl_all ) > 0.
        DELETE rt_pmats WHERE pmatid IN lt_pmat_excl_all.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_mc_volume.

    IF iv_matid IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_prod) = CAST /scwm/if_af_product(
        /scdl/cl_af_management=>get_instance( )->get_service( /scwm/if_af_product=>sc_me_as_service ) ).

    TRY.
        lo_prod->read_material_multiple(
          EXPORTING
            iv_lgnum      = mv_lgnum
            it_matid      = VALUE #( ( iv_matid ) )
            iv_notext     = abap_true
          IMPORTING
            et_mat_global = DATA(lt_mat_global)
            et_mat_uom    = DATA(lt_mat_uom) ).
      CATCH /scwm/cx_md_api_faulty_call ##NO_HANDLER.
      CATCH /scwm/cx_md_exception       ##NO_HANDLER.
    ENDTRY.


    DATA(ls_global) = REF #( lt_mat_global[ matid = iv_matid ] OPTIONAL ).
    IF ls_global IS INITIAL.
      RETURN.
    ENDIF.

    IF iv_from_auom = abap_false.
      DATA(ls_mat_uom) = VALUE #( lt_mat_uom[ matid = iv_matid meinh = ls_global->meins ] OPTIONAL ).

    ELSE.
      DELETE lt_mat_uom WHERE meinh = ls_global->meins.

      SORT lt_mat_uom BY umrez ASCENDING.

      ls_mat_uom = VALUE #( lt_mat_uom[ 1 ] OPTIONAL ).
    ENDIF.

    rs_result-quan = ls_mat_uom-volum.
    rs_result-unit = ls_mat_uom-voleh.
  ENDMETHOD.


  METHOD get_pmats_by_pack_profile.
    DATA: ls_t340d            TYPE  /scwm/s_t340d,
          ls_t300_md          TYPE  /scwm/s_t300_md,
          lt_packspec         TYPE  /scwm/tt_guid_ps,
          lt_packspec_content TYPE  /scwm/tt_packspec_nested.

    CALL FUNCTION '/SCWM/T340D_READ_SINGLE'
      EXPORTING
        iv_lgnum  = mv_lgnum
      IMPORTING
        es_t340d  = ls_t340d
      EXCEPTIONS
        not_found = 1
        OTHERS    = 99.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION '/SCWM/T300_MD_READ_SINGLE'
      EXPORTING
        iv_lgnum   = mv_lgnum
      IMPORTING
        es_t300_md = ls_t300_md
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 99.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " run packspec determination
    DATA(ls_fields) = VALUE /scwm/pak_com_i( pak_rule = iv_pack_prof
                                             pak_locid = ls_t300_md-scuguid ).

    CALL FUNCTION '/SCWM/PS_FIND_AND_EVALUATE'
      EXPORTING
        is_fields       = ls_fields
        iv_procedure    = ls_t340d-whoctlist
        i_data          = VALUE /scwm/dlv_docid_item_str( )
      IMPORTING
        et_packspec     = lt_packspec
      EXCEPTIONS
        determine_error = 1
        read_error      = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT lt_packspec ASSIGNING FIELD-SYMBOL(<ls_packspec>).

      CALL FUNCTION '/SCWM/PS_PACKSPEC_GET'
        EXPORTING
          iv_guid_ps          = <ls_packspec>
          iv_read_elements    = abap_true
          iv_no_buffer        = abap_false
        IMPORTING
          et_packspec_content = lt_packspec_content
        EXCEPTIONS
          error               = 1
          OTHERS              = 2.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      SORT lt_packspec_content BY content-content_seq DESCENDING.
      DATA(ls_pscont) = VALUE #( lt_packspec_content[ 1 ] OPTIONAL ).

      CHECK ls_pscont IS NOT INITIAL.

      SORT ls_pscont-levels BY display_seq DESCENDING.
      DATA(ls_level) = VALUE #( ls_pscont-levels[ 1 ] OPTIONAL ).

      CHECK ls_level IS NOT INITIAL.

      rt_pmat = VALUE #( BASE rt_pmat ( matid = ls_level-hu_matid matnr = ls_level-hu_mat ) ).

    ENDLOOP.
  ENDMETHOD.


  METHOD get_pmat_by_matid.
    " run packspec determination
    DATA(lt_ps_keys) = VALUE /scwm/tt_ps_header_key( ).

    CALL FUNCTION '/SCWM/API_PACKSPEC_GETLIST'
      EXPORTING
        is_content_query = VALUE /scwm/s_ps_content_query( matid = it_matids )
      IMPORTING
        et_ps_keys       = lt_ps_keys.

    LOOP AT lt_ps_keys ASSIGNING FIELD-SYMBOL(<ls_packspec>).

      DATA(lt_packspec_content_temp) = VALUE /scwm/tt_packspec_nested( ).
      CALL FUNCTION '/SCWM/PS_PACKSPEC_GET'
        EXPORTING
          iv_guid_ps          = <ls_packspec>-guid_ps
          iv_read_elements    = abap_true
          iv_no_buffer        = abap_false
        IMPORTING
          et_packspec_content = lt_packspec_content_temp
        EXCEPTIONS
          error               = 1
          OTHERS              = 2.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      APPEND LINES OF lt_packspec_content_temp TO rt_packspec_content.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_pmat_carton.
    DATA: lv_pack_prof TYPE zde_param_low.

    zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = mv_lgnum
        iv_process   = zif_param_const=>c_zgen_0002
        iv_parameter = zif_param_const=>c_pmat_mc_conv
      IMPORTING
        ev_constant  = lv_pack_prof ).

    DATA(lt_pmats) = get_pmats_by_pack_profile( iv_pack_prof = CONV #( lv_pack_prof ) ).

    rt_pmat = VALUE #( FOR <pm> IN lt_pmats ( matid = <pm>-matid matnr = <pm>-matnr ) ).

    CLEAR: lv_pack_prof, lt_pmats.
    zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = mv_lgnum
        iv_process   = zif_param_const=>c_zgen_0002
        iv_parameter = zif_param_const=>c_pmat_mc_nonconv
      IMPORTING
        ev_constant  = lv_pack_prof ).

    lt_pmats = get_pmats_by_pack_profile( iv_pack_prof = CONV #( lv_pack_prof ) ).

    rt_pmat = VALUE #( BASE rt_pmat FOR <pm> IN lt_pmats
        ( matid = <pm>-matid matnr = <pm>-matnr nonconv = abap_true ) ).

  ENDMETHOD.


  METHOD get_pmat_pallet_sped.
    zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = mv_lgnum
        iv_process   = zif_param_const=>c_zgen_0005
        iv_parameter = zif_param_const=>c_pmat_sped_pallet
      IMPORTING
        ev_constant  = DATA(lv_pack_prof) ).

    rt_pmat = get_pmats_by_pack_profile( iv_pack_prof = CONV #( lv_pack_prof ) ).
  ENDMETHOD.


  METHOD get_pmat_planned_shipping.
    zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = mv_lgnum
        iv_process   = zif_param_const=>c_zgen_0004
        iv_parameter = zif_param_const=>c_pmat_planned_ship
      IMPORTING
        ev_constant  = DATA(lv_pack_prof) ).

    rt_pmat = get_pmats_by_pack_profile( iv_pack_prof = CONV #( lv_pack_prof ) ).
  ENDMETHOD.


  METHOD get_pmat_totes.
    zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = mv_lgnum
        iv_process   = zif_param_const=>c_zgen_0003
        iv_parameter = zif_param_const=>c_pmat_aarea
      IMPORTING
        ev_constant  = DATA(lv_pack_prof) ).

    rt_pmat = get_pmats_by_pack_profile( iv_pack_prof = CONV #( lv_pack_prof ) ).
  ENDMETHOD.


  METHOD get_pmat_totes_pick.
    zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = mv_lgnum
        iv_process   = zif_param_const=>c_zgen_0007
        iv_parameter = zif_param_const=>c_pmat_aarea
      IMPORTING
        ev_constant  = DATA(lv_pack_prof) ).

    rt_pmat = get_pmats_by_pack_profile( iv_pack_prof = CONV #( lv_pack_prof ) ).
  ENDMETHOD.


  METHOD update_pmat_dimensions.

    IF lines( ct_packmat ) = 0.
      RETURN.
    ENDIF.

    DATA(lo_prod) = CAST /scwm/if_af_product( /scdl/cl_af_management=>get_instance(
        )->get_service( /scwm/if_af_product=>sc_me_as_service ) ).

    TRY.
        lo_prod->read_material_multiple(
          EXPORTING
            it_matid      = VALUE #( FOR <pm> IN ct_packmat WHERE ( max_length IS INITIAL ) ( <pm>-pmat_guid ) )
          IMPORTING
            et_mat_global = DATA(lt_mat_global)
            et_mat_pack   = DATA(lt_mat_pack) ).
      CATCH /scwm/cx_md_api_faulty_call ##NO_HANDLER.
      CATCH /scwm/cx_md_exception       ##NO_HANDLER.
    ENDTRY.

    " upate list of Pack materials with dimensions data
    LOOP AT ct_packmat ASSIGNING FIELD-SYMBOL(<ls_pmat_alg>).

      DATA(ls_mat_global) = REF #( lt_mat_global[ matid = <ls_pmat_alg>-pmat_guid ] OPTIONAL ).
      CHECK ls_mat_global IS NOT INITIAL.

      DATA(ls_mat_pack) = REF #( lt_mat_pack[ matid = <ls_pmat_alg>-pmat_guid ] OPTIONAL ).
      CHECK ls_mat_pack IS NOT INITIAL.

      " Pack material &1 will be used for cuboid algorithm.
*      MESSAGE i013(zmc_out) WITH ls_mat_global->matnr INTO mv_msg.
*      add_message( ).

      <ls_pmat_alg>-max_length = ls_mat_pack->maxl.
      <ls_pmat_alg>-max_width  = ls_mat_pack->maxb.
      <ls_pmat_alg>-max_height = ls_mat_pack->maxh.
      <ls_pmat_alg>-unit_lwh   = ls_mat_pack->maxdim_uom.

      <ls_pmat_alg>-max_volume = ls_mat_pack->maxv.
      <ls_pmat_alg>-unit_v     = ls_mat_pack->maxv_uom.

      <ls_pmat_alg>-max_weight = ls_mat_pack->maxw.
      <ls_pmat_alg>-unit_w     = ls_mat_pack->maxw_uom.

    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
