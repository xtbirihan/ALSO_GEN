class ZCL_FLOWRACK_ALGORITHM definition
  public
  final
  create public .

public section.

  methods CALC_CAPACITY
    importing
      !IV_MATID type /SCWM/DE_MATID
      !IV_ENTITLED type /SCWM/DE_ENTITLED
      !IT_BINTYPES type /SCWM/TT_T303S
    returning
      value(RT_MAXQTY) type ZCL_CRUD_ZTLPTYP_MAXQTY=>TT_LPTYP_MAXQTY .
  methods CONSTRUCTOR
    importing
      !IV_LGNUM type /SCWM/LGNUM
    raising
      ZCX_CORE_EXCEPTION .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF ms_material_flowrack ,
      matid    TYPE /scwm/de_matid,
      quan     TYPE /scwm/ltap_vsolm,
      meins    TYPE /scwm/de_base_uom,
      length   TYPE laeng,
      width    TYPE breit,
      height   TYPE hoehe,
      unit_lwh TYPE /scwm/dimeh,
    END OF ms_material_flowrack .

  data MS_WME_DEFAULT type /SCWM/S_T340D .
  data MV_LGNUM type /SCWM/LGNUM .

  methods PREPARE_MAT_DATA
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_MATID type /SCWM/DE_MATID
      !IV_ENTITLED type /SCWM/DE_ENTITLED
      !IV_LGTYP type /SCWM/LGTYP
      !IS_BIN_DATA type /SCWM/S_WHO_PMAT
    returning
      value(RS_MAT) type MS_MATERIAL_FLOWRACK .
  methods READ_PACK_SPEC
    importing
      !IV_MATID type /SCWM/DE_MATID
    returning
      value(RS_PACKSPEC) type /SCWM/S_PS_LEVEL_INT .
ENDCLASS.



CLASS ZCL_FLOWRACK_ALGORITHM IMPLEMENTATION.


  METHOD calc_capacity.
**********************************************************************
*& Key           : RM-230428
*& Request No.   : GAP-39 FS Bin capacity method for KDL bins
**********************************************************************
*& Description (short)
*&
**********************************************************************

    DATA:
      lv_numb_cart1   TYPE i,
      lv_numb_cart2   TYPE i,
      lv_max_cart     TYPE i,
      lv_max_pc       TYPE i.

    DATA(ls_bintyp_base) = VALUE #( it_bintypes[ 1 ] OPTIONAL ).

    DATA(ls_selopt_bintype) = VALUE rseloption( FOR <bintype> IN it_bintypes
                                                WHERE ( lptyp IS NOT INITIAL )
                                                      ( sign   = wmegc_sign_inclusive
                                                        option = wmegc_option_eq
                                                        low    = <bintype>-lptyp  ) ).

    IF ls_selopt_bintype IS INITIAL.
      RETURN.
    ENDIF.

    " Select Storage Bin Types to determine bin types data -  width & length
    SELECT * FROM /scwm/t303
             INTO TABLE @DATA(lt_t303)
            WHERE lgnum = @mv_lgnum
              AND lptyp IN @ls_selopt_bintype.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT lt_t303 ASSIGNING FIELD-SYMBOL(<ls_t303>).

      CLEAR: lv_numb_cart1, lv_numb_cart2, lv_max_cart, lv_max_pc.

      DATA(ls_bin_data) = VALUE /scwm/s_who_pmat(
                          max_length = <ls_t303>-max_length
                          max_width  = <ls_t303>-max_width
                          unit_lwh   = <ls_t303>-unit_lwh
                          max_height = <ls_t303>-max_height ).

      IF ls_bin_data-unit_lwh IS INITIAL.
        ls_bin_data-unit_lwh = ms_wme_default-dist_uom.
      ENDIF.

      DATA(ls_mat) = prepare_mat_data( iv_lgnum    = <ls_t303>-lgnum
                                       iv_matid    = iv_matid
                                       iv_entitled = iv_entitled
                                       iv_lgtyp    = ls_bintyp_base-lgtyp
                                       is_bin_data = ls_bin_data ).

      "If product height > Bin type height, no single product fits in this bin type -> capacity = 0
      IF ls_mat-height IS NOT INITIAL AND ls_mat-height > ls_bin_data-max_height.
        lv_max_pc = 0.
      ELSE.

        "Try 2 rotations
        "Rotation 1
        "INT(box width / bin width) * INT(box length / bin length).
        TRY.
            lv_numb_cart1 = floor( ls_bin_data-max_width / ls_mat-width ) *
                            floor( ls_bin_data-max_length / ls_mat-length ).
          CATCH cx_sy_zerodivide ##NO_HANDLER.
        ENDTRY.

        "Rotation 2
        "INT(box length / bin width) * INT(box width / bin length
        TRY.
            lv_numb_cart2 = floor( ls_bin_data-max_width / ls_mat-length ) *
                            floor( ls_bin_data-max_length / ls_mat-width ).
          CATCH cx_sy_zerodivide ##NO_HANDLER.
        ENDTRY.

        IF lv_numb_cart1 > lv_numb_cart2.
          lv_max_cart = lv_numb_cart1.
        ELSE.
          lv_max_cart = lv_numb_cart2.
        ENDIF.

        lv_max_pc = ls_mat-quan * lv_max_cart.

      ENDIF.

      rt_maxqty = VALUE #( BASE rt_maxqty ( lgnum   = <ls_t303>-lgnum
                                            lgtyp   = ls_bintyp_base-lgtyp
                                            lptyp   = <ls_t303>-lptyp
                                            matid   = iv_matid
                                            max_qty = lv_max_pc
                                            uom     = ls_mat-meins
                                                      ) ).
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.
********************************************************************
*& Key          : BSUGAREV-Jan 17, 2024
*& Request No.  : GAP-39 FS Bin capacity method for KDL bins
********************************************************************
*& Description  :
*&
********************************************************************
    CALL FUNCTION '/SCWM/T340D_READ_SINGLE'
      EXPORTING
        iv_lgnum  = iv_lgnum
      IMPORTING
        es_t340d  = ms_wme_default
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_core_exception( textid = VALUE #( msgid = sy-msgid
                                                                msgno = sy-msgno
                                                                attr1 = sy-msgv1
                                                                attr2 = sy-msgv1
                                                                attr3 = sy-msgv1
                                                                attr4 = sy-msgv1 ) ).
    ENDIF.

    mv_lgnum = iv_lgnum.

  ENDMETHOD.


  METHOD prepare_mat_data.
**********************************************************************
*& Key           : RM-230428
*& Request No.   : GAP-39 FS Bin capacity method for KDL bins
**********************************************************************
*& Description (short)
*&
**********************************************************************
    DATA:
      lt_mat_lgtyp  TYPE  /scwm/tt_material_lgtyp,
      lt_mat_global TYPE /scwm/tt_material_global,
      lt_mat_uom    TYPE /scwm/tt_material_uom.

    DATA(ls_mat) = VALUE ms_material_flowrack( matid = iv_matid ).

    DATA(lo_conv) = /scmb/cl_md_access_mdl=>get_md_access( ).

    TRY.
        CALL FUNCTION '/SCWM/MATERIAL_READ_MULTIPLE'
          EXPORTING
            iv_lgnum      = iv_lgnum
            it_matid      = VALUE /scwm/tt_matid( ( ls_mat-matid ) )
            iv_entitled   = iv_entitled
            it_lgtyp      = VALUE /scwm/tt_lgtyp( ( iv_lgtyp ) )
          IMPORTING
            et_mat_global = lt_mat_global
            et_mat_uom    = lt_mat_uom
            et_mat_lgtyp  = lt_mat_lgtyp.

      CATCH /scwm/cx_md ##NO_HANDLER.
    ENDTRY.

    " How much PCs can be fitted in 1 cartoon box
    ls_mat-meins = VALUE #( lt_mat_global[ matid = ls_mat-matid ]-meins OPTIONAL ). " Base UoM

    "Always to use the smallest available Z* UOM
    DELETE lt_mat_uom WHERE meinh = ls_mat-meins.
    SORT lt_mat_uom BY umrez ASCENDING.

    DATA(ls_cartton_base) = VALUE #( lt_mat_uom[ 1 ] OPTIONAL ).

    DATA(ls_tote_dim) = read_pack_spec( iv_matid ).

    DATA(ls_mat_lgtyp) = VALUE #( lt_mat_lgtyp[ 1 ] OPTIONAL ).
    " Product is handled in totes (ZZDIRREPL = space)
    " get the tote dimensions from the packspec level with quantity classification ‘2’
    IF ls_mat_lgtyp-zz1_dirrpl_stt IS INITIAL.

      ls_mat-quan  = ls_tote_dim-total_quan.
      ls_mat-meins = ls_tote_dim-total_unit.

      " Update Units of measure: Length, Width and Height
      ls_mat-unit_lwh = ls_tote_dim-unit_max_lwh.
      ls_mat-length   = ls_tote_dim-max_length.
      ls_mat-width    = ls_tote_dim-max_width.
    ELSE.

      ls_mat-quan  = ls_cartton_base-umrez.

      " Update Units of measure: Length, Width and Height
      ls_mat-unit_lwh = ls_cartton_base-meabm.
      ls_mat-length   = ls_cartton_base-laeng.
      ls_mat-width    = ls_cartton_base-breit.
    ENDIF.

    "height - to check only the mastercarton height, because that class is only used for mastercarton storage
    ls_mat-height = ls_cartton_base-hoehe.

    IF is_bin_data-unit_lwh <> ls_mat-unit_lwh.

      "convert height
      TRY.
          ls_mat-height = lo_conv->prod_quan_conversion(
            iv_prodid   = VALUE #( )
            iv_uom_from = ls_mat-unit_lwh
            iv_uom_to   = is_bin_data-unit_lwh
            iv_quan     = CONV /scmb/md_qty( ls_mat-height ) ).
          ##NO_HANDLER
        CATCH /scmb/cx_md_access.
      ENDTRY.

      "convert width
      TRY.
          ls_mat-width = lo_conv->prod_quan_conversion(
            iv_prodid   = VALUE #( )
            iv_uom_from = ls_mat-unit_lwh
            iv_uom_to   = is_bin_data-unit_lwh
            iv_quan     = CONV /scmb/md_qty( ls_mat-width ) ).
          ##NO_HANDLER
        CATCH /scmb/cx_md_access.
      ENDTRY.

      "convert length
      TRY.
          ls_mat-length = lo_conv->prod_quan_conversion(
            iv_prodid   = VALUE #( )
            iv_uom_from = ls_mat-unit_lwh
            iv_uom_to   = is_bin_data-unit_lwh
            iv_quan     = CONV /scmb/md_qty( ls_mat-length ) ).
          ##NO_HANDLER
        CATCH /scmb/cx_md_access.
      ENDTRY.

      ls_mat-unit_lwh = is_bin_data-unit_lwh.
    ENDIF.

    rs_mat = ls_mat.

  ENDMETHOD.


  METHOD read_pack_spec.
********************************************************************
*& Key          : BSUGAREV-Jan 17, 2024
*& Request No.  : GAP-39 FS Bin capacity method for KDL bins
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA:
      lt_packspec_content TYPE /scwm/tt_packspec_nested,
      lt_ps_keys          TYPE /scwm/tt_ps_header_key.

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
        rs_packspec = VALUE #( lt_packspec_content[ 1 ]-levels[ quancla = zif_wme_c=>gs_quancla-totes ] ).
      CATCH cx_sy_itab_line_not_found ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
