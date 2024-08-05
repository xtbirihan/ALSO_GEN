CLASS zcl_cuboid_algorithm DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      tt_mat_cuboid_input TYPE STANDARD TABLE OF zstr_mat_cuboid_input WITH EMPTY KEY .
    TYPES:
      tt_material_cubalg TYPE STANDARD TABLE OF zstr_material_cubalg WITH EMPTY KEY .
    TYPES:
      BEGIN OF ty_prod_pos,
        length TYPE laeng,
        width  TYPE breit,
        height TYPE hoehe,
        unit   TYPE meins,
      END OF ty_prod_pos .
    TYPES:
      BEGIN OF ty_pack_result,
        matid            TYPE /scwm/de_matid,
        pc_max           TYPE i,
        pc_packed        TYPE i,
        unit             TYPE meins,
        volum            TYPE volum,
        unit_v           TYPE /scwm/de_vol_uom,
        prod_pos_fcuboid TYPE ty_prod_pos,
        prod_pos_scuboid TYPE ty_prod_pos,
      END OF ty_pack_result .
    TYPES:
      BEGIN OF ty_cuboid_alg_result,
        pmat_guid   TYPE  /scwm/de_matid,
        volum       TYPE volum,
        unit_v      TYPE /scwm/de_vol_uom,
        closed      TYPE boole_d,
        mc_flag     TYPE /scwm/s_quan-unit,
        mat_details TYPE STANDARD TABLE OF ty_pack_result WITH EMPTY KEY,
      END OF ty_cuboid_alg_result .
    TYPES:
      tt_cuboid_alg_result TYPE STANDARD TABLE OF ty_cuboid_alg_result WITH EMPTY KEY .
    TYPES:
      BEGIN OF ty_bintypes,
        lgnum	TYPE /scwm/lgnum,
        lgtyp	TYPE /scwm/lgtyp,
        lptyp	TYPE /scwm/lvs_lptyp,
        quan  TYPE /scwm/s_quan-quan,
        unit  TYPE /scwm/s_quan-unit,
      END OF ty_bintypes .
    TYPES:
      tt_bintyp TYPE STANDARD TABLE OF ty_bintypes WITH EMPTY KEY .

    METHODS calc_bin_type_capacity
      IMPORTING
        !iv_matid          TYPE /scwm/de_matid
        !it_bintypes       TYPE /scwm/tt_t303s
      RETURNING
        VALUE(rt_bintypes) TYPE zcl_crud_ztlptyp_maxqty=>tt_lptyp_maxqty .
    METHODS constructor
      IMPORTING
        !iv_lgnum TYPE /scwm/lgnum
      RAISING
        zcx_core_exception .
    METHODS pack_by_best_pmat
      IMPORTING
        !it_materials         TYPE tt_mat_cuboid_input
        !it_packmat           TYPE /scwm/tt_who_pmat
      RETURNING
        VALUE(rt_pack_result) TYPE tt_cuboid_alg_result .
    METHODS pack_by_custom_pmat
      IMPORTING
        !iv_entitled    TYPE /scwm/de_entitled
        !is_bp          TYPE zstr_ship_bp
        !it_materials   TYPE zcl_cuboid_algorithm=>tt_mat_cuboid_input
        !it_packmat     TYPE /scwm/tt_who_pmat
      EXPORTING
        !et_pack_result TYPE tt_cuboid_alg_result
        !et_bapiret     TYPE bapirettab .
protected section.

  methods PACK_STOCK
    importing
      !IT_MATERIALS type TT_MAT_CUBOID_INPUT
      !IT_PACKMAT type /SCWM/TT_WHO_PMAT
    returning
      value(RT_PACK_RESULT) type TT_CUBOID_ALG_RESULT .
  methods REPACK_IN_SMALLER_PMAT
    importing
      !IT_MATERIALS type TT_MAT_CUBOID_INPUT
      !IT_PACKMAT type /SCWM/TT_WHO_PMAT
      !IT_PACKED_STOCK type TT_CUBOID_ALG_RESULT
    returning
      value(RT_PACK_RESULT) type TT_CUBOID_ALG_RESULT .
private section.

  data MO_PMAT_FUNC type ref to ZCL_PACKMMAT_ALGO .
  data MO_PROD type ref to /SCWM/IF_AF_PRODUCT .
  data MS_WME_DEFAULT type /SCWM/S_T340D .
  data MT_BAPIRET type BAPIRETTAB .
  data MT_MATERIALS type TT_MATERIAL_CUBALG .
  data MT_MATERIALS_INIT type TT_MATERIAL_CUBALG .
  data MT_PACKMAT type /SCWM/TT_WHO_PMAT .
  data MV_LGNUM type /SCWM/LGNUM .
  data MV_MSG type STRING .

  methods CALC_MAX_PC
    importing
      !IV_ONE_CUBOID type BOOLE_D default ABAP_TRUE
      !IS_MATERIAL type ZSTR_MATERIAL_CUBALG
      !IS_PACKMAT type /SCWM/S_WHO_PMAT
    returning
      value(RS_PACK_RESULT) type TY_PACK_RESULT .
  methods CALC_PACKED_PC_VOLUME
    importing
      !IV_NUMB_PC type I
      !IV_VOLUM type VOLUM
    returning
      value(RV_RESULT) type VOLUM .
  methods CALC_PC_SPACE_ABOVE
    importing
      !IS_PACKMAT type /SCWM/S_WHO_PMAT
      !IS_MAT_POS type TY_PROD_POS
    returning
      value(RS_PACK_RESULT) type TY_PACK_RESULT .
  methods CALC_PC_SPACE_END
    importing
      !IS_PACKMAT type /SCWM/S_WHO_PMAT
      !IS_MAT_POS type TY_PROD_POS
    returning
      value(RS_PACK_RESULT) type TY_PACK_RESULT .
  methods CALC_PC_SPACE_SIDE
    importing
      !IS_PACKMAT type /SCWM/S_WHO_PMAT
      !IS_MAT_POS type TY_PROD_POS
    returning
      value(RS_PACK_RESULT) type TY_PACK_RESULT .
  methods CONVERT_UNIT_QUANTITY_SINGLE
    importing
      !IV_PRODUCTID type /SCDL/DL_PRODUCTID optional
      !IV_UOM_FROM type /SCDL/DL_UOM
      !IV_UOM_TO type /SCDL/DL_UOM
      !IV_QTY_FROM type /SCDL/DL_QTY
    returning
      value(RV_QTY_TO) type /SCDL/DL_QTY .
  methods EXIT_PACKING
    returning
      value(RV_RESULT) type BOOLE_D .
  methods IS_COMBINE_PACK_POSSIBLE
    importing
      !IT_MAT_PACKED type TT_MATERIAL_CUBALG
      !IS_MAT_NEW type ZSTR_MATERIAL_CUBALG
      !IS_PMAT type /SCWM/S_WHO_PMAT
    returning
      value(RV_RESULT) type BOOLE_D .
  methods PACK_SPECIAL_MATERIALS
    importing
      !IT_MATERIALS type ZCL_CUBOID_ALGORITHM=>TT_MAT_CUBOID_INPUT
      !IT_PACKMATS type /SCWM/TT_WHO_PMAT
    returning
      value(ET_PACK_RESULT) type TT_CUBOID_ALG_RESULT .
  methods PREPARE_MATERIALS_DATA
    importing
      !IT_MATERIALS type TT_MAT_CUBOID_INPUT
      !IT_PACKMAT type /SCWM/TT_WHO_PMAT .
  methods REPACK_CANDISP_MATERIALS
    importing
      !IV_ENTITLED type /SCWM/DE_ENTITLED
      !IT_MATERIALS type TT_MAT_CUBOID_INPUT
      !IT_PACKMAT type /SCWM/TT_WHO_PMAT
    returning
      value(RT_PACK_RESULT) type TT_CUBOID_ALG_RESULT .
  methods SORT_SET_MATERIALS_FOR_PACK
    importing
      !IT_MATERIALS type TT_MATERIAL_CUBALG .
ENDCLASS.



CLASS ZCL_CUBOID_ALGORITHM IMPLEMENTATION.


  METHOD calc_bin_type_capacity.
********************************************************************
*& Key          : BSUGAREV-04.05.2023 15:45:56
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  : Calculate maximum number of pieces that can be
*&    stored in a bin type. Dimensions of the bin type and material
*&    are determined from their master data
*&
********************************************************************
    BREAK-POINT ID zcg_cuboid_algorithm.

    DATA(ls_bintyp_base) = VALUE #( it_bintypes[ 1 ] OPTIONAL ).

    DATA(ls_selopt_bintyp) = VALUE rseloption( FOR <bt> IN it_bintypes
                               WHERE ( lptyp IS NOT INITIAL )
                               ( sign   = wmegc_sign_inclusive
                                 option = wmegc_option_eq
                                 low    = <bt>-lptyp ) ).

    IF lines( ls_selopt_bintyp ) = 0.
      RETURN.
    ENDIF.

    " determine bin types characteristics L/W/H
    " make a direct select because there is no appropriate FM
    SELECT * FROM /scwm/t303
      INTO TABLE @DATA(lt_t303)
     WHERE lgnum = @mv_lgnum
       AND lptyp IN @ls_selopt_bintyp.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT lt_t303 ASSIGNING FIELD-SYMBOL(<ls_btyp>).

*      DATA(ls_mat) = VALUE zstr_mat_cuboid_input( matid = iv_matid ).

      DATA(ls_pmat) = VALUE /scwm/s_who_pmat(
                          max_length = <ls_btyp>-max_length
                          max_width  = <ls_btyp>-max_width
                          max_height = <ls_btyp>-max_height
                          unit_lwh   = <ls_btyp>-unit_lwh ).

      prepare_materials_data(
          it_materials = VALUE #( ( matid = iv_matid ) )
          it_packmat   = VALUE #( ( ls_pmat ) ) ).

      DATA(ls_material) = VALUE #( mt_materials[ 1 ] OPTIONAL ).

      DATA(ls_packmat) = VALUE #( mt_packmat[ 1 ] OPTIONAL ).

      DATA(ls_max_qty) = calc_max_pc( iv_one_cuboid = abap_false
                                      is_material = ls_material
                                      is_packmat  = ls_packmat ).

      CHECK ls_max_qty-pc_max IS NOT INITIAL.

      rt_bintypes = VALUE #( BASE rt_bintypes ( lgnum = <ls_btyp>-lgnum
                                                lgtyp = ls_bintyp_base-lgtyp
                                                lptyp = <ls_btyp>-lptyp
                                                matid = iv_matid
                                                max_qty = ls_max_qty-pc_max
                                                uom = ls_max_qty-unit ) ).
    ENDLOOP.

  ENDMETHOD.


  METHOD calc_max_pc.
    DATA: lv_numb_pc  TYPE i,
          ls_prod_pos TYPE ty_prod_pos.

    DATA(lv_counter) = 0.

    " validate material dimensions are maintained
    IF is_material-length = 0 OR is_material-width = 0 OR is_material-height = 0.
      RETURN.
    ENDIF.

    DO 6 TIMES.
      lv_counter += 1.

      CASE lv_counter.
        WHEN 1.
          " int (lc/lp) * int (wc/wp) * int (hc/hp)
          lv_numb_pc = floor( is_packmat-max_length / is_material-length ) *
                       floor( is_packmat-max_width  / is_material-width ) *
                       floor( is_packmat-max_height / is_material-height ).

          ls_prod_pos-length = is_material-length.
          ls_prod_pos-width  = is_material-width.
          ls_prod_pos-height = is_material-height.

          CHECK lv_numb_pc > 0.

          rs_pack_result-pc_max = lv_numb_pc.
*          rs_pack_result-volum  = calc_packed_pc_volume( iv_numb_pc = lv_numb_pc iv_volum = is_material-volum ).
          rs_pack_result-prod_pos_fcuboid-length = is_material-length.
          rs_pack_result-prod_pos_fcuboid-width  = is_material-width.
          rs_pack_result-prod_pos_fcuboid-height = is_material-height.

        WHEN 2.
          " int (lc/lp) * int (wc/hp) * int (hc/wp)
          lv_numb_pc = floor( is_packmat-max_length / is_material-length ) *
                       floor( is_packmat-max_width  / is_material-height ) *
                       floor( is_packmat-max_height / is_material-width ).

          ls_prod_pos-length = is_material-length.
          ls_prod_pos-width  = is_material-height.
          ls_prod_pos-height = is_material-width.

        WHEN 3.
          " int (lc/hp) * int (wc/wp) * int (hc/lp)
          lv_numb_pc = floor( is_packmat-max_length / is_material-height ) *
                       floor( is_packmat-max_width  / is_material-width ) *
                       floor( is_packmat-max_height / is_material-length ).

          ls_prod_pos-length = is_material-height.
          ls_prod_pos-width  = is_material-width.
          ls_prod_pos-height = is_material-length.

        WHEN 4.
          " int (lc/hp) * int (wc/lp) * int (hc/wp)
          lv_numb_pc = floor( is_packmat-max_length / is_material-height ) *
                       floor( is_packmat-max_width  / is_material-length ) *
                       floor( is_packmat-max_height / is_material-width ).

          ls_prod_pos-length = is_material-height.
          ls_prod_pos-width  = is_material-length.
          ls_prod_pos-height = is_material-width.

        WHEN 5.
          " int (lc/wp) * int (wc/lp) * int (hc/hp)
          lv_numb_pc = floor( is_packmat-max_length / is_material-width ) *
                       floor( is_packmat-max_width  / is_material-length ) *
                       floor( is_packmat-max_height / is_material-height ).

          ls_prod_pos-length = is_material-width.
          ls_prod_pos-width  = is_material-length.
          ls_prod_pos-height = is_material-height.

        WHEN 6.
          " int (lc/wp) * int (wc/hp) * int (hc/lp)
          lv_numb_pc = floor( is_packmat-max_length / is_material-width ) *
                       floor( is_packmat-max_width  / is_material-height ) *
                       floor( is_packmat-max_height / is_material-length ).

          ls_prod_pos-length = is_material-width.
          ls_prod_pos-width  = is_material-height.
          ls_prod_pos-height = is_material-length.

      ENDCASE.

      " 1. check if calculation for only one cuboid is requested
      " OBSOLETE 2. PC to be packed are less then calculated MAX possible PC
      " If any of these two conditions is true -> free space and No. of PC in that space is not calculated
      IF iv_one_cuboid = abap_true. ""OR is_material-quan < lv_numb_pc.

        IF rs_pack_result-pc_max < lv_numb_pc.
          CLEAR: rs_pack_result.
          rs_pack_result-pc_max = lv_numb_pc.
*          rs_pack_result-volum  = calc_packed_pc_volume( iv_numb_pc = lv_numb_pc iv_volum = is_material-volum ).
          rs_pack_result-prod_pos_fcuboid = ls_prod_pos.
        ENDIF.

        CONTINUE.
      ENDIF.

      DATA(ls_best_scuboid) = VALUE ty_pack_result( ).

      " calculate remaining space in the box and PC in that space
      " Above the box: lc, wc, hc – int(hc/hp) * hp
      ls_best_scuboid = calc_pc_space_above(
        is_packmat = is_packmat
        is_mat_pos = ls_prod_pos ).

      " Side of the box: lc, wc – int(wc/wp) * wp, hc
      DATA(ls_pc_side) = calc_pc_space_side(
        is_packmat = is_packmat
        is_mat_pos = ls_prod_pos ).

      IF ls_pc_side-pc_max > ls_best_scuboid-pc_max.
        ls_best_scuboid = ls_pc_side.
      ENDIF.

      " End of the box: lc – int(lc/lp) * lp, wc, hc
      DATA(ls_pc_end) = calc_pc_space_end(
        is_packmat = is_packmat
        is_mat_pos = ls_prod_pos ).

      IF ls_pc_end-pc_max > ls_best_scuboid-pc_max.
        ls_best_scuboid = ls_pc_end.
      ENDIF.

      lv_numb_pc += ls_best_scuboid-pc_max.

      IF rs_pack_result-pc_max < lv_numb_pc.
        rs_pack_result-pc_max = lv_numb_pc.
*        rs_pack_result-volum  = calc_packed_pc_volume( iv_numb_pc = lv_numb_pc iv_volum = is_material-volum ).
        rs_pack_result-prod_pos_fcuboid = ls_prod_pos.
        rs_pack_result-prod_pos_scuboid = ls_best_scuboid-prod_pos_fcuboid.
        rs_pack_result-prod_pos_scuboid-unit = is_material-unit_lwh.
      ENDIF.

    ENDDO.

    IF rs_pack_result-pc_max > 0.
      rs_pack_result-matid = is_material-matid.
      rs_pack_result-unit = is_material-meins.
      rs_pack_result-unit_v = is_material-unit_v.
      rs_pack_result-prod_pos_fcuboid-unit = is_material-unit_lwh.
    ENDIF.

  ENDMETHOD.


  METHOD calc_packed_pc_volume.
    rv_result = iv_numb_pc * iv_volum.
  ENDMETHOD.


  METHOD calc_pc_space_above.
    " lc, wc, hc – int(hc/hp) * hp

    DATA(lv_height_free_space) = CONV i(
      is_packmat-max_height - floor( is_packmat-max_height / is_mat_pos-height ) * is_mat_pos-height ).

    IF lv_height_free_space < is_mat_pos-height AND
       lv_height_free_space < is_mat_pos-length AND
       lv_height_free_space < is_mat_pos-width.
      RETURN.
    ENDIF.

    DATA(ls_packmat) = is_packmat.

    ls_packmat-max_height = lv_height_free_space.

    rs_pack_result = calc_max_pc(
        is_material = VALUE #( length = is_mat_pos-length
                               width  = is_mat_pos-width
                               height = is_mat_pos-height )
        is_packmat  = ls_packmat ).

  ENDMETHOD.


  METHOD calc_pc_space_end.
    " End of the box: lc – int(lc/lp) * lp, wc, hc

    DATA(lv_lenght_free_space) = CONV i(
      is_packmat-max_length - floor( is_packmat-max_length / is_mat_pos-length ) * is_mat_pos-length ).

    IF lv_lenght_free_space < is_mat_pos-height AND
       lv_lenght_free_space < is_mat_pos-length AND
       lv_lenght_free_space < is_mat_pos-width.
      RETURN.
    ENDIF.

    DATA(ls_packmat) = is_packmat.

    ls_packmat-max_length = lv_lenght_free_space.

    rs_pack_result = calc_max_pc(
        is_material = VALUE #( length = is_mat_pos-length
                               width  = is_mat_pos-width
                               height = is_mat_pos-height )
        is_packmat  = ls_packmat ).
  ENDMETHOD.


  METHOD calc_pc_space_side.
    " Side of the box: lc, wc – int(wc/wp) * wp, hc

    DATA(lv_width_free_space) = CONV i(
      is_packmat-max_width - floor( is_packmat-max_width / is_mat_pos-width ) * is_mat_pos-width ).

    IF lv_width_free_space < is_mat_pos-height AND
       lv_width_free_space < is_mat_pos-length AND
       lv_width_free_space < is_mat_pos-width.
      RETURN.
    ENDIF.

    DATA(ls_packmat) = is_packmat.

    ls_packmat-max_width = lv_width_free_space.

    rs_pack_result = calc_max_pc(
        is_material = VALUE #( length = is_mat_pos-length
                               width  = is_mat_pos-width
                               height = is_mat_pos-height )
        is_packmat  = ls_packmat ).
  ENDMETHOD.


  METHOD constructor.
********************************************************************
*& Key          : <BSUGAREV>-15.03.2023 10:44:56
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description : Algorithm is used in different processes, each public
*&   method is indended to handle different scenario
********************************************************************
    BREAK-POINT ID zcg_cuboid_algorithm.

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

    " get instance of producet class
    mo_prod = CAST #( /scdl/cl_af_management=>get_instance( )->get_service( /scwm/if_af_product=>sc_me_as_service ) ).
    mo_pmat_func = NEW #( iv_lgnum ).
  ENDMETHOD.


  METHOD convert_unit_quantity_single.
********************************************************************
*& Key          : BSUGAREV-Jan 17, 2024
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA(lo_conv) = /scmb/cl_md_access_mdl=>get_md_access( ).

    IF iv_productid IS NOT INITIAL.
      TRY.
          rv_qty_to = lo_conv->prod_quan_conversion(
              iv_prodid    = iv_productid
              iv_uom_from  = iv_uom_from
              iv_uom_to    = iv_uom_to
              iv_quan      = iv_qty_from ).
          ##NO_HANDLER
        CATCH /scmb/cx_md_access.
      ENDTRY.

    ELSE.

      CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
        EXPORTING
          input                = iv_qty_from
          unit_in              = iv_uom_from
          unit_out             = iv_uom_to
        IMPORTING
          output               = rv_qty_to
        EXCEPTIONS
          conversion_not_found = 1                " Conversion factor could not be determined
          division_by_zero     = 2                " Division by zero caught
          input_invalid        = 3                " Input value is not a number
          output_invalid       = 4                " OUTPUT parameter is not a number
          overflow             = 5                " Field overflow
          type_invalid         = 6                " an output parameter is not a number
          units_missing        = 7                " no units specified
          unit_in_not_found    = 8                " UNIT_IN is not maintained
          unit_out_not_found   = 9                " UNIT_OUT is not maintained
          OTHERS               = 10.
      IF sy-subrc <> 0.
        CLEAR: rv_qty_to.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD exit_packing.
********************************************************************
*& Key          : BSUGAREV-Jan 17, 2024
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  :
*&
********************************************************************
    rv_result = abap_true.

    DELETE mt_materials WHERE quan = 0.

    IF lines( mt_materials ) > 0.
      rv_result = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD is_combine_pack_possible.

    LOOP AT it_mat_packed ASSIGNING FIELD-SYMBOL(<ls_mat_packed>).

      DATA(lv_max_length) = <ls_mat_packed>-length.
      IF <ls_mat_packed>-length < is_mat_new-length.
        lv_max_length = is_mat_new-length.
      ENDIF.

      DATA(lv_max_width) = <ls_mat_packed>-width.
      IF <ls_mat_packed>-width < is_mat_new-width.
        lv_max_width = is_mat_new-width.
      ENDIF.

      " variant 1: max(l1,l2) max(w1,w2)  h1+h2
      IF is_pmat-max_length >= lv_max_length AND
         is_pmat-max_width  >= lv_max_width  AND
         is_pmat-max_height >= ( <ls_mat_packed>-height + is_mat_new-height ).

        rv_result = abap_true.
        CONTINUE.
      ENDIF.

      " variant 2: max(l1,l2) max(h1,w2)  w1+h2
      lv_max_width = <ls_mat_packed>-height.
      IF lv_max_width < is_mat_new-width.
        lv_max_width = is_mat_new-width.
      ENDIF.

      IF is_pmat-max_length >= lv_max_length AND
         is_pmat-max_width  >= lv_max_width  AND
         is_pmat-max_height >= ( <ls_mat_packed>-width + is_mat_new-height ).

        rv_result = abap_true.
        CONTINUE.
      ENDIF.

      " variant 3: max(w1,l2) max(h1,w2)  l1+h2
      lv_max_length = <ls_mat_packed>-width.
      IF lv_max_length < is_mat_new-length.
        lv_max_length = is_mat_new-length.
      ENDIF.

      lv_max_width = <ls_mat_packed>-height.
      IF lv_max_width < is_mat_new-width.
        lv_max_width = is_mat_new-width.
      ENDIF.

      IF is_pmat-max_length >= lv_max_length AND
         is_pmat-max_width  >= lv_max_width  AND
         is_pmat-max_height >= ( <ls_mat_packed>-length + is_mat_new-height ).

        rv_result = abap_true.
        CONTINUE.
      ENDIF.

      " new material does not match with one of already packed materials => combination is not possible
      rv_result = abap_false.
      EXIT.
    ENDLOOP.

  ENDMETHOD.


  METHOD pack_by_best_pmat.
********************************************************************
*& Key          : <BSUGAREV>-15.03.2023 10:44:56
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description - select best possible pack material for provided list
*&      of materials. Calculations are done based on custom cuboid
*&      algorithm
********************************************************************
    BREAK-POINT ID zcg_cuboid_algorithm.

    DATA(lt_packed_stock) = pack_stock( it_materials = it_materials
                                        it_packmat   = it_packmat ).

    rt_pack_result = repack_in_smaller_pmat( it_materials    = it_materials
                                             it_packmat      = it_packmat
                                             it_packed_stock = lt_packed_stock ).
  ENDMETHOD.


  METHOD pack_by_custom_pmat.
********************************************************************
*& Key          : BSUGAREV-Jan 17, 2024
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA: lt_packmat_ship_for_alg TYPE /scwm/tt_who_pmat,
          lt_packmat_spec_for_alg TYPE /scwm/tt_who_pmat.

    CLEAR: et_pack_result.

    " build selection option with materials from customizing and determine their MATIDs
    DATA(lt_spec_mats_from_cust) = zcl_crud_ztout_spmat_pmat=>select_multi_by_lgnum( mv_lgnum ).
    DATA(lt_matnr) = VALUE /scmb/mdl_matnr_tab( FOR <sp> IN lt_spec_mats_from_cust ( matnr = <sp>-pmat ) ).
    lt_matnr = VALUE #( BASE lt_matnr FOR <sp> IN lt_spec_mats_from_cust ( matnr = <sp>-matnr ) ).

    TRY.
        mo_prod->convert_matnr_to_matid_multi(
          EXPORTING
            it_matnr     = lt_matnr
          IMPORTING
            et_matid     = DATA(lt_mats_pmats_special) ).
        ##NO_HANDLER
      CATCH /scwm/cx_md_api_faulty_call.
        ##NO_HANDLER
      CATCH /scwm/cx_md_exception.
    ENDTRY.

    DATA(lt_material_spec) = VALUE zcl_cuboid_algorithm=>tt_mat_cuboid_input(
        FOR <smc> IN lt_spec_mats_from_cust
        FOR <am>  IN lt_mats_pmats_special WHERE ( matnr = <smc>-matnr )
        FOR <im>  IN it_materials WHERE ( matid = <am>-matid ) ( <im> ) ).

    SORT lt_material_spec BY matid.
    DELETE ADJACENT DUPLICATES FROM lt_material_spec COMPARING ALL FIELDS.

    DATA(lt_mat_special_selopt) = VALUE rseloption(
        FOR <spmso> IN lt_material_spec ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = <spmso>-matid ) ).

    " add an extra line with empty value, this is needed in case lt_material_spec = 0 in order to work properly
    lt_mat_special_selopt = VALUE #( BASE lt_mat_special_selopt ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = space ) ).

    DATA(lt_pmat_special_selopt) = VALUE rseloption(
        FOR <smc> IN lt_spec_mats_from_cust
        FOR <am>  IN lt_mats_pmats_special WHERE ( matnr = <smc>-pmat ) ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = <am>-matid ) ).

    SORT lt_pmat_special_selopt BY low.
    DELETE ADJACENT DUPLICATES FROM lt_pmat_special_selopt COMPARING ALL FIELDS.

    DATA(lt_material_candisp) = VALUE zcl_cuboid_algorithm=>tt_mat_cuboid_input(
        FOR <inm> IN it_materials WHERE ( matid NOT IN lt_mat_special_selopt ) ( <inm> ) ).

    TRY.
        mo_prod->convert_matid_to_matnr_multi(
          EXPORTING
            it_matid     = VALUE #( FOR <ipid> IN it_packmat ( <ipid>-pmat_guid ) )
          IMPORTING
            et_matnr     = DATA(lt_pmat_input) ).
        ##NO_HANDLER
      CATCH /scwm/cx_md_api_faulty_call.
        ##NO_HANDLER
      CATCH /scwm/cx_md_exception.
    ENDTRY.

    DATA(lt_pmat_input_filter) = mo_pmat_func->filter_pmats_by_bp(
        is_bp    = is_bp
        it_pmats = VALUE #( FOR <l> IN lt_pmat_input ( matid = <l>-matid matnr = <l>-matnr ) ) ).

    IF lines( lt_pmat_special_selopt ) > 0.
      DELETE lt_pmat_input_filter WHERE pmatid IN lt_pmat_special_selopt.
    ENDIF.

    IF lines( lt_material_candisp ) > 0.
      lt_packmat_ship_for_alg = VALUE #( FOR <pma> IN lt_pmat_input_filter
                                         FOR <pminp> IN it_packmat WHERE ( pmat_guid = <pma>-pmatid )
                                         ( <pminp> ) ).

      mo_pmat_func->update_pmat_dimensions( CHANGING ct_packmat = lt_packmat_ship_for_alg ).

      DATA(lt_pack) = repack_candisp_materials( iv_entitled  = iv_entitled
                                                it_materials = lt_material_candisp
                                                it_packmat   = lt_packmat_ship_for_alg ).
      APPEND LINES OF lt_pack TO et_pack_result.
    ENDIF.

    IF lines( lt_material_spec ) > 0.
      lt_packmat_spec_for_alg = VALUE #( FOR <sppm> IN lt_pmat_special_selopt ( pmat_guid = <sppm>-low ) ).
      mo_pmat_func->update_pmat_dimensions( CHANGING ct_packmat = lt_packmat_spec_for_alg ).

      lt_pack = pack_special_materials( it_materials = lt_material_spec
                                        it_packmats  = lt_packmat_spec_for_alg ).

      APPEND LINES OF lt_pack TO et_pack_result.
    ENDIF.

    et_bapiret = mt_bapiret.

  ENDMETHOD.


  METHOD pack_special_materials.
********************************************************************
*& Key          : BSUGAREV-Jan 17, 2024
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  :
*&
*&
********************************************************************
    TYPES: BEGIN OF ty_mats_with_pmats,
             pmats TYPE /scwm/tt_matid.
             INCLUDE TYPE zstr_mat_cuboid_input.
    TYPES: END OF ty_mats_with_pmats.

    DATA: lt_mats_with_pmats TYPE STANDARD TABLE OF ty_mats_with_pmats WITH EMPTY KEY.

    IF lines( it_materials ) = 0.
      RETURN.
    ENDIF.

    DATA(lt_spec_mats_from_cust) = zcl_crud_ztout_spmat_pmat=>select_multi_by_lgnum( mv_lgnum ).

    " build selection option with materials from customizing and determine their MATIDs
    DATA(lt_matnr) = VALUE /scmb/mdl_matnr_tab( FOR <sp> IN lt_spec_mats_from_cust ( matnr = <sp>-pmat ) ).
    lt_matnr = VALUE #( BASE lt_matnr FOR <sp> IN lt_spec_mats_from_cust ( matnr = <sp>-matnr ) ).

    TRY.
        mo_prod->convert_matnr_to_matid_multi(
          EXPORTING
            it_matnr     = lt_matnr
          IMPORTING
            et_matid     = DATA(lt_matid_matnr) ).
        ##NO_HANDLER
      CATCH /scwm/cx_md_api_faulty_call.
        ##NO_HANDLER
      CATCH /scwm/cx_md_exception.
    ENDTRY.

    " build a mapping table between matid and packmatIDs
    LOOP AT it_materials ASSIGNING FIELD-SYMBOL(<ls_mat>).
      APPEND INITIAL LINE TO lt_mats_with_pmats ASSIGNING FIELD-SYMBOL(<ls_mat_pmats>).

      MOVE-CORRESPONDING <ls_mat> TO <ls_mat_pmats>.

      DATA(lv_matnr) = VALUE #( lt_matid_matnr[ matid = <ls_mat>-matid ]-matnr ).

      <ls_mat_pmats>-pmats = VALUE #( FOR <l>   IN lt_spec_mats_from_cust WHERE ( matnr = lv_matnr )
                                      FOR <lmm> IN lt_matid_matnr         WHERE ( matnr = <l>-pmat )
                                           ( <lmm>-matid ) ).

      SORT <ls_mat_pmats>-pmats.
      DELETE ADJACENT DUPLICATES FROM <ls_mat_pmats>-pmats.
    ENDLOOP.

    DATA(lt_materials) = it_materials.

    LOOP AT lt_materials ASSIGNING <ls_mat>.

      DATA(lt_mats_for_algo) = VALUE tt_mat_cuboid_input( ( <ls_mat> ) ).

      DATA(lt_current_mat_pmats) = VALUE #( lt_mats_with_pmats[ matid = <ls_mat>-matid ]-pmats OPTIONAL ).
      DELETE lt_mats_with_pmats WHERE matid = <ls_mat>-matid.

      SORT lt_current_mat_pmats ASCENDING.

      LOOP AT lt_mats_with_pmats ASSIGNING <ls_mat_pmats>.
        SORT <ls_mat_pmats>-pmats ASCENDING.

        CHECK lt_current_mat_pmats = <ls_mat_pmats>-pmats.

        " delete this material, it is added in the current list of materials for packing
        DELETE lt_materials WHERE matid = <ls_mat_pmats>-matid.

        lt_mats_for_algo = VALUE #( BASE lt_mats_for_algo ( CORRESPONDING #( <ls_mat_pmats> ) ) ).

        DELETE lt_mats_with_pmats.
      ENDLOOP.

      TRY.
          DATA(lt_pack_spec) = NEW zcl_cuboid_algorithm( mv_lgnum
              )->pack_by_best_pmat( it_materials = lt_mats_for_algo
                                    it_packmat   = VALUE #( FOR <lpm> IN lt_current_mat_pmats
                                                            FOR <apm> IN it_packmats WHERE ( pmat_guid = <lpm> )
                                                            ( <apm> ) ) ).
          ##NO_HANDLER
        CATCH zcx_core_exception.
      ENDTRY.

      APPEND LINES OF lt_pack_spec TO et_pack_result.
    ENDLOOP.
  ENDMETHOD.


  METHOD pack_stock.
    DATA: lv_packed_pc_total_volum TYPE volum.

    " read materials GLOBAL data to check the main UoM. Do conversion if provided and main UoM are different
    prepare_materials_data( it_materials = it_materials
                            it_packmat   = it_packmat ).
    ##NEEDED
    DATA(lv_counter) = 0.

    " add as many pack materials in RT_PACK_RESULT as needed until all provided quantities are packed
    " once the first pack is completed and no space is left, the packing steps are restarted
    " biggest packaging material is always used -> at the end we try to repack in smaller pack material
    DO.
      IF exit_packing( ) = abap_true.
        EXIT.
      ENDIF.

      lv_counter += 1.

      DATA(lv_closed_for_pack) = abap_false.

      DATA(ls_mat_first) = VALUE #( mt_materials[ 1 ] OPTIONAL ).

      DATA(ls_packmat) = VALUE #( mt_packmat[ 1 ] OPTIONAL ).

      " Product 1: calculate max number of pieces
      DATA(ls_pack_result) = calc_max_pc( iv_one_cuboid = abap_false
                                          is_material   = ls_mat_first
                                          is_packmat    = ls_packmat ).

      IF ls_pack_result IS INITIAL.
        " material cannot be packed using supplied pack material
        RETURN.
      ENDIF.

      IF ls_mat_first-quan < ls_pack_result-pc_max.

        ls_pack_result-pc_packed = ls_mat_first-quan.
        ls_pack_result-volum = calc_packed_pc_volume( iv_numb_pc = CONV #( ls_mat_first-quan ) iv_volum = ls_mat_first-volum ).
        ls_pack_result-unit_v = ls_mat_first-unit_v.

        rt_pack_result = VALUE #( BASE rt_pack_result
            ( pmat_guid = ls_packmat-pmat_guid
              volum     = ls_packmat-max_volume
              unit_v    = ls_packmat-unit_v
              mat_details = VALUE #( ( ls_pack_result ) ) ) ).
      ELSE.

        WHILE ls_mat_first-quan > 0.

          ls_pack_result-pc_packed = ls_pack_result-pc_max.
          lv_closed_for_pack = abap_true.

          IF ls_mat_first-quan < ls_pack_result-pc_max.
            ls_pack_result-pc_packed = ls_mat_first-quan.
            lv_closed_for_pack = abap_false.
          ENDIF.

          ls_pack_result-volum = calc_packed_pc_volume( iv_numb_pc = ls_pack_result-pc_packed iv_volum = ls_mat_first-volum ).

          rt_pack_result = VALUE #( BASE rt_pack_result
              ( pmat_guid = ls_packmat-pmat_guid
                volum     = ls_packmat-max_volume
                unit_v    = ls_packmat-unit_v
                closed    = lv_closed_for_pack
                mat_details = VALUE #( ( ls_pack_result ) ) ) ).

          ls_mat_first-quan -= ls_pack_result-pc_max.

        ENDWHILE.

      ENDIF.

      " update quantity of the first material. At this point all PCs are packed
      DATA(ls_fmat_upd) = REF #( mt_materials[ matid = ls_mat_first-matid ] OPTIONAL ).
      IF ls_fmat_upd IS NOT INITIAL.
        ls_fmat_upd->quan = 0. ""ls_mat_first-quan.
      ENDIF.

      lv_closed_for_pack = abap_false.
      DATA(lv_mats_count) = lines( mt_materials ).
      " Start interating from second material in the list, first is already "packed"
      LOOP AT mt_materials ASSIGNING FIELD-SYMBOL(<ls_mat>) FROM 2.

        " we are interested from the last entry. If thre is entry with closed = false in rt_pack_result
        " be updated to true, no matter if last material is packed or not
        IF lv_mats_count = sy-tabix.
          lv_closed_for_pack = abap_true.
        ENDIF.

        " continue with bulkiness only if packing is not closed
        READ TABLE rt_pack_result ASSIGNING FIELD-SYMBOL(<ls_mat_packed>)
              WITH KEY pmat_guid = ls_packmat-pmat_guid closed = abap_false.
        IF <ls_mat_packed> IS NOT ASSIGNED.
          EXIT.
        ENDIF.

        <ls_mat_packed>-closed = lv_closed_for_pack.

        DATA(lt_mat_packed) = VALUE tt_material_cubalg( ).
        LOOP AT <ls_mat_packed>-mat_details ASSIGNING FIELD-SYMBOL(<ls_matd>).
          lt_mat_packed = VALUE #( BASE lt_mat_packed
                                   FOR <l> IN mt_materials_init
                                   WHERE ( matid = <ls_matd>-matid ) ( <l> ) ).
        ENDLOOP.

        CHECK is_combine_pack_possible( it_mat_packed = lt_mat_packed
                                        is_mat_new = <ls_mat>
                                        is_pmat = ls_packmat ) = abap_true.


        CLEAR: lv_packed_pc_total_volum.

        LOOP AT <ls_mat_packed>-mat_details ASSIGNING FIELD-SYMBOL(<ls_mdet>).
          lv_packed_pc_total_volum += <ls_mdet>-volum.
        ENDLOOP.

        DATA(lv_left_vol_capacity) = CONV volum( <ls_mat_packed>-volum - lv_packed_pc_total_volum ).

        DATA(lv_possible_pc_to_pack) = CONV i( floor( lv_left_vol_capacity / <ls_mat>-volum ) ).

        " if no capacity is left in the current pack material, restart the process
        CHECK lv_possible_pc_to_pack > 0.

        IF <ls_mat>-quan <= lv_possible_pc_to_pack.

          <ls_mat_packed>-mat_details = VALUE #( BASE <ls_mat_packed>-mat_details
            ( matid  = <ls_mat>-matid
              pc_packed = CONV #( <ls_mat>-quan )
              unit   = <ls_mat>-meins
              volum  = calc_packed_pc_volume( iv_numb_pc = CONV #( <ls_mat>-quan ) iv_volum = <ls_mat>-volum )
              unit_v = <ls_mat>-unit_v ) ).

          <ls_mat>-quan = 0.
        ELSE.

          <ls_mat_packed>-closed = abap_true.

          <ls_mat_packed>-mat_details = VALUE #( BASE <ls_mat_packed>-mat_details
            ( matid  = <ls_mat>-matid
              pc_packed = lv_possible_pc_to_pack
              unit   = <ls_mat>-meins
              volum  = calc_packed_pc_volume( iv_numb_pc = lv_possible_pc_to_pack iv_volum = <ls_mat>-volum )
              unit_v = <ls_mat>-unit_v ) ).

          <ls_mat>-quan -= lv_possible_pc_to_pack.

          EXIT.
        ENDIF.

      ENDLOOP.

    ENDDO.
  ENDMETHOD.


  METHOD prepare_materials_data.
    DATA: lt_matid      TYPE /scwm/tt_matid,
          lt_mats       TYPE tt_material_cubalg,
          lt_mat_global TYPE /scwm/tt_material_global,
          lt_mat_uom    TYPE /scwm/tt_material_uom.

    mt_packmat = it_packmat.

    IF lines( mt_packmat ) = 0.
      RETURN.
    ENDIF.

    TRY.
        mo_prod->read_material_multiple(
          EXPORTING
            it_matid           = VALUE #( FOR <l> IN mt_packmat ( <l>-pmat_guid ) )
            iv_notext          = abap_true
          IMPORTING
            et_mat_pack        = DATA(lt_packmat_packdata) ).
        ##NO_HANDLER
      CATCH /scwm/cx_md_api_faulty_call.
        ##NO_HANDLER
      CATCH /scwm/cx_md_exception.
    ENDTRY.

    " convert UoM for pack materials
    LOOP AT mt_packmat ASSIGNING FIELD-SYMBOL(<ls_pmat>). ""FROM 2.

      DATA(ls_packmat_packdata) = VALUE #( lt_packmat_packdata[ matid = <ls_pmat>-pmat_guid ] OPTIONAL ).

      " this is needed for bin type capacity calculation
      IF <ls_pmat>-unit_lwh IS INITIAL.
        IF ls_packmat_packdata-maxdim_uom IS INITIAL.
          <ls_pmat>-unit_lwh = ms_wme_default-dist_uom.
        ELSE.
          <ls_pmat>-unit_lwh = ls_packmat_packdata-maxdim_uom.
        ENDIF.
      ENDIF.

      IF <ls_pmat>-unit_v IS INITIAL.
        IF ls_packmat_packdata-maxv_uom IS INITIAL.
          <ls_pmat>-unit_v = ms_wme_default-voleh.
        ELSE.
          <ls_pmat>-unit_v = ls_packmat_packdata-maxv_uom.
        ENDIF.
      ENDIF.



      IF <ls_pmat>-max_length IS INITIAL.
        <ls_pmat>-max_length = ls_packmat_packdata-maxl.
      ENDIF.

      IF <ls_pmat>-max_width IS INITIAL.
        <ls_pmat>-max_width = ls_packmat_packdata-maxb.
      ENDIF.

      IF <ls_pmat>-max_height IS INITIAL.
        <ls_pmat>-max_height = ls_packmat_packdata-maxh.
      ENDIF.

      IF <ls_pmat>-max_volume IS INITIAL.
        <ls_pmat>-max_volume = ls_packmat_packdata-maxv.
      ENDIF.

      " convert distance variables to CM
      IF zif_wme_c=>gs_uom-cm <> <ls_pmat>-unit_lwh.

        <ls_pmat>-max_length = convert_unit_quantity_single( ""iv_productid = <ls_pmat>-pmat_guid
                                                             iv_uom_from  = <ls_pmat>-unit_lwh
                                                             iv_uom_to    = zif_wme_c=>gs_uom-cm
                                                             iv_qty_from  = CONV #( <ls_pmat>-max_length ) ).

        <ls_pmat>-max_width = convert_unit_quantity_single( ""iv_productid = <ls_pmat>-pmat_guid
                                                            iv_uom_from  = <ls_pmat>-unit_lwh
                                                            iv_uom_to    = zif_wme_c=>gs_uom-cm
                                                            iv_qty_from  = CONV #( <ls_pmat>-max_width ) ).

        <ls_pmat>-max_height = convert_unit_quantity_single( ""iv_productid = <ls_pmat>-pmat_guid
                                                             iv_uom_from  = <ls_pmat>-unit_lwh
                                                             iv_uom_to    = zif_wme_c=>gs_uom-cm
                                                             iv_qty_from  = CONV #( <ls_pmat>-max_height ) ).

        <ls_pmat>-unit_lwh = zif_wme_c=>gs_uom-cm.
      ENDIF.

      " convert VOLUME to DM3
      IF zif_wme_c=>gs_uom-cdm <> <ls_pmat>-unit_v.
        <ls_pmat>-max_volume = convert_unit_quantity_single( ""iv_productid = <ls_pmat>-pmat_guid
                                                             iv_uom_from  = <ls_pmat>-unit_v
                                                             iv_uom_to    = zif_wme_c=>gs_uom-cdm
                                                             iv_qty_from  = CONV #( <ls_pmat>-max_volume ) ).
        <ls_pmat>-unit_v = zif_wme_c=>gs_uom-cdm.
      ENDIF.

    ENDLOOP.

    SORT mt_packmat BY max_volume DESCENDING.

    DATA(ls_pmat_biggest) = mt_packmat[ 1 ].

    lt_matid = VALUE #( FOR <mi> IN it_materials ( <mi>-matid ) ).

    TRY.
        mo_prod->read_material_multiple(
          EXPORTING
            it_matid      = lt_matid
            iv_notext     = abap_true
          IMPORTING
            et_mat_global = lt_mat_global
            et_mat_uom    = lt_mat_uom ).
        ##NO_HANDLER
      CATCH /scwm/cx_md_api_faulty_call.
        ##NO_HANDLER
      CATCH /scwm/cx_md_exception.
    ENDTRY.

    LOOP AT it_materials ASSIGNING FIELD-SYMBOL(<ls_mat_collect>).
      DATA(ls_mat_exist) = REF #( lt_mats[ matid = <ls_mat_collect>-matid ] OPTIONAL ).

      IF ls_mat_exist IS INITIAL.
        APPEND <ls_mat_collect> TO lt_mats.
      ELSE.
        ls_mat_exist->quan += <ls_mat_collect>-quan.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_mats ASSIGNING FIELD-SYMBOL(<ls_mat>).
      DATA(ls_mat_global) = VALUE #( lt_mat_global[ matid = <ls_mat>-matid ] OPTIONAL ).

      CHECK ls_mat_global IS NOT INITIAL.

      " update QTY to main UoM
      IF ls_mat_global-meins <> <ls_mat>-meins.
        <ls_mat>-quan = convert_unit_quantity_single( iv_productid = <ls_mat>-matid
                                                      iv_uom_from  = <ls_mat>-meins
                                                      iv_uom_to    = ls_mat_global-meins
                                                      iv_qty_from  = <ls_mat>-quan ).
        <ls_mat>-meins = ls_mat_global-meins.
      ENDIF.

      DATA(ls_mat_uom) = VALUE #( lt_mat_uom[ matid = <ls_mat>-matid  meinh = <ls_mat>-meins ] OPTIONAL ).
      CHECK ls_mat_uom IS NOT INITIAL.

      " Update Units of measure: Length, Width, Height, Volume
      <ls_mat>-unit_lwh = ls_pmat_biggest-unit_lwh.
      <ls_mat>-length = ls_mat_uom-laeng.
      <ls_mat>-width = ls_mat_uom-breit.
      <ls_mat>-height = ls_mat_uom-hoehe.

      IF ls_pmat_biggest-unit_lwh <> ls_mat_uom-meabm.
        " update Length
        <ls_mat>-length = convert_unit_quantity_single( ""iv_productid = ls_mat_uom-matid
                                                        iv_uom_from  = ls_mat_uom-meabm
                                                        iv_uom_to    = ls_pmat_biggest-unit_lwh
                                                        iv_qty_from  = CONV #( ls_mat_uom-laeng ) ).
        " update Width
        <ls_mat>-width = convert_unit_quantity_single( ""iv_productid = ls_mat_uom-matid
                                                       iv_uom_from  = ls_mat_uom-meabm
                                                       iv_uom_to    = ls_pmat_biggest-unit_lwh
                                                       iv_qty_from  = CONV #( ls_mat_uom-breit ) ).
        " update Height
        <ls_mat>-height = convert_unit_quantity_single( ""iv_productid = ls_mat_uom-matid
                                                        iv_uom_from  = ls_mat_uom-meabm
                                                        iv_uom_to    = ls_pmat_biggest-unit_lwh
                                                        iv_qty_from  = CONV #( ls_mat_uom-hoehe ) ).
      ENDIF.

      " Update Volume
      <ls_mat>-unit_v = ls_pmat_biggest-unit_v.
      <ls_mat>-volum = ls_mat_uom-volum.
      IF ls_pmat_biggest-unit_v <> ls_mat_uom-voleh.
        <ls_mat>-volum = convert_unit_quantity_single( ""iv_productid = ls_mat_uom-matid
                                                       iv_uom_from  = ls_mat_uom-voleh
                                                       iv_uom_to    = ls_pmat_biggest-unit_v
                                                       iv_qty_from  = CONV #( ls_mat_uom-volum ) ).
      ENDIF.

    ENDLOOP.

    " calculate total volum for different materials. It is needed to sort them and start with material with biggest volum
    sort_set_materials_for_pack( it_materials = lt_mats ).

  ENDMETHOD.


  METHOD repack_candisp_materials.
********************************************************************
*& Key          : BSUGAREV-Jan 17, 2024
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  :
*&
*&
********************************************************************
    rt_pack_result = pack_by_best_pmat( it_materials = it_materials
                                        it_packmat   = it_packmat ).

    IF lines( rt_pack_result ) = 0.
      RETURN.
    ENDIF.

    TRY.
        mo_prod->read_material_multiple(
          EXPORTING
            it_matid      = VALUE #( FOR <l> IN it_materials ( <l>-matid ) )
            iv_lgnum      = mv_lgnum
            iv_entitled   = iv_entitled
          IMPORTING
            et_mat_lgnum  = DATA(lt_mat_lgnum)
            et_mat_uom    = DATA(lt_mat_uom) ).
        ##NO_HANDLER
      CATCH /scwm/cx_md_api_faulty_call.
        ##NO_HANDLER
      CATCH /scwm/cx_md_exception.
    ENDTRY.

    DATA(lt_pmat_cart) = mo_pmat_func->get_pmat_carton( ).

    LOOP AT rt_pack_result ASSIGNING FIELD-SYMBOL(<ls_pack>).

      DATA(ls_mat_mc_check) = VALUE zstr_mat_cuboid_input( ).

      CHECK lines( <ls_pack>-mat_details ) = 1.

      DATA(ls_matid_det) = <ls_pack>-mat_details[ 1 ].

      LOOP AT it_materials ASSIGNING FIELD-SYMBOL(<ls_mat_in>) WHERE matid = ls_matid_det-matid
                                                                 AND quan  >= ls_matid_det-pc_packed
                                                                 AND no_mc_pack = abap_false.
        ls_mat_mc_check = <ls_mat_in>.
        EXIT.
      ENDLOOP.

      CHECK ls_mat_mc_check IS NOT INITIAL.

      DATA(ls_mat_lgnum) = REF #( lt_mat_lgnum[ matid = ls_mat_mc_check-matid ] OPTIONAL ).

      CHECK ls_mat_lgnum IS NOT INITIAL AND ls_mat_lgnum->zz1_disp_whd = zif_wme_c=>gs_matdisp-can_dispatch.

      " for MC determine conveyable or nonconveyable pack material
      DATA(ls_mast_carton) = VALUE #( lt_pmat_cart[ nonconv = ls_mat_lgnum->zz1_nonconveyable_whd ] OPTIONAL ).

      LOOP AT lt_mat_uom ASSIGNING FIELD-SYMBOL(<ls_mat_uom>) WHERE matid = ls_mat_mc_check-matid
                                                                AND meinh = ls_mat_mc_check-bin_auom
                                                                AND umrez = ls_matid_det-pc_packed.

        DATA(ls_mc_volum) = mo_pmat_func->get_mc_volume( iv_matid     = ls_mat_mc_check-matid
                                                         iv_from_auom = ls_mat_mc_check-bin_auom ).

        <ls_pack>-volum = ls_mc_volum-quan.
        <ls_pack>-unit_v = ls_mc_volum-unit.
        <ls_pack>-pmat_guid = ls_mast_carton-matid.
        <ls_pack>-mc_flag   = ls_mat_mc_check-bin_auom.
        EXIT.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD repack_in_smaller_pmat.

    rt_pack_result = it_packed_stock.

    IF lines( it_packmat ) = 1.
      RETURN.
    ENDIF.

    LOOP AT rt_pack_result ASSIGNING FIELD-SYMBOL(<ls_pstock>).
      DATA(lt_packmat) = it_packmat.

      DATA(lv_pguid_last) = <ls_pstock>-pmat_guid.
      DO.
        DELETE lt_packmat WHERE pmat_guid = lv_pguid_last.

        IF lines( lt_packmat ) = 0.
          EXIT.
        ENDIF.

        DATA(lt_mats) = VALUE tt_mat_cuboid_input( FOR <m> IN <ls_pstock>-mat_details
          ( matid = <m>-matid
            quan  = <m>-pc_packed
            meins = <m>-unit ) ).

        DATA(lt_packed_stock) = pack_stock( it_materials = lt_mats
                                            it_packmat   = lt_packmat ).

        IF lines( lt_packed_stock ) = 1.
          " all material packed in 1 pack material => repacking is successful, use smaller pack material
          <ls_pstock>-pmat_guid = lt_packed_stock[ 1 ]-pmat_guid.
          <ls_pstock>-volum     = lt_packed_stock[ 1 ]-volum.

          lv_pguid_last = lt_packed_stock[ 1 ]-pmat_guid.
        ELSE.
          " stock is packed in more HUs => repacking failed, use the same pack material
          EXIT.
        ENDIF.

      ENDDO.
    ENDLOOP.

  ENDMETHOD.


  METHOD sort_set_materials_for_pack.
    TYPES:
      BEGIN OF ty_mat_volum,
        matid   TYPE /scwm/de_matid,
        volum_t TYPE volum,
        volum_s TYPE volum,
      END OF ty_mat_volum.

    DATA: lt_mat_volum TYPE STANDARD TABLE OF ty_mat_volum WITH EMPTY KEY.

    DATA(lt_mats) = it_materials.

    CLEAR: mt_materials.

    " calculate total volum for different materials. It is needed to sort them and start with material with biggest volum
    lt_mat_volum = VALUE #( FOR <mt> IN lt_mats ( matid   = <mt>-matid
                                                  volum_t = ( <mt>-volum * <mt>-quan )
                                                  volum_s = <mt>-volum ) ).

    SORT lt_mat_volum BY volum_t DESCENDING volum_s DESCENDING.

    " build a global table with sorted materials
    LOOP AT lt_mat_volum ASSIGNING FIELD-SYMBOL(<ls_mat_v>).
      DATA(ls_mat) = VALUE #( lt_mats[ matid = <ls_mat_v>-matid ] OPTIONAL ).

      APPEND ls_mat TO mt_materials.
    ENDLOOP.

    " update gt_materials_init only the first time to keep the list of all materials
    IF lines( mt_materials_init ) = 0.
      mt_materials_init = mt_materials.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
