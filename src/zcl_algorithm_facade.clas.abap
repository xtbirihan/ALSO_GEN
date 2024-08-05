class ZCL_ALGORITHM_FACADE definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_lptyp_maxcapa,
        lgtyp_r TYPE /scwm/tt_lgtyp_r,
        lptyp_r	TYPE /scwm/tt_lptyp_r,
        matid_r	TYPE /scwm/tt_matid_r,
      END OF ty_lptyp_maxcapa .
  types:
    tt_lptyp_maxcapa TYPE STANDARD TABLE OF ty_lptyp_maxcapa WITH EMPTY KEY .

  class-methods CHECK_PACK_MAT_IS_TOTE
    importing
      !IV_MATID type /SCWM/DE_MATID
      !IV_LGNUM type /SCWM/LGNUM
    returning
      value(RV_TRUE) type XFELD .
  class-methods DETERMINE_BINTYP_CAPACITY
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IT_ENTITLED type /SCWM/TT_ENTITLED
      !IV_QUERY_DB_QTY type BOOLE_D default ABAP_TRUE
      !IS_SELECT_CRIT type TY_LPTYP_MAXCAPA
      !IS_MAT_LGTYP type /SCWM/S_MATERIAL_LGTYP optional
    exporting
      !ET_LPTYP_MAXQTY type ZCL_CRUD_ZTLPTYP_MAXQTY=>TT_LPTYP_MAXQTY .
  PROTECTED SECTION.

private section.
ENDCLASS.



CLASS ZCL_ALGORITHM_FACADE IMPLEMENTATION.


  METHOD check_pack_mat_is_tote.
********************************************************************
*& Key          : <AAHMEDOV>-Dec 4, 2023
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
* Description  : If pack mat of import material is tote
* direct replenishment in material master = false
********************************************************************

    DATA(lt_packspec_content) = NEW zcl_packmmat_algo( iv_lgnum )->get_pmat_by_matid(
      EXPORTING
        it_matids = VALUE #( ( matid = iv_matid ) )
    ).

    DATA(lv_packmat) = VALUE #( lt_packspec_content[ 1 ]-levels[ quancla = zif_wme_c=>gs_quancla-totes ]-hu_mat OPTIONAL ).

    SHIFT lv_packmat LEFT DELETING LEADING '0'.

    IF lv_packmat IS INITIAL.
      RETURN.
    ENDIF.

    zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = iv_lgnum                 " Warehouse Number/Warehouse Complex
        iv_process   = zif_param_const=>c_zcross_0005                " Process ID (Specification, Program, BAdI etc.)
        iv_parameter = zif_param_const=>c_pmat_tote                 " Parameter ID for process
      IMPORTING
        et_range     = DATA(lt_tote_r)                 " SELECT-OPTIONS Table
    ).

    rv_true = xsdbool( lv_packmat IN lt_tote_r ).

  ENDMETHOD.


  METHOD determine_bintyp_capacity.
********************************************************************
*& Key          : <BSUGAREV>-04.05.2023 15:45:56
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  : Determine capacity of bin type depending on different
*&    algorithms. Type of algorithm for each bin type is maintained in
*&    in table ZTLGTYP_ALGO.
*&    1. check if bin capacity is already determined and saved in
*&       table ZTLPTYP_MAXQTY. If this is the case new calculation for
*&       this bin type is not done
*&    2. Cuboid Algorithm:    ZCL_CUBOID_ALGORITHM=>CALC_BIN_TYPE_CAPACITY
*&    3. Flowrack Algorithm:  ZCL_FLOWRACK_ALGORITHM=>CALC_CAPACITY
*&    4. Dynamic Algorithm:   ZCL_DYNAMIC_ALGORITHM=>CALC_BINTYP_CAPACITY
********************************************************************
    CONSTANTS:
      lc_dynamic_alg TYPE zde_algo VALUE '9'.

    DATA: lt_bintypes_maxcapa TYPE zcl_crud_ztlptyp_maxqty=>tt_lptyp_maxqty,
          lt_mat_lgtyp        TYPE /scwm/tt_material_lgtyp.

    CLEAR: et_lptyp_maxqty.

    TRY.
        DATA(lo_cuboid) = NEW zcl_cuboid_algorithm( iv_lgnum ).
        DATA(lo_flowrack) = NEW zcl_flowrack_algorithm( iv_lgnum ).
      CATCH zcx_core_exception.
        RETURN.
    ENDTRY.

    " select bin types for storage types
    DATA(lt_stortyp_bintyp_map) = zcl_crud_scwm_t303s=>select_multi(
      iv_lgnum   = iv_lgnum
      it_lgtyp_r = is_select_crit-lgtyp_r
      it_lptyp_r = is_select_crit-lptyp_r ).

    " build selection option table with LGTYP from select result. Continue working with this data
    DATA(lt_lgtyp_from_t303s_r) = VALUE /scwm/tt_lgtyp_r( FOR <l> IN lt_stortyp_bintyp_map
        ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = <l>-lgtyp ) ).
    SORT lt_lgtyp_from_t303s_r BY low.
    DELETE ADJACENT DUPLICATES FROM lt_lgtyp_from_t303s_r COMPARING low.

    " build selection option table with LPTYP from select result. Continue working with this data
    DATA(lt_lptyp_from_t303s_r) = VALUE /scwm/tt_lptyp_r( FOR <l> IN lt_stortyp_bintyp_map
        ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = <l>-lptyp ) ).
    SORT lt_lptyp_from_t303s_r BY low.
    DELETE ADJACENT DUPLICATES FROM lt_lptyp_from_t303s_r COMPARING low.

    IF iv_query_db_qty = abap_true.
      zcl_crud_ztlptyp_maxqty=>select_multi_by_all_filters(
        EXPORTING
          iv_lgnum          = iv_lgnum
          it_lgtyp_r        = lt_lgtyp_from_t303s_r
          it_lptyp_r        = lt_lptyp_from_t303s_r
          it_matid_r        = VALUE #( FOR <mi> IN is_select_crit-matid_r ( CORRESPONDING #( <mi> ) ) )
        IMPORTING
          et_ztlptyp_maxqty = et_lptyp_maxqty ).
    ENDIF.

    " select algorithm for storage types
    zcl_crud_ztlgtyp_algo=>select_multi_by_lgnum_lgtyp(
      EXPORTING
        iv_lgnum        = iv_lgnum
        it_lgtyp_r      = lt_lgtyp_from_t303s_r
      IMPORTING
        et_ztlgtyp_algo = DATA(lt_lgtyp_algo) ).

    " determine materials storage data, we are interested from direct replenishment indicator
    DATA(lo_prod) = CAST /scwm/if_af_product( /scdl/cl_af_management=>get_instance(
        )->get_service( /scwm/if_af_product=>sc_me_as_service ) ).

    LOOP AT it_entitled ASSIGNING FIELD-SYMBOL(<ls_ent>).

      "in case this method has been called from the slotting transaction
      "get data from is_mat_lgtyp
      "because the result from read_material_multiple could be empty
      "while there is no storage type data for the material yet
      IF is_mat_lgtyp IS NOT INITIAL.

*        APPEND CORRESPONDING #( is_mat_lgtyp ) TO lt_mat_lgtyp.
        APPEND VALUE /scwm/s_material_lgtyp( matid = is_mat_lgtyp-matid
                                             entitled = is_mat_lgtyp-entitled
                                             lgtyp = is_mat_lgtyp-lgtyp
                                             lgnum = is_mat_lgtyp-lgnum ) TO lt_mat_lgtyp.

      ELSE.

        TRY.
            lo_prod->read_material_multiple(
              EXPORTING
                it_matid     = VALUE #( FOR <m> IN is_select_crit-matid_r ( <m>-low ) )  ""lt_matids_list
                iv_entitled  = <ls_ent>-entitled
                iv_lgnum     = iv_lgnum
                it_lgtyp     = VALUE #( FOR <stt> IN lt_lgtyp_algo WHERE ( algo = zif_wme_c=>gs_algorithms-flowrack ) ( <stt>-lgtyp ) )
              IMPORTING
                et_mat_lgtyp = DATA(lt_mat_lgtyp_temp) ).

            APPEND LINES OF lt_mat_lgtyp_temp TO lt_mat_lgtyp.
            ##NO_HANDLER
          CATCH /scwm/cx_md_api_faulty_call.
            ##NO_HANDLER
          CATCH /scwm/cx_md_exception.
        ENDTRY.
      ENDIF.
    ENDLOOP.

    LOOP AT is_select_crit-matid_r ASSIGNING FIELD-SYMBOL(<ls_matid>).

      DATA(lt_bintypes) = VALUE /scwm/tt_t303s( ).
      LOOP AT lt_stortyp_bintyp_map ASSIGNING FIELD-SYMBOL(<ls_stortyp_bintyp_map>).

        " check if combination (LGTYP-LPTYP-MATID) already exist in ZTLPTYP_MAXQTY
        " continue working only with ones that not exist in ZTLPTYP_MAXQTY
        CHECK NOT line_exists( et_lptyp_maxqty[ lgnum = <ls_stortyp_bintyp_map>-lgnum
                                                lgtyp = <ls_stortyp_bintyp_map>-lgtyp
                                                lptyp = <ls_stortyp_bintyp_map>-lptyp
                                                matid = <ls_matid>-low ] ).

        lt_bintypes = VALUE #( BASE lt_bintypes ( lgtyp = <ls_stortyp_bintyp_map>-lgtyp
                                                  lptyp = <ls_stortyp_bintyp_map>-lptyp ) ).
      ENDLOOP.

      CHECK lines( lt_bintypes ) > 0.

      LOOP AT lt_bintypes ASSIGNING FIELD-SYMBOL(<ls_dummy>) GROUP BY ( lgtyp = <ls_dummy>-lgtyp )
                          ASSIGNING FIELD-SYMBOL(<ls_bintyps>).

        DATA(ls_algo_type) = VALUE #( lt_lgtyp_algo[ lgnum = iv_lgnum lgtyp = <ls_bintyps>-lgtyp ] OPTIONAL ).

        CHECK ls_algo_type IS NOT INITIAL.

        DATA(lt_bintypes_for_algo) = VALUE /scwm/tt_t303s( ).

        LOOP AT GROUP <ls_bintyps> ASSIGNING FIELD-SYMBOL(<ls_bintyp>).
          lt_bintypes_for_algo = VALUE #( BASE lt_bintypes_for_algo
              ( lgtyp = <ls_bintyp>-lgtyp
                lptyp = <ls_bintyp>-lptyp ) ).
        ENDLOOP.

        CLEAR: lt_bintypes_maxcapa.

        IF ls_algo_type-algo = zif_wme_c=>gs_algorithms-cuboid.
          lt_bintypes_maxcapa = lo_cuboid->calc_bin_type_capacity( iv_matid    = <ls_matid>-low
                                                                   it_bintypes = lt_bintypes_for_algo ).


          CHECK lines( lt_bintypes_maxcapa ) > 0.

          INSERT LINES OF lt_bintypes_maxcapa INTO TABLE et_lptyp_maxqty.
        ELSE.

          "BEGIN MOD <AAHMEDOV>-Jan 22, 2023
          " In case the product master does not contain any storage type data,
          " check if the packaging material of this product is a tote
          " if yes -> caclulate the max bin type capacities with the
          " dynamic algorithm
          IF NOT line_exists( lt_mat_lgtyp[ matid = <ls_matid>-low
                                        lgtyp = <ls_bintyps>-lgtyp ] ).

            IF check_pack_mat_is_tote( iv_matid = <ls_matid>-low
                                       iv_lgnum = iv_lgnum ) = abap_true.

              NEW zcl_dynamic_algorithm( )->calc_without_lgtyp_data(
                EXPORTING
                  iv_lgnum        = iv_lgnum
                  iv_entitled     = VALUE #( it_entitled[ 1 ] OPTIONAL ) "
                  iv_matid        = <ls_matid>-low
                  it_lgtyp        = VALUE #( ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = <ls_bintyps>-lgtyp ) )
                IMPORTING
                  et_lptyp_maxqty = lt_bintypes_maxcapa
              ).

              IF lines( is_select_crit-lptyp_r ) > 0.
                DELETE lt_bintypes_maxcapa WHERE lptyp NOT IN is_select_crit-lptyp_r. "#EC CI_SORTSEQ
              ENDIF.

              " remove entries already found in ztlptyp_maxqty
              LOOP AT lt_bintypes_maxcapa ASSIGNING FIELD-SYMBOL(<ls_bint_capa>).

                IF line_exists( et_lptyp_maxqty[ lgnum = <ls_bint_capa>-lgnum lgtyp = <ls_bint_capa>-lgtyp lptyp = <ls_bint_capa>-lptyp matid = <ls_bint_capa>-matid ] ).
                  DELETE lt_bintypes_maxcapa.
                ENDIF.
              ENDLOOP.

              IF lines( lt_bintypes_maxcapa ) > 0.
                INSERT LINES OF lt_bintypes_maxcapa INTO TABLE et_lptyp_maxqty.
              ENDIF.

            ENDIF.

          ENDIF.
          "END MOD <AAHMEDOV>-Jan 22, 2023

          LOOP AT lt_mat_lgtyp ASSIGNING FIELD-SYMBOL(<ls_mat_lgtyp>) WHERE matid = <ls_matid>-low
                                                                        AND lgtyp = <ls_bintyps>-lgtyp.
            " If algorithm is flowrack and direct replenishment flag (material/storage type) is true
            " bintype capacity will be calsulated by dynamic algorithm

            "BEGIN MOD <AAHMEDOV>-Dec 5, 2023
            "check if pack material from pack spec of this product is tote
            "or zz1_dirrpl_stt eq abap_true
            IF ls_algo_type-algo = zif_wme_c=>gs_algorithms-flowrack AND ( check_pack_mat_is_tote(
                                                                           iv_matid = <ls_mat_lgtyp>-matid                  " Product
                                                                           iv_lgnum = iv_lgnum ) = abap_true AND               " Warehouse Number/Warehouse Complex
                                                                         <ls_mat_lgtyp>-zz1_dirrpl_stt = abap_false ).
              ls_algo_type-algo = lc_dynamic_alg.
            ENDIF.
            "END MOD <AAHMEDOV>-Dec 5, 2023

            CASE ls_algo_type-algo.
              WHEN zif_wme_c=>gs_algorithms-flowrack.

                lt_bintypes_maxcapa = lo_flowrack->calc_capacity( iv_matid    = <ls_matid>-low
                                                                  iv_entitled = <ls_mat_lgtyp>-entitled
                                                                  it_bintypes = lt_bintypes_for_algo ).
              WHEN OTHERS.
                NEW zcl_dynamic_algorithm( )->calc_bintyp_capacity(
                  EXPORTING
                    iv_lgnum        = iv_lgnum
                    iv_entitled     = <ls_mat_lgtyp>-entitled
                    iv_matid        = <ls_matid>-low
                    is_mat_lgtyp    = is_mat_lgtyp
                    it_lgtyp        = VALUE #( ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = <ls_bintyps>-lgtyp ) )
                  IMPORTING
                    et_lptyp_maxqty = lt_bintypes_maxcapa ).

                IF lines( is_select_crit-lptyp_r ) > 0.
                  DELETE lt_bintypes_maxcapa WHERE lptyp NOT IN is_select_crit-lptyp_r. "#EC CI_SORTSEQ
                ENDIF.

                " remove entries already found in ztlptyp_maxqty
                LOOP AT lt_bintypes_maxcapa ASSIGNING <ls_bint_capa>.

                  IF line_exists( et_lptyp_maxqty[ lgnum = <ls_bint_capa>-lgnum lgtyp = <ls_bint_capa>-lgtyp lptyp = <ls_bint_capa>-lptyp matid = <ls_bint_capa>-matid ] ).
                    DELETE lt_bintypes_maxcapa.
                  ENDIF.
                ENDLOOP.

            ENDCASE.

            CHECK lines( lt_bintypes_maxcapa ) > 0.

            INSERT LINES OF lt_bintypes_maxcapa INTO TABLE et_lptyp_maxqty.
          ENDLOOP.
        ENDIF.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
