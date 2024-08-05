INTERFACE zif_wme_c
  PUBLIC .

  CONSTANTS:
    BEGIN OF gs_act_typ,
      intl TYPE /scwm/de_actty VALUE 'INTL',
      pick TYPE /scwm/de_actty VALUE 'PICK',
      ptwy TYPE /scwm/de_actty VALUE 'PTWY',
      repl TYPE /scwm/de_actty VALUE 'REPL',
    END OF gs_act_typ .
  CONSTANTS:
    BEGIN OF gs_exccode,
      chbd TYPE /scwm/de_exccode VALUE 'CHBD',
    END OF gs_exccode .
  CONSTANTS:
    BEGIN OF gs_msgobj,
      zewm TYPE balsub-object VALUE 'ZEWM',
    END OF gs_msgobj .
  CONSTANTS:
    BEGIN OF gs_msgsubobj,
      zwho_pack        TYPE balsub-subobject VALUE 'ZWHO_PACK',
      zwho_upd_dem_qty TYPE balsub-subobject VALUE 'ZWHO_UPD_DEM_QTY',
      zcrs_upd_max_qty TYPE balsub-subobject VALUE 'ZCROSS_UPDATE_MAXQTY',
      zworkstation     TYPE balsub-subobject VALUE 'ZWORKSTATION',
      zout_pack_ui     TYPE balsub-subobject VALUE 'ZOUT_PACK_UI',
      zworkcenter      TYPE balsub-subobject VALUE 'ZWORKCENTER',
      zodata_vce       TYPE balsub-subobject VALUE 'ZODATA_VCE',
    END OF gs_msgsubobj .
  CONSTANTS:
    BEGIN OF gs_zptwy_rule,
      add_to_stock TYPE zde_put_rule VALUE '1', " Addition to stock
      empt_bin     TYPE zde_put_rule VALUE '2', " Empty bin
      empt_bin_max TYPE zde_put_rule VALUE '3', " Empty bin = X (MAX)
      empt_bin_1   TYPE zde_put_rule VALUE '4', " Empty bin = 1
    END OF gs_zptwy_rule .
  CONSTANTS:
    BEGIN OF gs_zsplit,
      no TYPE zde_splitput_lgtyp VALUE 'N',  " Do not split quant
      yb TYPE zde_splitput_lgtyp VALUE 'YB', " Split quant (between several Storage Bin of Storage Type)
      ys TYPE zde_splitput_lgtyp VALUE 'YS', " Split quant (between several Storage Types)
    END OF gs_zsplit .
  CONSTANTS:
    BEGIN OF gs_uom,
      mm  TYPE meins VALUE 'MM',
      cm  TYPE meins VALUE 'CM',
      dm  TYPE meins VALUE 'DM',
      m   TYPE meins VALUE 'M',
      mmq TYPE meins VALUE 'MMQ',
      ccm TYPE meins VALUE 'CCM',
      cdm TYPE meins VALUE 'CDM',
      m3  TYPE meins VALUE 'M3',
      kg  TYPE meins VALUE 'KG',
      pal TYPE meins VALUE 'PAL',
    END OF gs_uom .
  CONSTANTS:
    BEGIN OF gs_packprofile,
      p1pc TYPE /scwm/de_wcrpp VALUE 'P1PC',
      pslo TYPE /scwm/de_wcrpp VALUE 'PSLO',
      pshu TYPE /scwm/de_wcrpp VALUE 'PSHU',
    END OF gs_packprofile .
  CONSTANTS:
    BEGIN OF gs_whohu_upd,
      create TYPE /scwm/s_whohu-updkz VALUE 'I',
      change TYPE /scwm/s_whohu-updkz VALUE 'U',
      delete TYPE /scwm/s_whohu-updkz VALUE 'D',
    END OF gs_whohu_upd .
  CONSTANTS:
    BEGIN OF gs_algorithms,
      cuboid   TYPE zde_algo VALUE '1',
      flowrack TYPE zde_algo VALUE '2',
    END OF gs_algorithms.
  CONSTANTS:
    BEGIN OF gs_quancla,
      totes TYPE /scwm/de_quancla VALUE '2', "Totes/Mastercarton
    END OF gs_quancla .
  CONSTANTS:
    BEGIN OF gs_bin_capa,
      update TYPE char4 VALUE 'ZTBU', "UPDATE BIN TOTAL CAPACITY
      clear  TYPE /scwm/de_actty VALUE 'ZTBC', "CLEAR BIN TOTAL CAPACITY
    END OF gs_bin_capa .
  CONSTANTS:
    BEGIN OF gs_matdisp,
      dispatch     TYPE zz1_disp VALUE 'D',
      can_dispatch TYPE zz1_disp VALUE 'C',
      non_dispatch TYPE zz1_disp VALUE 'N',
    END OF gs_matdisp.
  CONSTANTS:
    BEGIN OF gs_procty,
      repl      TYPE /scwm/de_procty VALUE 'P310',
      immediate TYPE /scwm/de_procty VALUE 'Z999',
    END OF gs_procty.
  CONSTANTS:
    BEGIN OF gs_nrobj,
      qseq_obj             TYPE nrobj VALUE 'ZQUEUE_SEQ', "Queue Sequence for ZTOUT_QUEUE_DET table
      qseq_nrrange         TYPE nrnr VALUE '01',
      tote_shipmat_obj     TYPE nrobj  VALUE 'ZTOTE_PMAT', "Tote : Small Ship-Packaging Material Mapping Sequence for ZTOUT_TOTE_PMAT
      tote_shipmat_nrrange TYPE nrnr VALUE '01',
    END OF gs_nrobj.
  CONSTANTS:
    BEGIN OF gs_ppmat,
      pshu TYPE zde_ppmat VALUE 1, "PSHU
      mast TYPE zde_ppmat VALUE 2, "MAST
    END OF gs_ppmat.
  CONSTANTS:
    BEGIN OF gs_hupacktyp,
      slo  TYPE zde_packtyp VALUE 'SLO',
      spo  TYPE zde_packtyp VALUE 'SPO',
      mc   TYPE zde_packtyp VALUE 'MC',
      sc   TYPE zde_packtyp VALUE 'SC',
      tote TYPE zde_packtyp VALUE 'TOTE',
    END OF gs_hupacktyp.
  CONSTANTS:
    BEGIN OF gs_lgtyp_autoconf,
      conv TYPE char4 VALUE 'ZCNV',
    END OF gs_lgtyp_autoconf.
  CONSTANTS:
    BEGIN OF gs_hustep,
      init TYPE	/scwm/de_hustep VALUE 'I',
    END OF gs_hustep.
ENDINTERFACE.
