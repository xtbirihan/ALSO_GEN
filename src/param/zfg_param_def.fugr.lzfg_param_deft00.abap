*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_PARAM_DEF...................................*
TABLES: ZMV_PARAM_DEF, *ZMV_PARAM_DEF. "view work areas
CONTROLS: TCTRL_ZMV_PARAM_DEF
TYPE TABLEVIEW USING SCREEN '0002'.
DATA: BEGIN OF STATUS_ZMV_PARAM_DEF. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_PARAM_DEF.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_PARAM_DEF_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_PARAM_DEF.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_PARAM_DEF_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_PARAM_DEF_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_PARAM_DEF.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_PARAM_DEF_TOTAL.

*...processing: ZTPARAM_DEF.....................................*
DATA:  BEGIN OF STATUS_ZTPARAM_DEF                   .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTPARAM_DEF                   .
CONTROLS: TCTRL_ZTPARAM_DEF
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTPARAM_DEF                   .
TABLES: ZTPARAM_DEF                    .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
