*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_PARAM_MAST..................................*
TABLES: ZMV_PARAM_MAST, *ZMV_PARAM_MAST. "view work areas
CONTROLS: TCTRL_ZMV_PARAM_MAST
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_PARAM_MAST. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_PARAM_MAST.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_PARAM_MAST_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_PARAM_MAST.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_PARAM_MAST_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_PARAM_MAST_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_PARAM_MAST.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_PARAM_MAST_TOTAL.

*.........table declarations:.................................*
TABLES: ZTPARAM_MAST                   .
