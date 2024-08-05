*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_SWITCH_ACT..................................*
TABLES: ZMV_SWITCH_ACT, *ZMV_SWITCH_ACT. "view work areas
CONTROLS: TCTRL_ZMV_SWITCH_ACT
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_SWITCH_ACT. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_SWITCH_ACT.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_SWITCH_ACT_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_SWITCH_ACT.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_SWITCH_ACT_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_SWITCH_ACT_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_SWITCH_ACT.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_SWITCH_ACT_TOTAL.

*.........table declarations:.................................*
TABLES: ZTSWITCH_ACT                   .
