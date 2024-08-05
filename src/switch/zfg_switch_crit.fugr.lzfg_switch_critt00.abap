*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_SWITCH_CRIT.................................*
TABLES: ZMV_SWITCH_CRIT, *ZMV_SWITCH_CRIT. "view work areas
CONTROLS: TCTRL_ZMV_SWITCH_CRIT
TYPE TABLEVIEW USING SCREEN '0002'.
DATA: BEGIN OF STATUS_ZMV_SWITCH_CRIT. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_SWITCH_CRIT.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_SWITCH_CRIT_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_SWITCH_CRIT.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_SWITCH_CRIT_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_SWITCH_CRIT_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_SWITCH_CRIT.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_SWITCH_CRIT_TOTAL.

*.........table declarations:.................................*
TABLES: ZTSWITCH_CRIT                  .
