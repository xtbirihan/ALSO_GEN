*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_PARAM_CUST..................................*
TABLES: ZMV_PARAM_CUST, *ZMV_PARAM_CUST. "view work areas
CONTROLS: TCTRL_ZMV_PARAM_CUST
TYPE TABLEVIEW USING SCREEN '0002'.
DATA: BEGIN OF STATUS_ZMV_PARAM_CUST. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_PARAM_CUST.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_PARAM_CUST_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_PARAM_CUST.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_PARAM_CUST_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_PARAM_CUST_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_PARAM_CUST.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_PARAM_CUST_TOTAL.

*...processing: ZTPARAM_CUST....................................*
DATA:  BEGIN OF STATUS_ZTPARAM_CUST                  .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTPARAM_CUST                  .
CONTROLS: TCTRL_ZTPARAM_CUST
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTPARAM_CUST                  .
TABLES: ZTPARAM_CUST                   .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
