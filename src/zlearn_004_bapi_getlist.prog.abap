*&---------------------------------------------------------------------*
*& Report zlearn_004_bapi_getlist
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zlearn_004_bapi_getlist.

START-OF-SELECTION.

    DATA: ls_headerlist    type standard table of bapi_incinv_getlist_header with header line,
            ls_return  type standard table of bapiret2 with header line,
            ls_headerlist_length type i,
            ls_vendor type standard table of BAPI_INCINV_VENDOR_RA.
    CALL FUNCTION 'BAPI_INCOMINGINVOICE_GETLIST'
*      EXPORTING
*        tm_documents  = 'X'
*        erp_documents = 'X'
      TABLES
         vendor_ra     = ls_vendor
*        pstngdate_ra  =
*        docdate_ra    =
*        cpudate_ra    =
*        person_ext_ra =
*        status_ra     =
*        refdoc_ra     =
        headerlist    = ls_headerlist
        return        = ls_return.
    DESCRIBE TABLE ls_headerlist LINES ls_headerlist_length.
    WRITE: ls_headerlist_length.
