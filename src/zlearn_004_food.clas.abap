CLASS zlearn_004_food DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS: constructor,
      convert_string_to_xstring IMPORTING iv_string         TYPE string
                                RETURNING VALUE(iv_xstring) TYPE xstring,
      generate_access_token IMPORTING iv_url          TYPE string
                                      iv_username     TYPE string
                                      iv_password     TYPE string
                                      iv_data         TYPE string
                            RETURNING VALUE(rv_token) TYPE string,
      generate_invoice_number IMPORTING iv_url           TYPE string
                                        iv_access_token  TYPE string
                              RETURNING VALUE(rv_number) TYPE string,
      create_draft_invoice IMPORTING iv_url          TYPE string
                                     iv_access_token TYPE string
                                     iv_data         TYPE string
                           RETURNING VALUE(rv_id)    TYPE string,
      send_invoice IMPORTING iv_url          TYPE string
                             iv_access_token TYPE string
                             iv_data         TYPE string
                   RETURNING VALUE(rv_link)  TYPE string,
      link_to_browser IMPORTING iv_link_to_br TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
    TYPES: BEGIN OF ls_access_token,
             token_type   TYPE string,
             access_token TYPE string,
           END OF ls_access_token,
           BEGIN OF ls_invoice_number,
             invoice_number TYPE string,
           END OF ls_invoice_number,
           BEGIN OF ls_invoice_id,
             href TYPE string,
           END OF ls_invoice_id,
           BEGIN OF ls_invoice_link,
             href TYPE string,
           END OF ls_invoice_link.

    DATA: lv_access_token   TYPE ls_access_token,
          lv_invoice_number TYPE ls_invoice_number,
          lv_invoice_id     TYPE ls_invoice_id,
          lv_invoice_link   TYPE ls_invoice_link,
          lv_xstring        TYPE xstring,
          lv_http_status    TYPE i,
          lv_status_text    TYPE string,
          o_client          TYPE REF TO if_http_client.
ENDCLASS.



CLASS zlearn_004_food IMPLEMENTATION.
  METHOD constructor.

  ENDMETHOD.

  METHOD convert_string_to_xstring.
    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = iv_string
      IMPORTING
        buffer = iv_xstring.
  ENDMETHOD.

  METHOD generate_access_token.
    TRY.
        cl_http_client=>create_by_url( EXPORTING
                                         url     = iv_url
                                       IMPORTING
                                         client  = o_client ).
        o_client->authenticate( username = iv_username
                                password = iv_password ).
        o_client->request->set_data( data  = convert_string_to_xstring( iv_data ) ).
        o_client->request->set_header_field( name  = '~request_method'
                                             value = 'POST' ).
        o_client->send( timeout = if_http_client=>co_timeout_default ).
* Header-Felder explizit setzen
        o_client->receive( ).
        o_client->response->get_status( IMPORTING code   = lv_http_status
                                                  reason = lv_status_text ).
* Kontrolle ob der Request positiv verlaufen ist
        IF lv_http_status = 200.
          DATA(lv_result) = o_client->response->get_cdata(  ).
          /ui2/cl_json=>deserialize( EXPORTING json = lv_result
                                     CHANGING  data = lv_access_token ).
* Bearer Token wird extrahiert und über Returning Value zur Weiterverarbeitung gegeben
          rv_token = lv_access_token-access_token.
          o_client->close( ).
        ENDIF.
      CATCH cx_root INTO DATA(e_txt).
        WRITE: / e_txt->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD generate_invoice_number.
    TRY.
        cl_http_client=>create_by_url( EXPORTING
                                         url     = iv_url
                                       IMPORTING
                                         client  = o_client ).
        o_client->request->set_header_field( name  = 'Authorization'
                                             value = |Bearer { iv_access_token }| ).

        o_client->request->set_header_field( name = 'Content-Type' value ='application/json').
        o_client->request->set_header_field( name  = '~request_method'
                                        value = 'POST' ).
        o_client->send( timeout = if_http_client=>co_timeout_default ).
        o_client->receive( ).
        o_client->response->get_status( IMPORTING code   = lv_http_status
                                                 reason = lv_status_text ).
* Kontrolle ob der Request positiv verlaufen ist
        IF lv_http_status = 200.
          DATA(lv_result) = o_client->response->get_cdata(  ).
          /ui2/cl_json=>deserialize( EXPORTING json = lv_result
                                     CHANGING  data = lv_invoice_number ).
* Invoice Number wird extrahiert
          rv_number = lv_invoice_number-invoice_number.
          o_client->close( ).
        ENDIF.
      CATCH cx_root INTO DATA(e_txt).
        WRITE: / e_txt->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD create_draft_invoice.
    TRY.
        cl_http_client=>create_by_url( EXPORTING
                                         url     = iv_url
                                       IMPORTING
                                         client  = o_client ).
        o_client->request->set_header_field( name  = 'Authorization'
                                             value = |Bearer { iv_access_token }| ).
        o_client->request->set_header_field( name = 'Content-Type' value ='application/json').
        o_client->request->set_header_field( name  = '~request_method'
                                                value = 'POST' ).
        o_client->request->set_data( data  = convert_string_to_xstring( iv_data ) ).
        o_client->send( timeout = if_http_client=>co_timeout_default ).
        o_client->receive( ).
        o_client->response->get_status( IMPORTING code   = lv_http_status
                                                 reason = lv_status_text ).
* Kontrolle ob der Request positiv verlaufen ist
        IF lv_http_status = 201.
          DATA(lv_result) = o_client->response->get_cdata(  ).
          /ui2/cl_json=>deserialize( EXPORTING json = lv_result
                                     CHANGING  data = lv_invoice_id ).
* Invoice ID wird extrahiert
          rv_id = lv_invoice_id-href.
          o_client->close( ).
        ENDIF.

        DATA(lv_raw_message) = o_client->response->get_raw_message( ).
* vollständige HTTP Nachricht lesen
        DATA: lv_str_msg TYPE string.

* xstring -> string
        DATA(o_conv_r) = cl_abap_conv_in_ce=>create( input    = lv_raw_message
                                                   encoding = 'UTF-8' ).

        o_conv_r->read( IMPORTING data = lv_str_msg ).

      CATCH cx_root INTO DATA(e_txt).

        WRITE: / e_txt->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD send_invoice.
    TRY.
        cl_http_client=>create_by_url( EXPORTING
                                         url     = iv_url
                                       IMPORTING
                                         client  = o_client ).
        o_client->request->set_header_field( name  = 'Authorization'
                                             value = |Bearer { iv_access_token }| ).
        o_client->request->set_header_field( name = 'Content-Type' value ='application/json').

        o_client->request->set_header_field( name  = '~request_method'
                                                    value = 'POST' ).
        o_client->request->set_data( data  = convert_string_to_xstring( iv_data ) ).
        o_client->send( timeout = if_http_client=>co_timeout_default ).
        o_client->receive( ).
        o_client->response->get_status( IMPORTING code   = lv_http_status
                                                 reason = lv_status_text ).
        IF lv_http_status = 200.
          DATA(lv_result) = o_client->response->get_cdata(  ).
          /ui2/cl_json=>deserialize( EXPORTING json = lv_result
                                     CHANGING  data = lv_invoice_link ).
          rv_link = lv_invoice_link-href.
          o_client->close( ).
        ENDIF.

        DATA(lv_raw_message) = o_client->response->get_raw_message( ).
* vollständige HTTP Nachricht lesen
        DATA: lv_str_msg TYPE string.

* xstring -> string
        DATA(o_conv_r) = cl_abap_conv_in_ce=>create( input    = lv_raw_message
                                                   encoding = 'UTF-8' ).

        o_conv_r->read( IMPORTING data = lv_str_msg ).
      CATCH cx_root INTO DATA(e_txt).

        WRITE: / e_txt->get_text( ).
    ENDTRY.
  ENDMETHOD.

  METHOD link_to_browser.
*    MESSAGE iv_link_to_br TYPE 'I' DISPLAY LIKE 'I'.

*    cl_abap_browser=>show_url( url          = iv_link_to_br
*                               title        = 'ABAP-Browser'
*                               size         = cl_abap_browser=>large
*                               modal        = abap_true
*                               printing     = abap_false
*                               buttons      = abap_true
*                               format       = cl_abap_browser=>landscape
*                               position     = cl_abap_browser=>topleft
*                               context_menu = abap_false ).

*    CALL FUNCTION 'CALL_BROWSER'
*      EXPORTING
*        url                    = iv_link_to_br
*      EXCEPTIONS
*        frontend_not_supported = 1
*        frontend_error         = 2
*        prog_not_found         = 3
*        no_batch               = 4
*        unspecified_error      = 5
*        OTHERS                 = 6.

    cl_gui_frontend_services=>execute(
    EXPORTING
    parameter   = iv_link_to_br
    application = '"C:\Program Files (x86)\Microsoft\Edge\Application\msedge.exe"').


  ENDMETHOD.

ENDCLASS.
