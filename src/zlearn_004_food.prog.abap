*&---------------------------------------------------------------------*
*& Report zlearn_004_food
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zlearn_004_food.

TRY.
* URL
    DATA(lv_url) = |https://world.openfoodfacts.org/api/v0/product/41001318/|.

    DATA: o_client TYPE REF TO if_http_client.

* Client-Objekt erzeugen
    cl_http_client=>create_by_url( EXPORTING
                                     url     = lv_url
                                   IMPORTING
                                     client  = o_client ).

    IF o_client IS BOUND.
* Anmeldedaten übermitteln
      o_client->authenticate( username = ''
                              password = '' ).

* Logon-Popup ein- bzw. ausschalten
      o_client->propertytype_logon_popup = o_client->co_enabled.

* HTTP-Prtotokoll-Version
      o_client->request->set_version( version = if_http_request=>co_protocol_version_1_1 ).
* HTTP-Method
      o_client->request->set_method( if_http_request=>co_request_method_get ).

* Header-Felder explizit setzen
      o_client->request->set_header_field( name  = '~request_method'
                                           value = 'GET' ).

      o_client->request->set_header_field( name  = 'Content-Type'
                                           value = 'text/xml; charset=utf-8' ).

      o_client->request->set_header_field( name  = 'Accept'
                                           value = 'text/xml, text/html' ).

* Cookies akzeptieren
      o_client->propertytype_accept_cookie = if_http_client=>co_enabled.
* Kompression akzeptieren
      o_client->propertytype_accept_compress = if_http_client=>co_enabled.

* HTTP GET senden, evtl. Timeout angeben
      o_client->send( timeout = if_http_client=>co_timeout_default ).
       DATA: LV_CL_JSON_PARSER TYPE REF TO /ui5/cl_json_parser.

* Response lesen
      o_client->receive( ).

      DATA: lv_http_status TYPE i.
      DATA: lv_status_text TYPE string.

* HTTP Return Code holen
      o_client->response->get_status( IMPORTING
                                        code   = lv_http_status
                                        reason = lv_status_text ).

      WRITE: / 'HTTP_STATUS_CODE:', lv_http_status.
      WRITE: / 'STATUS_TEXT:', lv_status_text.

* Header-Daten der Übertragung
      WRITE: / 'HEADER_FIELDS:'.
      DATA(it_header_fields) = VALUE tihttpnvp( ).
      o_client->response->get_header_fields( CHANGING fields = it_header_fields ).

      LOOP AT it_header_fields ASSIGNING FIELD-SYMBOL(<f>).
        WRITE: / '[', <f>-name, ']', <f>-value.
      ENDLOOP.

* Cookies holen
      WRITE: / 'COOKIES:'.
      DATA(it_cookies) = VALUE tihttpcki( ).
      o_client->response->get_cookies( CHANGING cookies = it_cookies ).

      LOOP AT it_cookies ASSIGNING FIELD-SYMBOL(<c>).
        WRITE: / '[', <c>-name, ']', <c>-value, <c>-xdomain, <c>-path, <c>-secure, <c>-expires.
      ENDLOOP.

* vollständige HTTP Nachricht lesen
      DATA(lv_raw_message) = o_client->response->get_raw_message( ).
      DATA: lv_str_msg TYPE string.

* xstring -> string
      DATA(o_conv_r) = cl_abap_conv_in_ce=>create( input    = lv_raw_message
                                                   encoding = 'UTF-8' ).

      o_conv_r->read( IMPORTING data = lv_str_msg ).

      WRITE: / 'RAW MESSAGE:', lv_str_msg.

* Wenn Status 200 (Ok)
      IF lv_http_status = 200.
* HTTP Body als Character-Daten
        DATA(lv_result) = o_client->response->get_cdata( ).
     CREATE OBJECT LV_CL_JSON_PARSER.
     LV_CL_JSON_PARSER->PARSE( JSON = LV_result ).
        WRITE: / 'CDATA:'.
        WRITE: / lv_result.

    data(LS_CODE) = LV_CL_JSON_PARSER->VALUE( PATH = '/code' ).
    DATA(ls_code_co2) = lv_cl_json_parser->value( Path = '/product/ecoscore_data/agribalyse/co2_total' ).
    WRITE: / ls_code,
           / 'CO2-Verbrauch:', ls_code_co2, 'g CO2 pro 1g'.
      ENDIF.
* HTTP Connection schließen
      o_client->close( ).
    ENDIF.
  CATCH cx_root INTO DATA(e_txt).
    WRITE: / e_txt->get_text( ).
ENDTRY.
