REPORT  /pstech/bci_intf_monitor3.

************************ Selection Screen ******************************
DATA: s_intfms TYPE /pstech/bcintfms.
DATA: s_intfmm TYPE /pstech/bcintfmm.
DATA: s_xmlstr TYPE c LENGTH 255.
SELECTION-SCREEN BEGIN OF BLOCK b10 WITH FRAME TITLE text-b10.
SELECT-OPTIONS: s_intfid FOR s_intfms-interface_id NO INTERVALS.
SELECT-OPTIONS: s_intfk1 FOR s_intfms-interface_key1.
SELECT-OPTIONS: s_intfk2 FOR s_intfms-interface_key2.
SELECT-OPTIONS: s_intfk3 FOR s_intfms-interface_key3.
SELECT-OPTIONS: s_intfk4 FOR s_intfms-interface_key4.
SELECT-OPTIONS: s_intfmi FOR s_intfms-interface_msgid.
SELECT-OPTIONS: s_intfty FOR s_intfmm-interface_type NO INTERVALS.
SELECT-OPTIONS: s_intfdi FOR s_intfmm-interface_dir NO INTERVALS.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (31) text-a02 FOR FIELD s_exfrdt.
PARAMETERS: s_exfrdt TYPE datum DEFAULT sy-datum.
SELECTION-SCREEN COMMENT 44(1) text-a03 FOR FIELD s_exfrtm.
PARAMETERS: s_exfrtm TYPE uzeit.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (31) text-a04 FOR FIELD s_extodt.
PARAMETERS: s_extodt TYPE datum DEFAULT sy-datum.
SELECTION-SCREEN COMMENT 44(1) text-a03 FOR FIELD s_extotm.
PARAMETERS: s_extotm TYPE uzeit DEFAULT '240000'.
SELECTION-SCREEN END OF LINE.
PARAMETERS: s_intfmx TYPE tbmaxsel DEFAULT '500'.
SELECTION-SCREEN BEGIN OF BLOCK b11 WITH FRAME TITLE text-b11.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (8) text-a05 FOR FIELD s_xmle1.
SELECT-OPTIONS: s_xmle1 FOR s_xmlstr VISIBLE LENGTH 15 NO-EXTENSION NO INTERVALS LOWER CASE.
SELECTION-SCREEN COMMENT 35(8) text-a06 FOR FIELD s_xmlv1.
SELECT-OPTIONS: s_xmlv1 FOR s_xmlstr VISIBLE LENGTH 15 NO-EXTENSION NO INTERVALS LOWER CASE.
PARAMETERS: s_xmlo1 AS LISTBOX VISIBLE LENGTH 8 DEFAULT '1'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (8) text-a05 FOR FIELD s_xmle2.
SELECT-OPTIONS: s_xmle2 FOR s_xmlstr VISIBLE LENGTH 15 NO-EXTENSION NO INTERVALS LOWER CASE.
SELECTION-SCREEN COMMENT 35(8) text-a06 FOR FIELD s_xmlv2.
SELECT-OPTIONS: s_xmlv2 FOR s_xmlstr VISIBLE LENGTH 15 NO-EXTENSION NO INTERVALS LOWER CASE.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b11.
SELECTION-SCREEN END OF BLOCK b10.
SELECTION-SCREEN BEGIN OF BLOCK b20 WITH FRAME TITLE text-b20.
SELECTION-SCREEN BEGIN OF BLOCK b22 WITH FRAME TITLE text-b22.
PARAMETERS: s_parrfc TYPE spta_rfcgr MEMORY ID spta_rfcgr.
PARAMETERS: s_parmax TYPE syindex DEFAULT '0'.
SELECTION-SCREEN END OF BLOCK b22.
SELECTION-SCREEN BEGIN OF BLOCK b21 WITH FRAME TITLE text-b21.
PARAMETERS: s_alvvar TYPE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b21.
SELECTION-SCREEN END OF BLOCK b20.

************************ Data Definitions ****************************
CLASS lcl_gui_tree DEFINITION INHERITING FROM cl_uac_gui_tree_base.
  PUBLIC SECTION.
    METHODS constructor
       IMPORTING
         !id_parent TYPE REF TO cl_gui_container
         !id_lifetime TYPE i OPTIONAL
         !id_name TYPE string OPTIONAL
         !id_style TYPE i OPTIONAL
         !id_class_id TYPE string OPTIONAL
         !id_tree_text TYPE uac_value
         !id_tree_style TYPE uac_style_id OPTIONAL
         !id_expand_no_children TYPE uac_flag DEFAULT true
         !id_multi_selection TYPE uac_flag DEFAULT false
       EXCEPTIONS
         cntl_error
         system_error
         error_cntl_create
         gui_type_not_supported .
    METHODS set_styles
      IMPORTING
        !it_node_style TYPE uac_t_node_style OPTIONAL
        !it_cell_style TYPE uac_t_cell_style OPTIONAL
        !it_column_style TYPE uac_t_column_style OPTIONAL
      EXCEPTIONS
        cntl_error .
    METHODS register_events
      IMPORTING
        !cell_selected TYPE uac_flag DEFAULT false
        !cell_double_click TYPE uac_flag DEFAULT false
        !cell_context_menu TYPE uac_flag DEFAULT false
        !cell_value_changed TYPE uac_flag DEFAULT false
        !cell_dropdown TYPE uac_flag DEFAULT false
        !node_selected TYPE uac_flag DEFAULT false
        !node_double_click TYPE uac_flag DEFAULT false
        !node_context_menu TYPE uac_flag DEFAULT false
        !node_value_changed TYPE uac_flag DEFAULT false
        !multi_selection_change TYPE uac_flag DEFAULT false
        !keypressed TYPE uac_flag DEFAULT false
        !column_double_click TYPE uac_flag DEFAULT false
        !column_context_menu TYPE uac_flag DEFAULT false
        !column_order_changed TYPE uac_flag DEFAULT false
        !sort_order_changed TYPE uac_flag DEFAULT false
        !group_context_menu TYPE uac_flag DEFAULT false
        !application_event TYPE uac_flag DEFAULT false
        !group_changes TYPE uac_flag DEFAULT false
        !control_context_menu TYPE uac_flag DEFAULT false
      EXCEPTIONS
        cntl_error .
    METHODS change_node_leaf
      IMPORTING
        !id_tree_id TYPE uac_tree_id DEFAULT cd_tree1_id
        !id_node_id TYPE uac_node_id
        !id_is_leaf TYPE uac_flag .
  PROTECTED SECTION.
    DATA md_tree_id TYPE uac_tree_id .
ENDCLASS.                    "lcl_gui_tree DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_alv_events_handle DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv_events_handle DEFINITION.
  PUBLIC SECTION.
    METHODS command     FOR EVENT added_function OF cl_salv_events       IMPORTING e_salv_function.
    METHODS doubleclick FOR EVENT double_click   OF cl_salv_events_table IMPORTING row column.
    METHODS linkclick   FOR EVENT link_click     OF cl_salv_events_table IMPORTING row column.
ENDCLASS.                    "lcl_alv_events_handle DEFINITION
*
CLASS lcl_tool_events_handle DEFINITION.
  PUBLIC SECTION.
    METHODS command     FOR EVENT function_selected OF cl_gui_toolbar IMPORTING fcode.
ENDCLASS.                    "lcl_tool_events_handle DEFINITION
*
CLASS lcl_tree_events_handle DEFINITION.
  PUBLIC SECTION.
    METHODS node_open         FOR EVENT open_folder_for_1st_time   OF lcl_gui_tree IMPORTING tree_id node_id.
    METHODS node_menu_request FOR EVENT node_context_menu_request  OF lcl_gui_tree IMPORTING tree_id node_id.
    METHODS node_menu_select  FOR EVENT node_context_menu_selected OF lcl_gui_tree IMPORTING tree_id node_id fcode.
ENDCLASS.                    "lcl_tree_events_handle DEFINITION
TYPE-POOLS: spta, vrm.
TYPES: BEGIN OF gtk_intfms.
        INCLUDE TYPE /pstech/bcintfms.
TYPES:  interface_type   TYPE /pstech/bcintfmm-interface_type,
        interface_dir    TYPE /pstech/bcintfmm-interface_dir,
        interface_desc   TYPE /pstech/bcintfmm-interface_desc,
        key1_datatype    TYPE /pstech/bcintfmm-key1_datatype,
        key2_datatype    TYPE /pstech/bcintfmm-key2_datatype,
        key3_datatype    TYPE /pstech/bcintfmm-key3_datatype,
        key4_datatype    TYPE /pstech/bcintfmm-key4_datatype.
TYPES:  intf_msgid_ico   TYPE c LENGTH 4,
        intf_istat_ico   TYPE c LENGTH 4, "interface stat
        intf_astat_ico   TYPE c LENGTH 4, "application stat
        intf_alog_ico    TYPE c LENGTH 4, "application log stat
        idoc_docnum      TYPE edidc-docnum,
        proxy_msgguid    TYPE sxmspmast-msgguid,
        proxy_pid        TYPE sxmspmast-pid,
        proxy_msgstate   TYPE sxmspmast-msgstate,
        proxy_eo_refid   TYPE sxmspmast-eo_refid,
        proxy_eo_refval  TYPE sxmspmast-eo_refval,
        proxy_exetimest  TYPE sxmspmast-exetimest,
        proxy_inittimest TYPE sxmspmast-inittimest,
        proxy_sendtimest TYPE sxmspmast-sendtimest,
        log_msgty        TYPE symsgty,
        log_msgid        TYPE symsgid,
        log_msgno        TYPE symsgno,
        log_msgtxt       TYPE c LENGTH 500,
       END OF gtk_intfms.
TYPES: gti_intfms TYPE STANDARD TABLE OF gtk_intfms.
TYPES: BEGIN OF gtk_log_table,
         log_ico         TYPE c LENGTH 4,
         log_msg         TYPE c LENGTH 500,
       END OF gtk_log_table.
TYPES: BEGIN OF gtk_parallel_data,
           import TYPE gtk_intfms OCCURS 0,
           export TYPE gtk_intfms OCCURS 0,
         END OF gtk_parallel_data.
TYPES: BEGIN OF gtk_parallel_struc,
           intfms TYPE gtk_intfms,
           xmle1  LIKE s_xmle1 OCCURS 0,
           xmlv1  LIKE s_xmlv1 OCCURS 0,
           xmle2  LIKE s_xmle2 OCCURS 0,
           xmlv2  LIKE s_xmlv2 OCCURS 0,
           xmlo1  TYPE c,
         END OF gtk_parallel_struc.

DATA: gi_log_table                TYPE STANDARD TABLE OF gtk_log_table.
DATA: go_main_alv_table           TYPE REF TO cl_salv_table,
      go_log_table                TYPE REF TO cl_salv_table.
DATA: go_proxy_xml                TYPE REF TO cl_proxy_xml_edit.
DATA: go_idoc_tree                TYPE REF TO lcl_gui_tree,
      go_proxy_tree               TYPE REF TO lcl_gui_tree.
DATA: go_main_container           TYPE REF TO cl_gui_custom_container.
DATA: go_main_alv_container       TYPE REF TO cl_gui_container,
      go_main_view_container      TYPE REF TO cl_gui_container,
      go_view_tool_container      TYPE REF TO cl_gui_container,
      go_view_obj_container       TYPE REF TO cl_gui_container.
DATA: go_main_split_container     TYPE REF TO cl_gui_splitter_container,
      go_view_split_container     TYPE REF TO cl_gui_splitter_container.
DATA: go_view_toolbar             TYPE REF TO cl_gui_toolbar.

DATA: gi_proxy_xml_button         TYPE ttb_button,
      gi_proxy_tree_button        TYPE ttb_button,
      gi_idoc_tree_button         TYPE ttb_button.

DATA: gi_proxy_tree_context       TYPE uac_t_context_menu,
      gi_idoc_tree_context        TYPE uac_t_context_menu.

DATA: gi_tree_search_result       TYPE uac_t_node_long.
DATA: gv_tree_search_index        TYPE sytabix.

DATA: gv_xml_document             TYPE xstring.

DATA: gi_intfms          TYPE STANDARD TABLE OF gtk_intfms.
DATA: gk_intfms_par      TYPE gtk_parallel_data.
DATA: gk_intfms          TYPE gtk_intfms.

INITIALIZATION.
  PERFORM screen_alvvar_default CHANGING s_alvvar.
  PERFORM data_display_settings.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_alvvar.
  PERFORM screen_alvvar_searchvalue CHANGING s_alvvar.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_intfid-low.
  PERFORM screen_intfid_searchvalue.

START-OF-SELECTION.
  PERFORM data_retrieve.

  CALL SCREEN 1001.

*&---------------------------------------------------------------------*
*&      Form  data_retrieve
*&---------------------------------------------------------------------*
FORM data_retrieve.
  DATA  li_intfms                TYPE STANDARD TABLE OF gtk_intfms.
  DATA  li_intfmm                TYPE STANDARD TABLE OF /pstech/bcintfmm.
  FIELD-SYMBOLS: <lk_intfms>     TYPE gtk_intfms.
  FIELD-SYMBOLS: <lk_intfmm>     TYPE /pstech/bcintfmm.

  SELECT * INTO TABLE li_intfmm FROM /pstech/bcintfmm
    WHERE interface_id    IN s_intfid   AND
          interface_type  IN s_intfty   AND
          interface_dir   IN s_intfdi.

  LOOP AT li_intfmm ASSIGNING <lk_intfmm>.
    AUTHORITY-CHECK OBJECT 'ZBC_INTF'
             ID '/PSTECH/II' FIELD <lk_intfmm>-interface_id
             ID 'ACTVT' FIELD '03'.
    IF sy-subrc NE 0.
      DELETE li_intfmm INDEX sy-tabix.
    ENDIF.
  ENDLOOP.
  SORT li_intfmm BY interface_id.

  IF li_intfmm[] IS NOT INITIAL.
    SELECT * UP TO s_intfmx ROWS
      INTO TABLE gi_intfms FROM /pstech/bcintfms
      FOR ALL ENTRIES IN li_intfmm
      WHERE interface_id    EQ li_intfmm-interface_id AND
            interface_key1  IN s_intfk1   AND
            interface_key2  IN s_intfk2   AND
            interface_key3  IN s_intfk3   AND
            interface_key4  IN s_intfk4   AND
            interface_msgid IN s_intfmi   AND
        ( ( execute_date    EQ s_exfrdt   AND
            execute_time    GE s_exfrtm ) OR
            execute_date    GT s_exfrdt ) AND
        ( ( execute_date    EQ s_extodt   AND
            execute_time    LE s_extotm ) OR
            execute_date    LT s_extodt ).
  ENDIF.

  LOOP AT gi_intfms ASSIGNING <lk_intfms>.
    READ TABLE li_intfmm ASSIGNING <lk_intfmm> WITH KEY interface_id = <lk_intfms>-interface_id BINARY SEARCH.
    IF sy-subrc = 0.
      <lk_intfms>-interface_type = <lk_intfmm>-interface_type.
      <lk_intfms>-interface_dir  = <lk_intfmm>-interface_dir.
      <lk_intfms>-interface_desc = <lk_intfmm>-interface_desc.
      <lk_intfms>-key1_datatype  = <lk_intfmm>-key1_datatype.
      <lk_intfms>-key2_datatype  = <lk_intfmm>-key2_datatype.
      <lk_intfms>-key3_datatype  = <lk_intfmm>-key3_datatype.
      <lk_intfms>-key4_datatype  = <lk_intfmm>-key4_datatype.
    ENDIF.

    CASE <lk_intfms>-interface_type.
      WHEN '0'.
        <lk_intfms>-proxy_msgguid  = <lk_intfms>-interface_msgid.
        <lk_intfms>-intf_msgid_ico = '@R4@'.
      WHEN '1'.
        <lk_intfms>-idoc_docnum = <lk_intfms>-interface_msgid.
        <lk_intfms>-intf_msgid_ico = '@G5@'.
    ENDCASE.
  ENDLOOP.

  IF s_parrfc IS INITIAL.
    PERFORM data_retrieve_serial.
  ELSE.
    PERFORM data_retrieve_parallel.
  ENDIF.

  SORT gi_intfms BY execute_date ASCENDING execute_time ASCENDING proxy_sendtimest ASCENDING.
ENDFORM.                    "data_retrieve
*&---------------------------------------------------------------------*
*&      Form  data_retrieve_parallel
*&---------------------------------------------------------------------*
FORM data_retrieve_parallel.
  gk_intfms_par-import[] = gi_intfms[].
  CALL FUNCTION 'SPTA_PARA_PROCESS_START_2'
    EXPORTING
      server_group             = s_parrfc
      max_no_of_tasks          = s_parmax
      before_rfc_callback_form = 'DATA_RETRIEVE_PARA_PIPELINING'
      in_rfc_callback_form     = 'DATA_RETRIEVE_PARA_PROCESS'
      after_rfc_callback_form  = 'DATA_RETRIEVE_PARA_SYNC'
      callback_prog            = sy-repid.
  gi_intfms[] = gk_intfms_par-export[].
ENDFORM.                    "data_retrieve_parallel
*&---------------------------------------------------------------------*
*&      Form  data_retrieve_para_pipelining
*&---------------------------------------------------------------------*
FORM data_retrieve_para_pipelining USING lk_before_rfc_imp  TYPE spta_t_before_rfc_imp
                               CHANGING lk_before_rfc_exp  TYPE spta_t_before_rfc_exp
                                        li_rfcdata         TYPE spta_t_indxtab
                                        li_object_failed   TYPE spta_t_failed_objects
                                        li_object_pending  TYPE spta_t_pending_objects
                                        lk_user_param.
  DATA: lk_intfms_par TYPE gtk_parallel_struc.

  IF gk_intfms_par-import[] IS INITIAL.
    CLEAR lk_before_rfc_exp-start_rfc. EXIT.
  ELSE.
    lk_before_rfc_exp-start_rfc = 'X'.
  ENDIF.

  READ TABLE gk_intfms_par-import INTO lk_intfms_par-intfms INDEX 1.
  lk_intfms_par-xmle1[] = s_xmle1[].
  lk_intfms_par-xmlv1[] = s_xmlv1[].
  lk_intfms_par-xmle2[] = s_xmle2[].
  lk_intfms_par-xmlv2[] = s_xmlv2[].
  lk_intfms_par-xmlo1   = s_xmlo1.
  DELETE gk_intfms_par-import INDEX 1.

  CALL FUNCTION 'SPTA_INDX_PACKAGE_ENCODE'
    EXPORTING
      data    = lk_intfms_par
    IMPORTING
      indxtab = li_rfcdata.

ENDFORM.                    "data_retrieve_para_pipelining
*&---------------------------------------------------------------------*
*&      Form  data_retrieve_para_process
*&---------------------------------------------------------------------*
FORM data_retrieve_para_process USING lk_in_rfc_imp  TYPE spta_t_in_rfc_imp
                            CHANGING lk_in_rfc_exp  TYPE spta_t_in_rfc_exp
                                     li_rfcdata     TYPE spta_t_indxtab.
  DATA: lk_intfms_par TYPE gtk_parallel_struc.
  DATA: lk_log_msg               TYPE bal_s_msg.
  DATA: lk_log_msghndl           TYPE balmsghndl.
  DATA  li_log_header            TYPE balhdr_t.
  DATA: lk_log_header            TYPE balhdr.
  DATA: lv_idoc_status           TYPE edi_status.
  DATA: lv_idoc_status_statva    TYPE edi_statva.
  DATA: lv_idoc_status_stalight  TYPE edi_slight.
  DATA: li_msg_payload           TYPE sxms_messagepayload_tab.
  DATA: lo_xms_persist_adm       TYPE REF TO cl_xms_persist_adm.
  DATA: li_xml_contents          TYPE srt_xml_data_tab.
  DATA: lv_xml_tagname           TYPE string,
        lv_xml_tagnamespace      TYPE string.
  DATA: lv_xml_filter1 TYPE c,
        lv_xml_filter2 TYPE c.
  FIELD-SYMBOLS: <lk_xml_contents> TYPE srt_xml_data.
  FIELD-SYMBOLS: <lk_msg_payload>  TYPE sxms_messagepayload.


  CALL FUNCTION 'SPTA_INDX_PACKAGE_DECODE'
    EXPORTING
      indxtab = li_rfcdata
    IMPORTING
      data    = lk_intfms_par.

  "Proxy
  CASE lk_intfms_par-intfms-interface_type .
    WHEN '0'.
      IF ( lk_intfms_par-xmle1 IS NOT INITIAL AND lk_intfms_par-xmlv1 IS NOT INITIAL ) OR
         ( lk_intfms_par-xmle2 IS NOT INITIAL AND lk_intfms_par-xmlv2 IS NOT INITIAL ).
        CREATE OBJECT lo_xms_persist_adm.
      ENDIF.
      SELECT SINGLE pid msgstate eo_refid eo_refval exetimest inittimest sendtimest FROM sxmspmast
        INTO (lk_intfms_par-intfms-proxy_pid,       lk_intfms_par-intfms-proxy_msgstate,   lk_intfms_par-intfms-proxy_eo_refid, lk_intfms_par-intfms-proxy_eo_refval,
              lk_intfms_par-intfms-proxy_exetimest, lk_intfms_par-intfms-proxy_inittimest, lk_intfms_par-intfms-proxy_sendtimest)
        WHERE msgguid = lk_intfms_par-intfms-proxy_msgguid.
      SELECT SINGLE icon_id FROM sxmsmstat INTO lk_intfms_par-intfms-intf_istat_ico WHERE msgstate = lk_intfms_par-intfms-proxy_msgstate.
      CLEAR: lv_xml_filter1, lv_xml_filter2.

      IF ( lk_intfms_par-xmle1 IS NOT INITIAL AND lk_intfms_par-xmlv1 IS NOT INITIAL ) OR
         ( lk_intfms_par-xmle2 IS NOT INITIAL AND lk_intfms_par-xmlv2 IS NOT INITIAL ).
        CLEAR: li_msg_payload[], li_xml_contents[].
        TRY.
            lo_xms_persist_adm->get_xi_payload(
              EXPORTING
                im_msgguid         = lk_intfms_par-intfms-proxy_msgguid
                im_pid             = lk_intfms_par-intfms-proxy_pid
                im_version         = '000'
              IMPORTING
                ex_messagepayload  = li_msg_payload ).
          CATCH cx_xms_syserr_persist.
        ENDTRY.

        READ TABLE li_msg_payload ASSIGNING <lk_msg_payload> INDEX 1.
        IF sy-subrc = 0.
          cl_soap_xml_parser=>get_data( EXPORTING xdoc = <lk_msg_payload>-payload  IMPORTING data = li_xml_contents ).
          LOOP AT li_xml_contents ASSIGNING <lk_xml_contents>.
            CLEAR: lv_xml_tagname, lv_xml_tagnamespace.
            SPLIT <lk_xml_contents>-tag_name AT ':' INTO lv_xml_tagnamespace lv_xml_tagname.
            IF lv_xml_tagname IS INITIAL. lv_xml_tagname = lv_xml_tagnamespace. ENDIF.
            IF ( lk_intfms_par-xmle1 IS NOT INITIAL AND lk_intfms_par-xmlv1 IS NOT INITIAL ) AND ( lv_xml_tagname IN lk_intfms_par-xmle1 AND <lk_xml_contents>-tag_value IN lk_intfms_par-xmlv1 ) .
              lv_xml_filter1 = 'X'.
            ENDIF.
            IF ( lk_intfms_par-xmle2 IS NOT INITIAL AND lk_intfms_par-xmlv2 IS NOT INITIAL ) AND ( lv_xml_tagname IN lk_intfms_par-xmle2 AND <lk_xml_contents>-tag_value IN lk_intfms_par-xmlv2 ) .
              lv_xml_filter2 = 'X'.
            ENDIF.
          ENDLOOP.
        ENDIF.

        IF ( lk_intfms_par-xmle1 IS NOT INITIAL AND lk_intfms_par-xmlv1 IS NOT INITIAL ) AND ( lk_intfms_par-xmle2 IS NOT INITIAL AND lk_intfms_par-xmlv2 IS NOT INITIAL ).
          IF lk_intfms_par-xmlo1 = '1'. "Or
            IF lv_xml_filter1 IS NOT INITIAL OR  lv_xml_filter2 IS NOT INITIAL. ELSE. CLEAR lk_intfms_par-intfms. ENDIF.
          ELSE.
            IF lv_xml_filter1 IS NOT INITIAL AND lv_xml_filter2 IS NOT INITIAL. ELSE. CLEAR lk_intfms_par-intfms. ENDIF.
          ENDIF.
        ELSE.
          IF ( lk_intfms_par-xmle1 IS NOT INITIAL AND lk_intfms_par-xmlv1 IS NOT INITIAL ) AND lv_xml_filter1 IS INITIAL.
            CLEAR lk_intfms_par-intfms.
          ENDIF.
          IF ( lk_intfms_par-xmle2 IS NOT INITIAL AND lk_intfms_par-xmlv2 IS NOT INITIAL ) AND lv_xml_filter2 IS INITIAL.
            CLEAR lk_intfms_par-intfms.
          ENDIF.
        ENDIF.
      ENDIF.

    WHEN '1'.
      "IDoc
      SELECT SINGLE status   INTO lv_idoc_status          FROM edidc    WHERE docnum = lk_intfms_par-intfms-idoc_docnum.
      SELECT SINGLE statva   INTO lv_idoc_status_statva   FROM teds3    WHERE status = lv_idoc_status.
      SELECT SINGLE stalight INTO lv_idoc_status_stalight FROM stalight WHERE statva = lv_idoc_status_statva.
      CASE lv_idoc_status_stalight.
        WHEN '1'.
          lk_intfms_par-intfms-intf_istat_ico = '@5D@'.
        WHEN '2'.
          lk_intfms_par-intfms-intf_istat_ico = '@5B@'.
        WHEN '3'.
          lk_intfms_par-intfms-intf_istat_ico = '@5C@'.
      ENDCASE.
  ENDCASE.

  "Logs
  IF lk_intfms_par-intfms-lognumber IS NOT INITIAL.
    SELECT SINGLE * INTO lk_log_header FROM balhdr WHERE lognumber = lk_intfms_par-intfms-lognumber.
    CALL FUNCTION 'BAL_LOG_EXIST'
      EXPORTING
        i_log_handle  = lk_log_header-log_handle
      EXCEPTIONS
        log_not_found = 1.
    IF sy-subrc NE 0.
      APPEND lk_log_header TO li_log_header.
      CALL FUNCTION 'BAL_DB_LOAD'
        EXPORTING
          i_t_log_header = li_log_header
        EXCEPTIONS
          OTHERS         = 0.
    ENDIF.

    lk_log_msghndl-log_handle = lk_log_header-log_handle.
    lk_log_msghndl-msgnumber  = lk_log_header-last_msgnr.

    IF lk_log_header-last_msgnr > 1.
      lk_intfms_par-intfms-intf_alog_ico = '@DH@'.
    ENDIF.

    CALL FUNCTION 'BAL_LOG_MSG_READ'
      EXPORTING
        i_s_msg_handle = lk_log_msghndl
      IMPORTING
        e_s_msg        = lk_log_msg
      EXCEPTIONS
        log_not_found  = 1
        msg_not_found  = 2
        OTHERS         = 3.
    CASE lk_log_msg-msgty.
      WHEN 'A' OR 'E'. lk_intfms_par-intfms-intf_astat_ico = '@5C@'.
      WHEN 'I' OR 'S'. lk_intfms_par-intfms-intf_astat_ico = '@5B@'.
      WHEN 'W'       . lk_intfms_par-intfms-intf_astat_ico = '@5D@'.
    ENDCASE.
    MESSAGE ID lk_log_msg-msgid TYPE lk_log_msg-msgty NUMBER lk_log_msg-msgno WITH lk_log_msg-msgv1 lk_log_msg-msgv2 lk_log_msg-msgv3 lk_log_msg-msgv4 INTO lk_intfms_par-intfms-log_msgtxt.
  ENDIF.

  CALL FUNCTION 'SPTA_INDX_PACKAGE_ENCODE'
    EXPORTING
      data    = lk_intfms_par-intfms
    IMPORTING
      indxtab = li_rfcdata.

ENDFORM.                    "data_retrieve_para_process
*&---------------------------------------------------------------------*
*&      Form  data_retrieve_para_sync
*&---------------------------------------------------------------------*
FORM data_retrieve_para_sync USING li_rfcdata         TYPE spta_t_indxtab
                                  lv_rfcsubrc        TYPE sy-subrc
                                  lv_rfcmsg          TYPE spta_t_rfcmsg
                                  li_object_pending  TYPE spta_t_pending_objects
                                  lk_after_rfc_imp   TYPE spta_t_after_rfc_imp
                         CHANGING lk_after_rfc_exp   TYPE spta_t_after_rfc_exp
                                  lv_user_param.
  DATA: lk_intfms TYPE gtk_intfms.

  CALL FUNCTION 'SPTA_INDX_PACKAGE_DECODE'
    EXPORTING
      indxtab = li_rfcdata
    IMPORTING
      data    = lk_intfms.

  IF lk_intfms IS NOT INITIAL.
    APPEND lk_intfms TO gk_intfms_par-export.
  ENDIF.

ENDFORM.                    "data_retrieve_par_sync
*&---------------------------------------------------------------------*
*&      Form  data_retrieve_serial
*&---------------------------------------------------------------------*
FORM data_retrieve_serial.
  TYPES: BEGIN OF ltk_proxy_info,
           msgguid                TYPE sxmspmast-msgguid,
           pid                    TYPE sxmspmast-pid,
           msgstate               TYPE sxmspmast-msgstate,
           eo_refid               TYPE sxmspmast-eo_refid,
           eo_refval              TYPE sxmspmast-eo_refval,
           exetimest              TYPE sxmspmast-exetimest,
           inittimest             TYPE sxmspmast-inittimest,
           sendtimest             TYPE sxmspmast-sendtimest,
          END OF ltk_proxy_info.
  TYPES: BEGIN OF ltk_idoc_info,
          docnum                 TYPE edidc-docnum,
          status                 TYPE edidc-status,
          END OF ltk_idoc_info.

  DATA: lk_log_msg               TYPE bal_s_msg.
  DATA: lk_log_msghndl           TYPE balmsghndl.
  DATA  li_log_header            TYPE balhdr_t.
  DATA: li_log_handle            TYPE bal_t_logh,
        li_log_loaded            TYPE bal_t_logh.
  DATA  li_intfms                TYPE STANDARD TABLE OF gtk_intfms.
  DATA  li_proxy_info            TYPE STANDARD TABLE OF ltk_proxy_info.
  DATA  li_edidc                 TYPE STANDARD TABLE OF ltk_idoc_info.
  DATA  li_intfmm                TYPE STANDARD TABLE OF /pstech/bcintfmm.
  DATA: lv_idoc_status_statva    TYPE edi_statva.
  DATA: lv_idoc_status_stalight  TYPE edi_slight.
  DATA: li_msg_payload            TYPE sxms_messagepayload_tab.
  DATA: lo_xms_persist_adm       TYPE REF TO cl_xms_persist_adm.
  DATA: li_xml_contents           TYPE srt_xml_data_tab.
  DATA: lv_xml_tagname            TYPE string,
        lv_xml_tagnamespace       TYPE string.
  DATA: lv_xml_filter1 TYPE c,
        lv_xml_filter2 TYPE c.
  FIELD-SYMBOLS: <lk_intfms>     TYPE gtk_intfms.
  FIELD-SYMBOLS: <lk_intfmm>     TYPE /pstech/bcintfmm.
  FIELD-SYMBOLS: <lk_proxy_info> TYPE ltk_proxy_info.
  FIELD-SYMBOLS: <lk_idoc_info>  TYPE ltk_idoc_info.
  FIELD-SYMBOLS: <lk_log_header> TYPE balhdr.
  FIELD-SYMBOLS: <lk_xml_contents> TYPE srt_xml_data.
  FIELD-SYMBOLS: <lk_msg_payload>  TYPE sxms_messagepayload.

  "Proxy
  li_intfms[] = gi_intfms[].
  DELETE li_intfms WHERE interface_type NE '0'.
  DELETE gi_intfms WHERE interface_type EQ '0'.
  IF li_intfms[] IS NOT INITIAL.
    IF ( s_xmle1 IS NOT INITIAL AND s_xmlv1 IS NOT INITIAL ) OR
       ( s_xmle2 IS NOT INITIAL AND s_xmlv2 IS NOT INITIAL ).
      CREATE OBJECT lo_xms_persist_adm.
    ENDIF.
    SELECT msgguid pid msgstate eo_refid eo_refval exetimest inittimest sendtimest FROM sxmspmast INTO TABLE li_proxy_info FOR ALL ENTRIES IN li_intfms
      WHERE msgguid = li_intfms-proxy_msgguid.
    LOOP AT li_intfms ASSIGNING <lk_intfms>.
      CLEAR: lv_xml_filter1, lv_xml_filter2.
      READ TABLE li_proxy_info ASSIGNING <lk_proxy_info> WITH KEY msgguid = <lk_intfms>-proxy_msgguid.
      IF sy-subrc = 0.
        SELECT SINGLE icon_id FROM sxmsmstat INTO <lk_intfms>-intf_istat_ico WHERE msgstate = <lk_proxy_info>-msgstate.
        <lk_intfms>-proxy_pid        = <lk_proxy_info>-pid.
        <lk_intfms>-proxy_msgstate   = <lk_proxy_info>-msgstate.
        <lk_intfms>-proxy_eo_refid   = <lk_proxy_info>-eo_refid.
        <lk_intfms>-proxy_eo_refval  = <lk_proxy_info>-eo_refval.
        <lk_intfms>-proxy_exetimest  = <lk_proxy_info>-exetimest.
        <lk_intfms>-proxy_inittimest = <lk_proxy_info>-inittimest.
        <lk_intfms>-proxy_sendtimest = <lk_proxy_info>-sendtimest.

        "XML filters
        IF ( s_xmle1 IS NOT INITIAL AND s_xmlv1 IS NOT INITIAL ) OR
           ( s_xmle2 IS NOT INITIAL AND s_xmlv2 IS NOT INITIAL ).
          CLEAR: li_msg_payload[], li_xml_contents[].
          TRY.
              lo_xms_persist_adm->get_xi_payload(
                EXPORTING
                  im_msgguid         = <lk_intfms>-proxy_msgguid
                  im_pid             = <lk_intfms>-proxy_pid
                  im_version         = '000'
                IMPORTING
                  ex_messagepayload  = li_msg_payload ).
            CATCH cx_xms_syserr_persist.
          ENDTRY.

          READ TABLE li_msg_payload ASSIGNING <lk_msg_payload> INDEX 1.
          IF sy-subrc = 0.

            cl_soap_xml_parser=>get_data( EXPORTING xdoc = <lk_msg_payload>-payload  IMPORTING data = li_xml_contents ).
            LOOP AT li_xml_contents ASSIGNING <lk_xml_contents>.
              CLEAR: lv_xml_tagname, lv_xml_tagnamespace.
              SPLIT <lk_xml_contents>-tag_name AT ':' INTO lv_xml_tagnamespace lv_xml_tagname.
              IF lv_xml_tagname IS INITIAL. lv_xml_tagname = lv_xml_tagnamespace. ENDIF.

              IF ( s_xmle1 IS NOT INITIAL AND s_xmlv1 IS NOT INITIAL ) AND ( lv_xml_tagname IN s_xmle1 AND <lk_xml_contents>-tag_value IN s_xmlv1 ) .
                lv_xml_filter1 = 'X'.
              ENDIF.
              IF ( s_xmle2 IS NOT INITIAL AND s_xmlv2 IS NOT INITIAL ) AND ( lv_xml_tagname IN s_xmle2 AND <lk_xml_contents>-tag_value IN s_xmlv2 ) .
                lv_xml_filter2 = 'X'.
              ENDIF..
            ENDLOOP.
          ENDIF.

          IF     ( s_xmle1 IS NOT INITIAL AND s_xmlv1 IS NOT INITIAL ) AND ( s_xmle2 IS NOT INITIAL AND s_xmlv2 IS NOT INITIAL ) .
            IF     s_xmlo1 = '1'. "Or
              IF lv_xml_filter1 IS NOT INITIAL OR lv_xml_filter2 IS NOT INITIAL.   APPEND <lk_intfms> TO gi_intfms.  ENDIF.
            ELSE.
              IF lv_xml_filter1 IS NOT INITIAL AND lv_xml_filter2 IS NOT INITIAL.  APPEND <lk_intfms> TO gi_intfms.  ENDIF.
            ENDIF.
          ELSE.
            IF ( s_xmle1 IS NOT INITIAL AND s_xmlv1 IS NOT INITIAL ) AND lv_xml_filter1 IS NOT INITIAL.
              APPEND <lk_intfms> TO gi_intfms.
            ENDIF.
            IF ( s_xmle2 IS NOT INITIAL AND s_xmlv2 IS NOT INITIAL ) AND lv_xml_filter2 IS NOT INITIAL.
              APPEND <lk_intfms> TO gi_intfms.
            ENDIF.
          ENDIF.
        ELSE.
          APPEND <lk_intfms> TO gi_intfms.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  "IDoc
  li_intfms[] = gi_intfms[].
  DELETE li_intfms WHERE interface_type NE '1'.
  IF li_intfms[] IS NOT INITIAL.
    SELECT docnum status FROM edidc INTO TABLE li_edidc FOR ALL ENTRIES IN li_intfms WHERE docnum = li_intfms-idoc_docnum.
    LOOP AT gi_intfms ASSIGNING <lk_intfms> WHERE interface_type EQ '1'.
      READ TABLE li_edidc ASSIGNING <lk_idoc_info> WITH KEY docnum = <lk_intfms>-idoc_docnum.
      IF sy-subrc = 0.
        SELECT SINGLE statva   INTO lv_idoc_status_statva   FROM teds3    WHERE status = <lk_idoc_info>-status.
        SELECT SINGLE stalight INTO lv_idoc_status_stalight FROM stalight WHERE statva = lv_idoc_status_statva.
        CASE lv_idoc_status_stalight.
          WHEN '1'.
            <lk_intfms>-intf_istat_ico = '@5D@'.
          WHEN '2'.
            <lk_intfms>-intf_istat_ico = '@5B@'.
          WHEN '3'.
            <lk_intfms>-intf_istat_ico = '@5C@'.
        ENDCASE.
      ENDIF.
    ENDLOOP.
  ENDIF.

  "Logs
  li_intfms[] = gi_intfms[].
  DELETE li_intfms WHERE lognumber IS INITIAL.
  IF li_intfms[] IS NOT INITIAL.
    SELECT * INTO TABLE li_log_header FROM balhdr FOR ALL ENTRIES IN li_intfms WHERE lognumber = li_intfms-lognumber.
    LOOP AT li_log_header ASSIGNING <lk_log_header>.
      CALL FUNCTION 'BAL_LOG_EXIST'
        EXPORTING
          i_log_handle  = <lk_log_header>-log_handle
        EXCEPTIONS
          log_not_found = 1.
      IF sy-subrc = 0.
        DELETE li_log_header.
      ENDIF.
    ENDLOOP.

    "load logs from database
    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_log_header = li_log_header
      EXCEPTIONS
        OTHERS         = 0.
  ENDIF.

  LOOP AT gi_intfms ASSIGNING <lk_intfms> WHERE lognumber IS NOT INITIAL.
    READ TABLE li_log_header ASSIGNING <lk_log_header> WITH TABLE KEY mandant = sy-mandt lognumber = <lk_intfms>-lognumber.
    IF sy-subrc = 0.
      lk_log_msghndl-log_handle = <lk_log_header>-log_handle.
      lk_log_msghndl-msgnumber  = <lk_log_header>-last_msgnr.

      IF <lk_log_header>-last_msgnr > 1.
        <lk_intfms>-intf_alog_ico = '@DH@'.
      ENDIF.

      CALL FUNCTION 'BAL_LOG_MSG_READ'
        EXPORTING
          i_s_msg_handle = lk_log_msghndl
        IMPORTING
          e_s_msg        = lk_log_msg
        EXCEPTIONS
          log_not_found  = 1
          msg_not_found  = 2
          OTHERS         = 3.
      CASE lk_log_msg-msgty.
        WHEN 'A' OR 'E'. <lk_intfms>-intf_astat_ico = '@5C@'.
        WHEN 'I' OR 'S'. <lk_intfms>-intf_astat_ico = '@5B@'.
        WHEN 'W'       . <lk_intfms>-intf_astat_ico = '@5D@'.
      ENDCASE.
      MESSAGE ID lk_log_msg-msgid TYPE lk_log_msg-msgty NUMBER lk_log_msg-msgno WITH lk_log_msg-msgv1 lk_log_msg-msgv2 lk_log_msg-msgv3 lk_log_msg-msgv4 INTO <lk_intfms>-log_msgtxt.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "data_retrieve_serial
*&---------------------------------------------------------------------*
*&      Form  data_display_main_alv_config
*&---------------------------------------------------------------------*
FORM data_display_main_alv_config.
  DATA lo_alv_functions      TYPE REF TO cl_salv_functions.
  DATA lo_alv_columns        TYPE REF TO cl_salv_columns_table.
  DATA lo_alv_column         TYPE REF TO cl_salv_column_table.
  DATA lo_alv_layout         TYPE REF TO cl_salv_layout.
  DATA lo_alv_events         TYPE REF TO cl_salv_events_table.
  DATA lo_alv_selections     TYPE REF TO cl_salv_selections.
  DATA lo_alv_events_handle  TYPE REF TO lcl_alv_events_handle.
  DATA lo_alv_functions_list TYPE REF TO cl_salv_functions_list.
  DATA lk_alv_layout_key     TYPE salv_s_layout_key.
  DATA lo_elem_type          TYPE REF TO cl_abap_elemdescr.
  DATA lk_elem_ddic          TYPE dfies.
  DATA lo_msg                TYPE REF TO cx_root.
  DATA lv_msg                TYPE string.
  DATA: lv_alv_longtext TYPE scrtext_l.
  DATA: lv_alv_medtext  TYPE scrtext_m.
  FIELD-SYMBOLS: <lk_intfms> TYPE gtk_intfms.

  TRY.
      lo_alv_functions_list  = go_main_alv_table->get_functions( ).
      lo_alv_functions_list->set_group_export( ).
      lo_alv_functions_list->set_group_filter( ).
      lo_alv_functions_list->set_group_layout( ).
      lo_alv_functions_list->set_group_sort( ).
      lo_alv_functions_list->set_find( ).
      lo_alv_functions_list->set_graphics( abap_false ).
      lo_alv_functions_list->set_find_more( ).

      lo_alv_selections = go_main_alv_table->get_selections( ).
      lo_alv_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

      lo_alv_functions_list->add_function(
        name     = 'DELE'
        icon     = '@11@'
        tooltip  = 'Delete Index'
        position = if_salv_c_function_position=>right_of_salv_functions ).

      lo_alv_functions_list->add_function(
        name     = 'COPYPROCESS'
        icon     = '@2U@'
        text     = 'Copy'
        tooltip  = 'Copy and Process Messages'
        position = if_salv_c_function_position=>right_of_salv_functions ).

      lo_alv_functions_list->add_function(
        name     = 'RESTART'
        icon     = '@5Y@'
        text     = 'Restart'
        tooltip  = 'Restart Error Messages'
        position = if_salv_c_function_position=>right_of_salv_functions ).

      "Columns
      lo_alv_columns = go_main_alv_table->get_columns( ).
      lo_alv_columns->set_optimize( abap_true ).

      lo_alv_column ?= lo_alv_columns->get_column( 'INTF_MSGID_ICO' ).
      lo_alv_column->set_short_text( text-ks7 ).
      lo_alv_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
      lo_alv_column->set_icon( if_salv_c_bool_sap=>true ).

      lo_alv_column ?= lo_alv_columns->get_column( 'INTF_ASTAT_ICO' ).
      lo_alv_column->set_short_text( text-kt2 ).

      lo_alv_column ?= lo_alv_columns->get_column( 'LOG_MSGTXT' ).
      lo_alv_column->set_short_text( text-kt1 ).

      lo_alv_column ?= lo_alv_columns->get_column( 'INTF_ISTAT_ICO' ).
      lo_alv_column->set_short_text( text-kt3 ).


      lo_alv_column ?= lo_alv_columns->get_column( 'INTF_ALOG_ICO' ).
      lo_alv_column->set_short_text( text-ks8 ).
      lo_alv_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
      lo_alv_column->set_icon( if_salv_c_bool_sap=>true ).

      lo_alv_column ?= lo_alv_columns->get_column( 'EXECUTE_DATE' ).
      lo_alv_column->set_long_text( text-kl5 ).
      lo_alv_column->set_medium_text( text-k05 ).
      lo_alv_column->set_short_text( text-ks5 ).

      lo_alv_column ?= lo_alv_columns->get_column( 'EXECUTE_TIME' ).
      lo_alv_column->set_long_text( text-kl6 ).
      lo_alv_column->set_medium_text( text-k06 ).
      lo_alv_column->set_short_text( text-ks6 ).


      READ TABLE gi_intfms ASSIGNING <lk_intfms> INDEX 1.
      IF sy-subrc = 0.
        lo_alv_column ?= lo_alv_columns->get_column( 'INTERFACE_KEY1' ).
        lo_alv_column->set_long_text( text-k01 ).
        lo_alv_column->set_medium_text( text-k01 ).
        lo_alv_column->set_short_text( text-k01 ).
        IF <lk_intfms>-key1_datatype IS NOT INITIAL.
          lo_elem_type ?= cl_abap_elemdescr=>describe_by_name( <lk_intfms>-key1_datatype ).
          lk_elem_ddic = lo_elem_type->get_ddic_field( ).
          lo_alv_column->set_long_text( lk_elem_ddic-scrtext_l ).
          lo_alv_column->set_medium_text( lk_elem_ddic-scrtext_m ).
          lo_alv_column->set_short_text( lk_elem_ddic-scrtext_s ).
        ENDIF.

        lo_alv_column ?= lo_alv_columns->get_column( 'INTERFACE_KEY2' ).
        lo_alv_column->set_long_text( text-k02 ).
        lo_alv_column->set_medium_text( text-k02 ).
        lo_alv_column->set_short_text( text-k02 ).
        IF <lk_intfms>-key2_datatype IS NOT INITIAL.
          lo_elem_type ?= cl_abap_elemdescr=>describe_by_name( <lk_intfms>-key2_datatype ).
          lk_elem_ddic = lo_elem_type->get_ddic_field( ).
          lo_alv_column->set_long_text( lk_elem_ddic-scrtext_l ).
          lo_alv_column->set_medium_text( lk_elem_ddic-scrtext_m ).
          lo_alv_column->set_short_text( lk_elem_ddic-scrtext_s ).
        ENDIF.

        lo_alv_column ?= lo_alv_columns->get_column( 'INTERFACE_KEY3' ).
        lo_alv_column->set_long_text( text-k03 ).
        lo_alv_column->set_medium_text( text-k03 ).
        lo_alv_column->set_short_text( text-k03 ).
        IF <lk_intfms>-key3_datatype IS NOT INITIAL.
          lo_elem_type ?= cl_abap_elemdescr=>describe_by_name( <lk_intfms>-key3_datatype ).
          lk_elem_ddic = lo_elem_type->get_ddic_field( ).
          lo_alv_column->set_long_text( lk_elem_ddic-scrtext_l ).
          lo_alv_column->set_medium_text( lk_elem_ddic-scrtext_m ).
          lo_alv_column->set_short_text( lk_elem_ddic-scrtext_s ).
        ENDIF.

        lo_alv_column ?= lo_alv_columns->get_column( 'INTERFACE_KEY4' ).
        lo_alv_column->set_long_text( text-k04 ).
        lo_alv_column->set_medium_text( text-k04 ).
        lo_alv_column->set_short_text( text-k04 ).
        IF <lk_intfms>-key4_datatype IS NOT INITIAL.
          lo_elem_type ?= cl_abap_elemdescr=>describe_by_name( <lk_intfms>-key4_datatype ).
          lk_elem_ddic = lo_elem_type->get_ddic_field( ).
          lo_alv_column->set_long_text( lk_elem_ddic-scrtext_l ).
          lo_alv_column->set_medium_text( lk_elem_ddic-scrtext_m ).
          lo_alv_column->set_short_text( lk_elem_ddic-scrtext_s ).
        ENDIF.
      ENDIF.

      "Layouts
      lo_alv_layout = go_main_alv_table->get_layout( ).
      lk_alv_layout_key-report = sy-repid.
      lo_alv_layout->set_key( lk_alv_layout_key ).
      lo_alv_layout->set_save_restriction( lo_alv_layout->restrict_none ).
      lo_alv_layout->set_default( abap_true ).
      IF s_alvvar IS NOT INITIAL.
        lo_alv_layout->set_initial_layout( s_alvvar ).
      ENDIF.

      "Events
      lo_alv_events = go_main_alv_table->get_event( ).
      CREATE OBJECT lo_alv_events_handle.
      SET HANDLER lo_alv_events_handle->command     FOR lo_alv_events.
      SET HANDLER lo_alv_events_handle->doubleclick FOR lo_alv_events.
      SET HANDLER lo_alv_events_handle->linkclick   FOR lo_alv_events.

      go_main_alv_table->display( ).
    CATCH cx_salv_not_found cx_salv_wrong_call cx_salv_existing.
  ENDTRY.

ENDFORM.                    "data_display_alv_settings
*&---------------------------------------------------------------------*
*&      Form  data_display_proxy_tree
*&---------------------------------------------------------------------*
FORM data_display_proxy_tree USING lk_intfms TYPE gtk_intfms.
  DATA: li_proxy_toolbar_event    TYPE cntl_simple_events,
        lk_proxy_toolbar_event    TYPE cntl_simple_event.
  DATA: lo_proxy_toolbar_handle   TYPE REF TO lcl_tool_events_handle.
  DATA: li_msg_payload            TYPE sxms_messagepayload_tab.
  DATA: lo_xms_persist_adm        TYPE REF TO cl_xms_persist_adm.
  DATA: li_proxy_tree_button      TYPE ttb_button.
  DATA: lo_proxy_tree_handle TYPE REF TO lcl_tree_events_handle.
  FIELD-SYMBOLS: <lk_msg_payload> TYPE sxms_messagepayload.

  CLEAR: gi_tree_search_result, gv_tree_search_index.


  "Clear off other objects
  IF go_log_table IS NOT INITIAL OR go_proxy_xml IS NOT INITIAL OR go_idoc_tree IS NOT INITIAL.
    go_main_split_container->remove_control( row = 1 column = 2 ).
    FREE: go_main_view_container, go_log_table, go_proxy_xml, go_idoc_tree, go_proxy_tree.
  ENDIF.

  "Create view container
  IF go_main_view_container IS INITIAL.
    go_main_view_container = go_main_split_container->get_container( row = 1 column = 2 ).
    CLEAR: go_view_split_container, go_view_tool_container, go_view_obj_container, go_view_toolbar.
  ENDIF.

  "Create split toobar & objects, then set Toolbars
  IF go_view_split_container IS INITIAL.
    CREATE OBJECT go_view_split_container
      EXPORTING
        parent  = go_main_view_container
        rows    = 2
        columns = 1.
    go_view_split_container->set_row_mode(   mode = cl_gui_splitter_container=>mode_absolute ).
    go_view_split_container->set_row_sash(   id = 1 type = cl_gui_splitter_container=>type_sashvisible value = cl_gui_splitter_container=>false ).
    go_view_split_container->set_row_sash(   id = 1 type = cl_gui_splitter_container=>type_movable     value = cl_gui_splitter_container=>false ).
    go_view_split_container->set_row_height( id = 1 height = 24 ).
    go_view_split_container->set_border( border = ' ' ).
    go_view_tool_container  = go_view_split_container->get_container( row = 1 column = 1 ).
    go_view_obj_container  = go_view_split_container->get_container( row = 2 column = 1 ).

    CREATE OBJECT go_view_toolbar
      EXPORTING
        parent = go_view_tool_container.
    li_proxy_tree_button[] = gi_proxy_tree_button[].
    go_view_toolbar->add_button_group( li_proxy_tree_button ).
    lk_proxy_toolbar_event-eventid = cl_gui_toolbar=>m_id_function_selected.  lk_proxy_toolbar_event-appl_event = ' '.
    APPEND lk_proxy_toolbar_event TO li_proxy_toolbar_event.
    go_view_toolbar->set_registered_events( li_proxy_toolbar_event ).
    CREATE OBJECT lo_proxy_toolbar_handle.
    SET HANDLER lo_proxy_toolbar_handle->command     FOR go_view_toolbar.
  ENDIF.

  "Reinitialize when selected again.
  IF go_proxy_tree IS NOT INITIAL.
    go_proxy_tree->free( ).
    CLEAR go_proxy_tree.
  ENDIF.

  "Create Tree
  IF go_proxy_tree IS INITIAL.
    CREATE OBJECT go_proxy_tree
      EXPORTING
        id_parent    = go_view_obj_container
        id_tree_text = 'XML'.
    PERFORM data_display_proxy_tree_config.
    go_proxy_tree->register_node_ctxtmn_requested( ).
    go_proxy_tree->register_node_ctxtmn_selected( ).
    go_proxy_tree->register_node_1st_open( ).

    CREATE OBJECT lo_proxy_tree_handle.
    AUTHORITY-CHECK OBJECT 'ZBC_INTF'
             ID '/PSTECH/II' FIELD lk_intfms-interface_id
             ID 'ACTVT' FIELD '02'.
    IF sy-subrc = 0.
      SET HANDLER lo_proxy_tree_handle->node_menu_request     FOR go_proxy_tree.
      SET HANDLER lo_proxy_tree_handle->node_menu_select      FOR go_proxy_tree.
    ENDIF.
    SET HANDLER lo_proxy_tree_handle->node_open             FOR go_proxy_tree.
  ENDIF.

  IF gv_xml_document IS INITIAL.
    "Retrieve XML
    CREATE OBJECT lo_xms_persist_adm.
    TRY.
        lo_xms_persist_adm->get_xi_payload(
          EXPORTING
            im_msgguid         = lk_intfms-proxy_msgguid
            im_pid             = lk_intfms-proxy_pid
            im_version         = '000'
          IMPORTING
            ex_messagepayload  = li_msg_payload ).
      CATCH cx_xms_syserr_persist.
    ENDTRY.

    "Retrieve Payload & show XML
    READ TABLE li_msg_payload ASSIGNING <lk_msg_payload> INDEX 1.
    IF sy-subrc = 0.
      gk_intfms = lk_intfms.
      PERFORM data_display_proxy_xml2tree USING <lk_msg_payload>-payload CHANGING go_proxy_tree.
    ENDIF.
  ELSE.
    PERFORM data_display_proxy_xml2tree USING gv_xml_document CHANGING go_proxy_tree.
  ENDIF.

ENDFORM.                    "data_display_proxy_tree
*&---------------------------------------------------------------------*
*&      Form  data_display_proxy_tree2xml
*&---------------------------------------------------------------------*
FORM data_display_proxy_tree2xml USING lo_tree TYPE REF TO lcl_gui_tree
                              CHANGING lv_xml  TYPE xstring.
  TYPES: BEGIN OF ltk_xml_node,
          tree_node TYPE uac_node_id,
          xml_node  TYPE REF TO if_ixml_mini_node,
          value     TYPE string,
         END OF ltk_xml_node.
  DATA: li_tree_nodes      TYPE uac_t_node_long,
        li_tree_nodes_sort TYPE uac_t_node_long.

  DATA: lk_tree_cell  TYPE uac_s_cell_long.
  DATA: li_xml_nodes  TYPE STANDARD TABLE OF ltk_xml_node.
  DATA: lk_xml_node   TYPE ltk_xml_node.
  DATA: lv_xml_node_name  TYPE string,
        lv_xml_node_value TYPE string.
  FIELD-SYMBOLS: <lk_tree_node>       TYPE uac_s_node_long.
  FIELD-SYMBOLS: <lk_tree_node_sort>  TYPE uac_s_node_long.
  FIELD-SYMBOLS: <lk_xml_node>        TYPE ltk_xml_node.
  FIELD-SYMBOLS: <lv_xml_node_value>  TYPE any.

  DATA lv_string TYPE string.
  DATA: lo_ixml_dom         TYPE REF TO if_ixml_mini_dom.
  DATA: lo_ixml_render      TYPE REF TO if_ixml_mini_renderer.
  DATA: lo_ixml_node        TYPE REF TO if_ixml_mini_node.

  lo_ixml_dom = cl_ixml_mini=>create_dom( ).
  lo_ixml_render = cl_ixml_mini=>create_renderer( ).

  li_tree_nodes = lo_tree->get_nodes( ).
  PERFORM  data_display_tree_sort USING '0' li_tree_nodes CHANGING li_tree_nodes_sort.
  LOOP AT li_tree_nodes_sort ASSIGNING <lk_tree_node>.
    CLEAR lk_xml_node.
    READ TABLE li_xml_nodes ASSIGNING <lk_xml_node> WITH KEY tree_node = <lk_tree_node>-parent_node.
    IF sy-subrc = 0.
      lo_tree->get_cell( EXPORTING id_node_id = <lk_tree_node>-node_id id_column_id = '1' IMPORTING es_cell = lk_tree_cell ).
      ASSIGN lk_tree_cell-dref_value->* TO <lv_xml_node_value>.
      lv_xml_node_name = <lk_tree_node>-value.
      lv_xml_node_value = <lv_xml_node_value>.

      lk_xml_node-tree_node = <lk_tree_node>-node_id.
      CASE <lk_tree_node>-style_id.
        WHEN 'XML_STRUC'.
          lo_ixml_dom->add_element( EXPORTING name = lv_xml_node_name parent = <lk_xml_node>-xml_node  IMPORTING new_node = lk_xml_node-xml_node ).
        WHEN 'XML_ELEM'.
          lo_ixml_dom->add_element( EXPORTING name = lv_xml_node_name parent = <lk_xml_node>-xml_node  IMPORTING new_node = lk_xml_node-xml_node ).
          lk_xml_node-value = lv_xml_node_value.
        WHEN 'XML_ATTR'.
          lo_ixml_dom->add_attribute( EXPORTING name = lv_xml_node_name value = lv_xml_node_value parent = <lk_xml_node>-xml_node ).
      ENDCASE.

      APPEND lk_xml_node TO li_xml_nodes.
    ELSE.
      lv_xml_node_name = <lk_tree_node>-value.
      lk_xml_node-tree_node = <lk_tree_node>-node_id.
      lo_ixml_dom->add_element( EXPORTING name = lv_xml_node_name  IMPORTING new_node = lk_xml_node-xml_node ).
      APPEND lk_xml_node TO li_xml_nodes.
    ENDIF.
  ENDLOOP.

  LOOP AT li_xml_nodes ASSIGNING <lk_xml_node> WHERE value IS NOT INITIAL.
    lo_ixml_dom->add_text( EXPORTING value = <lk_xml_node>-value parent = <lk_xml_node>-xml_node ).
  ENDLOOP.
  CLEAR lv_xml.

  lo_ixml_render->render_xstring( EXPORTING dom = lo_ixml_dom IMPORTING stream = lv_xml ).

ENDFORM.                    "data_display_proxy_tree2xml
*&---------------------------------------------------------------------*
*&      Form  data_display_tree_sort
*&---------------------------------------------------------------------*
FORM  data_display_tree_sort USING lv_nodeid          TYPE uac_node_id
                                   li_tree_nodes      TYPE uac_t_node_long
                          CHANGING li_tree_nodes_sort TYPE uac_t_node_long.

  FIELD-SYMBOLS: <lk_tree_node_curr> TYPE uac_s_node_long,
                 <lk_tree_node_next> TYPE uac_s_node_long.
  READ TABLE li_tree_nodes ASSIGNING <lk_tree_node_curr> WITH KEY node_id = lv_nodeid.
  IF sy-subrc = 0.
    IF <lk_tree_node_curr>-parent_node IS INITIAL.
      APPEND <lk_tree_node_curr> TO li_tree_nodes_sort.
    ENDIF.
    IF <lk_tree_node_curr>-first_child IS NOT INITIAL.
      READ TABLE li_tree_nodes ASSIGNING <lk_tree_node_next> WITH KEY node_id = <lk_tree_node_curr>-first_child.
      IF sy-subrc = 0.
        APPEND <lk_tree_node_next> TO li_tree_nodes_sort.
        PERFORM data_display_tree_sort USING <lk_tree_node_next>-node_id li_tree_nodes CHANGING li_tree_nodes_sort.
      ENDIF.
    ENDIF.
    IF <lk_tree_node_curr>-next_sibling IS NOT INITIAL.
      READ TABLE li_tree_nodes ASSIGNING <lk_tree_node_next> WITH KEY node_id = <lk_tree_node_curr>-next_sibling.
      IF sy-subrc = 0.
        APPEND <lk_tree_node_next> TO li_tree_nodes_sort.
        PERFORM data_display_tree_sort USING <lk_tree_node_next>-node_id li_tree_nodes CHANGING li_tree_nodes_sort.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    "data_display_tree_sort
*&---------------------------------------------------------------------*
*&      Form  data_display_proxy_xml2tree
*&---------------------------------------------------------------------*
FORM data_display_proxy_xml2tree USING lv_xml TYPE xstring
                              CHANGING lo_tree TYPE REF TO lcl_gui_tree.
  TYPES: BEGIN OF ltk_tree_nodes,
           level  TYPE i,
           nodeid TYPE uac_node_id,
         END OF ltk_tree_nodes.
  DATA: li_proxy_xml                 TYPE srt_xml_data_tab.
  DATA: lk_proxy_xml                 TYPE srt_xml_data.
  DATA: lv_tree_node_lastid          TYPE uac_node_id.
  DATA: lv_tree_node_lastparent      TYPE uac_s_node_long.
  DATA: lv_tree_node_style           TYPE uac_style_id.
  DATA: lv_tree_node_relat           TYPE uac_relationship.
  DATA: lv_tree_node_leaf            TYPE uac_flag.
  DATA: li_tree_nodes                TYPE SORTED TABLE OF ltk_tree_nodes WITH UNIQUE KEY level.
  DATA: lk_tree_nodes                TYPE ltk_tree_nodes.
  DATA: lv_tree_editable             TYPE uac_flag.
  DATA: lv_proxy_next_tabix          TYPE sytabix.
  FIELD-SYMBOLS: <lk_proxy_xml>      TYPE srt_xml_data.
  FIELD-SYMBOLS: <lk_proxy_xml_next> TYPE srt_xml_data.

  AUTHORITY-CHECK OBJECT 'ZBC_INTF'
         ID '/PSTECH/II' FIELD gk_intfms-interface_id
         ID 'ACTVT' FIELD '02'.
  IF sy-subrc = 0.
    lv_tree_editable = '1'.
  ELSE.
    lv_tree_editable = '0'.
  ENDIF.

  cl_soap_xml_parser=>get_data( EXPORTING xdoc = lv_xml IMPORTING data = li_proxy_xml ).
  DELETE li_proxy_xml WHERE tag_name IS INITIAL.

  CLEAR lv_tree_node_lastid.
  LOOP AT li_proxy_xml ASSIGNING <lk_proxy_xml>.
    lv_proxy_next_tabix = sy-tabix + 1.
    UNASSIGN <lk_proxy_xml_next>.
    READ TABLE li_proxy_xml ASSIGNING <lk_proxy_xml_next> INDEX lv_proxy_next_tabix.

    lv_tree_node_leaf = 0.

    CASE <lk_proxy_xml>-tag_type.
      WHEN 'NODE_ONLY'.
        lv_tree_node_style = 'XML_ELEM'.
      WHEN 'DATA'.
        lv_tree_node_style = 'XML_ELEM'.
        lv_tree_node_leaf = 1.
      WHEN 'ATTRIBUTE'.
        lv_tree_node_style = 'XML_ATTR'.
        <lk_proxy_xml>-tag_level = <lk_proxy_xml>-tag_level + 1.
        lv_tree_node_leaf = 1.
      WHEN OTHERS.
        CONTINUE.
    ENDCASE.

    IF <lk_proxy_xml_next> IS ASSIGNED.
      IF <lk_proxy_xml_next>-tag_type = 'ATTRIBUTE' AND <lk_proxy_xml>-tag_type NE 'ATTRIBUTE' .
        lv_tree_node_leaf = 0.
      ENDIF.
    ENDIF.

    IF lk_proxy_xml-tag_level LT <lk_proxy_xml>-tag_level.
      IF <lk_proxy_xml>-tag_type NE 'ATTRIBUTE'.
        lo_tree->get_node( EXPORTING id_node_id = lv_tree_node_lastid IMPORTING es_node = lv_tree_node_lastparent ).
        lo_tree->change_node( id_node_id = lv_tree_node_lastparent-node_id id_style_id = 'XML_STRUC' ).
      ENDIF.
      lv_tree_node_relat = '6'. "last child
    ELSEIF lk_proxy_xml-tag_level GT <lk_proxy_xml>-tag_level.
      READ TABLE li_tree_nodes INTO lk_tree_nodes WITH KEY level = <lk_proxy_xml>-tag_level.
      lv_tree_node_lastid = lk_tree_nodes-nodeid.
      lv_tree_node_relat = '2'. "siblings
    ELSE.
      IF <lk_proxy_xml>-tag_type NE 'ATTRIBUTE' AND lk_proxy_xml-tag_type EQ 'ATTRIBUTE'.
        lv_tree_node_lastparent = go_proxy_tree->get_parent( id_node_id = lv_tree_node_lastid ).
        lo_tree->change_node( id_node_id = lv_tree_node_lastparent-node_id id_style_id = 'XML_STRUC' ).
      ENDIF.
      lv_tree_node_relat = '2'. "siblings
    ENDIF.

    lv_tree_node_lastid = lo_tree->add_node( id_relat_node = lv_tree_node_lastid id_is_leaf = lv_tree_node_leaf  id_relationship = lv_tree_node_relat id_value = <lk_proxy_xml>-tag_name id_style_id = lv_tree_node_style ).
    lo_tree->add_cell( id_node_id = lv_tree_node_lastid id_column_id = '1' id_editable = lv_tree_editable id_value = <lk_proxy_xml>-tag_value ).

    lk_tree_nodes-level = <lk_proxy_xml>-tag_level.
    lk_tree_nodes-nodeid = lv_tree_node_lastid.

    IF <lk_proxy_xml>-tag_type NE 'ATTRIBUTE'.
      READ TABLE li_tree_nodes WITH KEY level = <lk_proxy_xml>-tag_level TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        MODIFY TABLE li_tree_nodes FROM lk_tree_nodes.
      ELSE.
        APPEND lk_tree_nodes TO li_tree_nodes.
      ENDIF.
    ENDIF.

    lk_proxy_xml = <lk_proxy_xml>.
  ENDLOOP.
  lo_tree->expand_node( id_node_id = '1' ).
  PERFORM data_display_tree_update CHANGING lo_tree.
ENDFORM.                    "data_display_proxy_xml2tree
*&---------------------------------------------------------------------*
*&      Form  data_display_tree_update
*&---------------------------------------------------------------------*
FORM data_display_tree_update CHANGING lo_tree TYPE REF TO lcl_gui_tree.
  DATA: li_tree_nodes TYPE uac_t_node_long.
  FIELD-SYMBOLS <lk_tree_nodes> TYPE uac_s_node_long.

  IF go_proxy_tree IS NOT INITIAL.
    li_tree_nodes = lo_tree->get_nodes( ).
    LOOP AT li_tree_nodes ASSIGNING <lk_tree_nodes>.
      CASE <lk_tree_nodes>-style_id.
        WHEN 'XML_STRUC' OR 'XML_STRUC2' .
          lo_tree->change_cell( id_node_id = <lk_tree_nodes>-node_id id_column_id = '1' id_editable = '0' id_u_editable = '1' ).
      ENDCASE.
    ENDLOOP.
  ENDIF.

  lo_tree->column_optimize( id_column_id = '&Hierarchy' ).
  lo_tree->column_optimize( id_column_id = '1' ).
*  lo_tree->update_frontend( ).
ENDFORM.                    "data_display_tree_update
*&---------------------------------------------------------------------*
*&      Form  data_display_tree_search
*&---------------------------------------------------------------------*
FORM data_display_tree_search CHANGING lo_tree TYPE REF TO lcl_gui_tree.
  DATA: lk_popup_struc      TYPE sval.
  DATA: li_popup_struc      TYPE STANDARD TABLE OF sval.
  DATA: lv_search_pattern   TYPE uac_value.
  DATA: li_tree_node        TYPE uac_t_node_long,
        li_tree_node_sort   TYPE uac_t_node_long,
        li_tree_result_node TYPE uac_t_node_long.
  DATA: li_tree_result_cell TYPE uac_t_cell_long.
  FIELD-SYMBOLS : <lk_tree_result>      TYPE uac_s_node_long.

  lk_popup_struc-tabname = 'RSDXX'.
  lk_popup_struc-fieldname = 'FINDSTR'.
  lk_popup_struc-novaluehlp = 'X'.
  APPEND lk_popup_struc TO li_popup_struc.

  CLEAR: gi_tree_search_result, gv_tree_search_index.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = 'Find'
      no_value_check  = 'X'
    TABLES
      fields          = li_popup_struc
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  READ TABLE li_popup_struc INTO lk_popup_struc INDEX 1.
  IF lk_popup_struc-value IS NOT INITIAL.
    CONCATENATE '*' lk_popup_struc-value '*' INTO lv_search_pattern.
    lo_tree->find_item_by_pattern( EXPORTING id_pattern = lv_search_pattern IMPORTING et_nodes = li_tree_result_node et_cells = li_tree_result_cell ).
  ENDIF.


  IF li_tree_result_node[] IS NOT INITIAL OR li_tree_result_cell[] IS NOT INITIAL.
    li_tree_node = lo_tree->get_nodes( ).
    READ TABLE li_tree_node ASSIGNING <lk_tree_result> WITH KEY parent_node = ''.
    IF sy-subrc = 0.
      PERFORM data_display_tree_sort USING <lk_tree_result>-node_id li_tree_node CHANGING li_tree_node_sort.
      LOOP AT li_tree_node_sort ASSIGNING <lk_tree_result>.
        READ TABLE li_tree_result_node TRANSPORTING NO FIELDS WITH KEY node_id = <lk_tree_result>-node_id.
        IF sy-subrc = 0.
          APPEND  <lk_tree_result> TO gi_tree_search_result.
        ELSE.
          READ TABLE li_tree_result_cell TRANSPORTING NO FIELDS WITH KEY node_id = <lk_tree_result>-node_id.
          IF sy-subrc = 0.
            APPEND  <lk_tree_result> TO gi_tree_search_result .
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

  PERFORM data_display_tree_search_next CHANGING lo_tree.

ENDFORM.                    "data_display_tree_search
*&---------------------------------------------------------------------*
*&      Form  data_display_tree_search_next
*&---------------------------------------------------------------------*
FORM data_display_tree_search_next CHANGING lo_tree TYPE REF TO lcl_gui_tree.
  FIELD-SYMBOLS : <lk_tree_result>      TYPE uac_s_node_long.
  IF gi_tree_search_result[] IS NOT INITIAL.
    gv_tree_search_index = gv_tree_search_index + 1.
    READ TABLE gi_tree_search_result ASSIGNING <lk_tree_result> INDEX gv_tree_search_index.
    IF sy-subrc = 0.
      IF <lk_tree_result>-expanded NE '1'.
        lo_tree->expand_node( id_node_id = <lk_tree_result>-node_id ).
      ENDIF.
      lo_tree->set_selected_node( id_node_id = <lk_tree_result>-node_id ).
    ELSE.
      CLEAR gv_tree_search_index.
      MESSAGE 'End of search' TYPE 'I'.
    ENDIF.
  ENDIF.
ENDFORM.                    "data_display_tree_search_next
*&---------------------------------------------------------------------*
*&      Form  data_display_tree_hide_empty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LO_TREE    text
*----------------------------------------------------------------------*
FORM data_display_tree_hide_empty CHANGING lo_tree TYPE REF TO lcl_gui_tree.

  DATA: li_tree_node        TYPE uac_t_node_long,
        li_tree_node_sort   TYPE uac_t_node_long,
        li_tree_result_node TYPE uac_t_node_long.
  DATA: li_tree_result_cell TYPE uac_t_cell_long.
  FIELD-SYMBOLS : <lk_tree_result>      TYPE uac_s_node_long.

  li_tree_node = lo_tree->get_nodes( ).

  READ TABLE li_tree_node ASSIGNING <lk_tree_result> WITH KEY parent_node = ''.
  IF sy-subrc = 0.
    PERFORM data_display_tree_sort USING <lk_tree_result>-node_id li_tree_node CHANGING li_tree_node_sort.
    LOOP AT li_tree_node_sort ASSIGNING <lk_tree_result>.

    ENDLOOP.
  ENDIF.

ENDFORM.                    "data_display_tree_hide_empty
*&---------------------------------------------------------------------*
*&      Form  data_display_tree_show_empty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LO_TREE    text
*----------------------------------------------------------------------*
FORM data_display_tree_show_empty CHANGING lo_tree TYPE REF TO lcl_gui_tree.
  FIELD-SYMBOLS : <lk_tree_result>      TYPE uac_s_node_long.
  IF gi_tree_search_result[] IS NOT INITIAL.
    gv_tree_search_index = gv_tree_search_index + 1.
    READ TABLE gi_tree_search_result ASSIGNING <lk_tree_result> INDEX gv_tree_search_index.
    IF sy-subrc = 0.
      IF <lk_tree_result>-expanded NE '1'.
        lo_tree->expand_node( id_node_id = <lk_tree_result>-node_id ).
      ENDIF.
      lo_tree->set_selected_node( id_node_id = <lk_tree_result>-node_id ).
    ELSE.
      CLEAR gv_tree_search_index.
      MESSAGE 'End of search' TYPE 'I'.
    ENDIF.
  ENDIF.
ENDFORM.                    "data_display_tree_show_empty
*&---------------------------------------------------------------------*
*&      Form  data_display_proxy_tree_config
*&---------------------------------------------------------------------*
FORM data_display_proxy_tree_config.
  DATA: lk_node_style        TYPE uac_s_node_style.
  DATA: lk_column_style      TYPE uac_s_column_style.
  DATA: lk_cell_style        TYPE uac_s_cell_style.

  lk_column_style-font_style        = 0.
  lk_column_style-alignment         = 0.
  go_proxy_tree->add_column_style( is_column_style = lk_column_style id_style_id = 'COL2' ).
  go_proxy_tree->add_column( id_value  = 'Value' id_style_id = 'COL2' ).

  lk_cell_style-class               = 1.
  lk_cell_style-font_style          = 0.
  lk_cell_style-alignment           = 0.
  lk_cell_style-decimals            = 0.
  lk_cell_style-max_length          = 255.
  lk_cell_style-text_color          = '000000'.
  lk_cell_style-background_color    = 'FFFFFF'.
  go_proxy_tree->add_cell_style( is_cell_style = lk_cell_style id_style_id = 'CELL1' ).

  lk_node_style-font              = 1.
  lk_node_style-icon_open         = '@HP@'.
  lk_node_style-icon_closed       = '@HP@'.
  lk_node_style-text_color        = '000000'.
  lk_node_style-background_color  = 'FFFFFF'.
  lk_node_style-max_length        = 30.
  go_proxy_tree->add_node_style( is_node_style = lk_node_style id_style_id = 'XML_STRUC' ).

  lk_node_style-font              = 0.
  lk_node_style-icon_open         = '@HO@'.
  lk_node_style-icon_closed       = '@HO@'.
  lk_node_style-text_color        = '000000'.
  lk_node_style-background_color  = 'FFFFFF'.
  lk_node_style-max_length        = 30.
  go_proxy_tree->add_node_style( is_node_style = lk_node_style id_style_id = 'XML_ELEM' ).

  lk_node_style-font              = 2.
  lk_node_style-icon_open         = '@7E@'.
  lk_node_style-icon_closed       = '@7E@'.
  lk_node_style-text_color        = '000000'.
  lk_node_style-background_color  = 'FFFFFF'.
  lk_node_style-max_length        = 30.
  go_proxy_tree->add_node_style( is_node_style = lk_node_style id_style_id = 'XML_ATTR' ).

ENDFORM.                    "data_display_idoc_settings
*&---------------------------------------------------------------------*
*&      Form  data_display_proxy_xml
*&---------------------------------------------------------------------*
FORM data_display_proxy_xml USING lk_intfms TYPE gtk_intfms.
  DATA: li_proxy_toolbar_event    TYPE cntl_simple_events,
        lk_proxy_toolbar_event    TYPE cntl_simple_event.
  DATA: lo_proxy_toolbar_handle   TYPE REF TO lcl_tool_events_handle.
  DATA: li_msg_payload     TYPE sxms_messagepayload_tab.
  DATA: lo_xms_persist_adm TYPE REF TO cl_xms_persist_adm.
  FIELD-SYMBOLS: <lk_msg_payload> TYPE sxms_messagepayload.

  "Clear off other objects
  IF go_log_table IS NOT INITIAL OR go_idoc_tree IS NOT INITIAL OR go_proxy_tree IS NOT INITIAL.
    go_main_split_container->remove_control( row = 1 column = 2 ).
    FREE: go_main_view_container, go_log_table, go_proxy_xml, go_idoc_tree, go_proxy_tree.
  ENDIF.

  "Create view container
  IF go_main_view_container IS INITIAL.
    go_main_view_container = go_main_split_container->get_container( row = 1 column = 2 ).
    CLEAR: go_view_split_container, go_view_tool_container, go_view_obj_container, go_view_toolbar.
  ENDIF.

  "Create split toobar & objects, then set Toolbars
  IF go_view_split_container IS INITIAL.
    CREATE OBJECT go_view_split_container
      EXPORTING
        parent  = go_main_view_container
        rows    = 2
        columns = 1.
    go_view_split_container->set_row_mode(   mode = cl_gui_splitter_container=>mode_absolute ).
    go_view_split_container->set_row_sash(   id = 1 type = cl_gui_splitter_container=>type_sashvisible value = cl_gui_splitter_container=>false ).
    go_view_split_container->set_row_sash(   id = 1 type = cl_gui_splitter_container=>type_movable     value = cl_gui_splitter_container=>false ).
    go_view_split_container->set_row_height( id = 1 height = 24 ).
    go_view_split_container->set_border( border = ' ' ).
    go_view_tool_container = go_view_split_container->get_container( row = 1 column = 1 ).
    go_view_obj_container  = go_view_split_container->get_container( row = 2 column = 1 ).

    CREATE OBJECT go_view_toolbar
      EXPORTING
        parent = go_view_tool_container.
    go_view_toolbar->add_button_group( gi_proxy_xml_button ).
    lk_proxy_toolbar_event-eventid = cl_gui_toolbar=>m_id_function_selected.  lk_proxy_toolbar_event-appl_event = ' '.
    APPEND lk_proxy_toolbar_event TO li_proxy_toolbar_event.
    go_view_toolbar->set_registered_events( li_proxy_toolbar_event ).
    CREATE OBJECT lo_proxy_toolbar_handle.
    SET HANDLER lo_proxy_toolbar_handle->command     FOR go_view_toolbar.
  ENDIF.

  IF go_proxy_xml IS INITIAL.
    CREATE OBJECT go_proxy_xml
      EXPORTING
        parent = go_view_obj_container.
    go_proxy_xml->set_source_type( 'XML' ).
    go_proxy_xml->set_content_type( 'application/xml' ).

    AUTHORITY-CHECK OBJECT 'ZBC_INTF'
             ID '/PSTECH/II' FIELD lk_intfms-interface_id
             ID 'ACTVT' FIELD '02'.
    IF sy-subrc EQ 0.
      go_proxy_xml->set_readonly( abap_false ).
    ELSE.
      go_proxy_xml->set_readonly( abap_true ).
    ENDIF.

    go_proxy_xml->set_change_mode( abap_true ).
  ENDIF.

  "Retrieve XML
  IF gv_xml_document IS INITIAL.
    CREATE OBJECT lo_xms_persist_adm.
    TRY.
        lo_xms_persist_adm->get_xi_payload(
          EXPORTING
            im_msgguid         = lk_intfms-proxy_msgguid
            im_pid             = lk_intfms-proxy_pid
            im_version         = '000'
          IMPORTING
            ex_messagepayload  = li_msg_payload ).
      CATCH cx_xms_syserr_persist.
    ENDTRY.

    "Retrieve Payload & show XML
    READ TABLE li_msg_payload ASSIGNING <lk_msg_payload> INDEX 1.
    IF sy-subrc = 0.
      gk_intfms = lk_intfms.
      go_proxy_xml->set_xstring( <lk_msg_payload>-payload ).
    ENDIF.
    go_proxy_xml->set_change_mode( abap_false ).
  ELSE.
    go_proxy_xml->set_xstring( gv_xml_document ).
    go_proxy_xml->set_change_mode( abap_false ).
  ENDIF.
ENDFORM.                    "data_display_xml
*&---------------------------------------------------------------------*
*&      Form  data_display_log_alv
*&---------------------------------------------------------------------*
FORM data_display_log_alv USING lk_intfms TYPE gtk_intfms.
  DATA li_log_msghndl            TYPE bal_t_msgh.
  DATA li_log_handle             TYPE bal_t_logh.
  DATA li_log_header             TYPE balhdr_t.
  DATA lk_log_header             TYPE balhdr.
  DATA lk_log_msg                TYPE bal_s_msg.
  DATA lk_log_table              TYPE gtk_log_table.
  DATA lo_alv_columns            TYPE REF TO cl_salv_columns_table.
  DATA lo_alv_column             TYPE REF TO cl_salv_column_table.
  FIELD-SYMBOLS <lk_log_msghndl> TYPE balmsghndl.

  "Make sure that there's a log number
  CHECK lk_intfms-lognumber IS NOT INITIAL.

  "Cleanup
  IF go_idoc_tree IS NOT INITIAL OR go_proxy_xml IS NOT INITIAL OR go_proxy_tree IS NOT INITIAL.
    go_main_split_container->remove_control( row = 1 column = 2 ).
    FREE: go_main_view_container, go_log_table, go_proxy_xml, go_idoc_tree, go_proxy_tree.
  ENDIF.

  "Re-create the container
  IF go_main_view_container IS INITIAL.
    go_main_view_container = go_main_split_container->get_container( row = 1 column = 2 ).
  ENDIF.

  SELECT SINGLE * INTO lk_log_header FROM balhdr WHERE lognumber = lk_intfms-lognumber.
  SELECT log_handle INTO TABLE li_log_handle FROM balhdr WHERE lognumber = lk_intfms-lognumber.

  CALL FUNCTION 'BAL_LOG_EXIST'
    EXPORTING
      i_log_handle  = lk_log_header-log_handle
    EXCEPTIONS
      log_not_found = 1.
  IF sy-subrc NE 0.
    APPEND lk_log_header TO li_log_header.
    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_log_header = li_log_header
      EXCEPTIONS
        OTHERS         = 0.
  ENDIF.

  "retrieve msg handle
  CALL FUNCTION 'BAL_GLB_SEARCH_MSG'
    EXPORTING
      i_t_log_handle = li_log_handle
    IMPORTING
      e_t_msg_handle = li_log_msghndl
    EXCEPTIONS
      msg_not_found  = 1
      OTHERS         = 2.
  CHECK sy-subrc EQ 0.

  CLEAR gi_log_table.
  LOOP AT li_log_msghndl ASSIGNING  <lk_log_msghndl> .
    CALL FUNCTION 'BAL_LOG_MSG_READ'
      EXPORTING
        i_s_msg_handle = <lk_log_msghndl>
      IMPORTING
        e_s_msg        = lk_log_msg
      EXCEPTIONS
        log_not_found  = 1
        msg_not_found  = 2
        OTHERS         = 3.
    CASE lk_log_msg-msgty.
      WHEN 'A' OR 'E'. lk_log_table-log_ico = '@5C@'.
      WHEN 'I' OR 'S'. lk_log_table-log_ico = '@5B@'.
      WHEN 'W'       . lk_log_table-log_ico = '@5D@'.
    ENDCASE.
    MESSAGE ID lk_log_msg-msgid TYPE lk_log_msg-msgty NUMBER lk_log_msg-msgno WITH lk_log_msg-msgv1 lk_log_msg-msgv2 lk_log_msg-msgv3 lk_log_msg-msgv4 INTO lk_log_table-log_msg.
    APPEND lk_log_table TO gi_log_table.
  ENDLOOP.

  IF go_log_table IS INITIAL AND gi_log_table[] IS NOT INITIAL .
    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container  = go_main_view_container
          IMPORTING
            r_salv_table = go_log_table
          CHANGING
            t_table      = gi_log_table ).

        "Columns
        lo_alv_columns = go_log_table->get_columns( ).
        lo_alv_columns->set_optimize( abap_true ).

        lo_alv_column ?= lo_alv_columns->get_column( 'LOG_ICO' ).
        lo_alv_column->set_short_text( text-ks9 ).

        lo_alv_column ?= lo_alv_columns->get_column( 'LOG_MSG' ).
        lo_alv_column->set_short_text( text-kt1 ).

        go_log_table->display( ).
      CATCH cx_salv_msg cx_salv_not_found.
    ENDTRY.
  ELSE.
    go_log_table->refresh( ).
  ENDIF.

ENDFORM.                    "data_display_log

*&---------------------------------------------------------------------*
*&      Module  STATUS_1001  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_1001 OUTPUT.
  PERFORM data_display_screen.
ENDMODULE.                 " STATUS_1001  OUTPUT

*----------------------------------------------------------------------*
*  MODULE user_command_1001 INPUT
*----------------------------------------------------------------------*
MODULE user_command_1001 INPUT.
  PERFORM data_display_commands USING sy-ucomm.
  CALL METHOD cl_gui_cfw=>flush.
ENDMODULE.                 " USER_COMMAND_1001  INPUT
*&---------------------------------------------------------------------*
*&      Form  data_process_proxy_restart
*&---------------------------------------------------------------------*
FORM data_process_proxy_restart USING lk_intfms TYPE gtk_intfms.
  DATA lo_xms_main          TYPE REF TO cl_xms_main.
  DATA lx_xms_system_error  TYPE REF TO cx_xms_system_error. "#EC NEEDED

  lo_xms_main = cl_xms_main=>create_xmb( ).

* reinstantiate the message-object
  TRY.
      CALL METHOD lo_xms_main->restart_error_message
        EXPORTING
          im_message_guid = lk_intfms-proxy_msgguid
          im_version      = '000'
          im_pipeline_id  = lk_intfms-proxy_pid.
    CATCH cx_xms_system_error INTO lx_xms_system_error.
  ENDTRY.

ENDFORM.                    "data_process_proxy_restart
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS_PROXY_COPYPROC
*&---------------------------------------------------------------------*
FORM data_process_proxy_copyproc USING lk_intfms TYPE gtk_intfms.
  DATA lv_guid_new          TYPE sxmsguid.
  DATA lk_intfms_new        TYPE /pstech/bcintfms.
  DATA lo_xms_main          TYPE REF TO cl_xms_main.
  DATA lo_xms_system_error  TYPE REF TO cx_xms_system_error. "#EC NEEDED
  DATA lo_xms_runtime       TYPE REF TO cl_xms_run_time_env.
  DATA lk_xms_runtime_eoref TYPE sxmseoref.
  DATA lo_xms_message       TYPE REF TO if_xms_message.
  DATA lo_xms_message_xmb   TYPE REF TO if_xms_message_xmb.
  DATA l_timestamp          TYPE timestamp.
  DATA lo_xms_hoplist       TYPE REF TO cl_xms_msghdr30_hoplist.
  DATA lv_xms_hoplist_count TYPE i.
  DATA lo_xms_engine        TYPE REF TO if_xms_engine.
  DATA lv_adapter_type      TYPE sxmspstype.

  lo_xms_main = cl_xms_main=>create_xmb( ).

  "Reinstantiate the message-object
  TRY.
      CALL METHOD lo_xms_main->read_message_from_persist
        EXPORTING
          im_message_guid = lk_intfms-proxy_msgguid
          im_version      = '000'
          im_pipeline_id  = lk_intfms-proxy_pid
        IMPORTING
          ex_message      = lo_xms_message.
    CATCH cx_xms_system_error INTO lo_xms_system_error.
  ENDTRY.

  IF lo_xms_message IS NOT INITIAL.
    CALL METHOD cl_xms_main=>get_message_properties
      EXPORTING
        im_message      = lo_xms_message
      IMPORTING
        ex_run_time_env = lo_xms_runtime.

    lk_xms_runtime_eoref-id = 'RESTART'.
    CONCATENATE  sy-uname ':' lk_intfms-interface_msgid INTO lk_xms_runtime_eoref-val.
    lo_xms_runtime->set_user_name( sy-uname ).
    lo_xms_runtime->set_eo_ref_inbound( lk_xms_runtime_eoref ).

    lo_xms_message_xmb ?= lo_xms_message.

    "Create GUID for the message
    lv_guid_new = cl_xms_msg_util=>create_guid( ).

    "set attributes of the message-header
    lo_xms_message_xmb->set_message_id( lv_guid_new ).
    "set send date and time
    GET TIME STAMP FIELD l_timestamp.
    lo_xms_message_xmb->set_time_sent( l_timestamp ).

    CALL METHOD cl_xms_main=>get_message_properties
      EXPORTING
        im_message = lo_xms_message
      IMPORTING
        ex_hoplist = lo_xms_hoplist.

    IF lo_xms_hoplist IS NOT INITIAL.
      DESCRIBE TABLE lo_xms_hoplist->hoplist LINES lv_xms_hoplist_count.
      DELETE  lo_xms_hoplist->hoplist FROM lv_xms_hoplist_count.
    ENDIF.

    CASE lk_intfms-interface_dir.
      WHEN 'I'.
        lv_adapter_type = 'PLAINHTTP'.
      WHEN 'O'.
        lv_adapter_type = ''.
    ENDCASE.

    TRY.
        lo_xms_engine = cl_xms_main=>create_engine( ).
        CALL METHOD lo_xms_engine->enter_engine
          EXPORTING
            im_execute_flag = '1'
            im_adapter_id   = lv_adapter_type
          CHANGING
            ch_message      = lo_xms_message_xmb.

      CATCH cx_xms_system_error INTO lo_xms_system_error.
    ENDTRY.
    COMMIT WORK.
  ENDIF.

  IF lk_intfms-interface_dir = 'O'.
    lk_intfms_new = lk_intfms.
    lk_intfms_new-interface_msgid = lv_guid_new.
    lk_intfms_new-execute_date = sy-datum.
    lk_intfms_new-execute_time = sy-uzeit.
    MODIFY /pstech/bcintfms FROM lk_intfms_new.
  ENDIF.

ENDFORM.                    " DATA_PROCESS_PROXY_COPYPROC

*&---------------------------------------------------------------------*
*&      Form  data_process_proxy_editproc
*&---------------------------------------------------------------------*
FORM data_process_proxy_editproc USING lk_intfms TYPE gtk_intfms.
  DATA lv_contenttype       TYPE string.
  DATA lv_guid_new          TYPE sxmsguid.
  DATA lk_intfms_new        TYPE /pstech/bcintfms.
  DATA lo_xms_main          TYPE REF TO cl_xms_main.
  DATA lo_xms_system_error  TYPE REF TO cx_xms_system_error. "#EC NEEDED
  DATA lo_xms_runtime       TYPE REF TO cl_xms_run_time_env.
  DATA lk_xms_runtime_eoref TYPE sxmseoref.
  DATA lo_xms_message       TYPE REF TO if_xms_message.
  DATA lo_xms_message_xmb   TYPE REF TO if_xms_message_xmb.
  DATA lo_xms_payload       TYPE REF TO if_xms_payload.
  DATA lv_timestamp         TYPE timestamp.
  DATA lo_xms_hoplist       TYPE REF TO cl_xms_msghdr30_hoplist.
  DATA lv_xms_hoplist_count TYPE i.
  DATA lo_xms_engine        TYPE REF TO if_xms_engine.
  DATA lv_adapter_type      TYPE sxmspstype.

  CHECK gv_xml_document IS NOT INITIAL.

  lo_xms_main = cl_xms_main=>create_xmb( ).

* reinstantiate the message-object
  TRY.
      CALL METHOD lo_xms_main->read_message_from_persist
        EXPORTING
          im_message_guid = lk_intfms-proxy_msgguid
          im_version      = '000'
          im_pipeline_id  = lk_intfms-proxy_pid
        IMPORTING
          ex_message      = lo_xms_message.
    CATCH cx_xms_system_error INTO lo_xms_system_error.
  ENDTRY.

  IF lo_xms_message IS NOT INITIAL.
    "Create new GUID for the message
    lv_guid_new = cl_xms_msg_util=>create_guid( ).

    lo_xms_message_xmb ?= lo_xms_message.

    "Set Runtime Message
    CALL METHOD cl_xms_main=>get_message_properties
      EXPORTING
        im_message      = lo_xms_message
      IMPORTING
        ex_run_time_env = lo_xms_runtime.

    lk_xms_runtime_eoref-id = 'MODIFY'.
    CONCATENATE  sy-uname ':' lk_intfms-interface_msgid INTO lk_xms_runtime_eoref-val.
    lo_xms_runtime->set_user_name( sy-uname ).
    lo_xms_runtime->set_eo_ref_inbound( lk_xms_runtime_eoref ).

    "set attributes of the message-header
    lo_xms_message_xmb->set_message_id( lv_guid_new ).
    "set send date and time
    GET TIME STAMP FIELD lv_timestamp.
    lo_xms_message_xmb->set_time_sent( lv_timestamp ).

    "Remove last hoplist
    CALL METHOD cl_xms_main=>get_message_properties
      EXPORTING
        im_message = lo_xms_message
      IMPORTING
        ex_hoplist = lo_xms_hoplist.

    IF lo_xms_hoplist IS NOT INITIAL.
      DESCRIBE TABLE lo_xms_hoplist->hoplist LINES lv_xms_hoplist_count.
      DELETE  lo_xms_hoplist->hoplist FROM lv_xms_hoplist_count.
    ENDIF.

    "Update Payload
    TRY.
        lo_xms_payload = lo_xms_message_xmb->get_payload_by_name( lo_xms_message_xmb->co_payloadname_main ).
        lv_contenttype = lo_xms_payload->getcontenttype( ).
        lo_xms_payload->setbinarycontent( data = gv_xml_document type = lv_contenttype ).

        CASE lk_intfms-interface_dir.
          WHEN 'I'.
            lv_adapter_type = 'PLAINHTTP'.
          WHEN 'O'.
            lv_adapter_type = ''.
        ENDCASE.


        lo_xms_engine = cl_xms_main=>create_engine( ).
        CALL METHOD lo_xms_engine->enter_engine
          EXPORTING
            im_execute_flag = '1'
            im_adapter_id   = lv_adapter_type
          CHANGING
            ch_message      = lo_xms_message_xmb.

      CATCH cx_xms_system_error INTO lo_xms_system_error.
    ENDTRY.
    COMMIT WORK.

    IF lk_intfms-interface_dir = 'O'.
      lk_intfms_new = lk_intfms.
      lk_intfms_new-interface_msgid = lv_guid_new.
      lk_intfms_new-execute_date = sy-datum.
      lk_intfms_new-execute_time = sy-uzeit.
      MODIFY /pstech/bcintfms FROM lk_intfms_new.
    ENDIF.

  ENDIF.


ENDFORM.                    " DATA_PROCESS_PROXY_EDITPROC


*&---------------------------------------------------------------------*
*&      Form  screen_alvvar_default
*&---------------------------------------------------------------------*
FORM screen_alvvar_default CHANGING lv_alvvar TYPE slis_vari.
  DATA: lk_alv_layout TYPE salv_s_layout_info,
        lk_alv_key    TYPE salv_s_layout_key.
  lk_alv_key-report = sy-repid.
  lk_alv_layout = cl_salv_layout_service=>get_default_layout( s_key = lk_alv_key ).
  lv_alvvar = lk_alv_layout-layout.
ENDFORM.                    "screen_alvvar_default
*&---------------------------------------------------------------------*
*&      Form  screen_alvvar_searchvalue
*&---------------------------------------------------------------------*
FORM screen_alvvar_searchvalue  CHANGING lv_alvvar TYPE slis_vari.
  DATA: lk_alv_layout TYPE salv_s_layout_info,
        lk_alv_key    TYPE salv_s_layout_key.
  lk_alv_key-report = sy-repid.
  lk_alv_layout = cl_salv_layout_service=>f4_layouts( s_key = lk_alv_key ).
  lv_alvvar = lk_alv_layout-layout.
ENDFORM.                    "screen_alvvar_searchvalue
*&---------------------------------------------------------------------*
*&      Form  screen_intfid_searchvalue
*&---------------------------------------------------------------------*
FORM screen_intfid_searchvalue.
  TYPES: BEGIN OF ltk_request,
          intfid TYPE /pstech/bcintfid,
          desc   TYPE /pstech/bcintfmm-interface_desc,
         END OF ltk_request.

  DATA: li_request           TYPE STANDARD TABLE OF ltk_request.
  DATA: li_return            TYPE rsdm_f4_return_values.
  FIELD-SYMBOLS <lk_return>  TYPE ddshretval.
  FIELD-SYMBOLS <lk_request> TYPE ltk_request.

  SELECT interface_id interface_desc INTO TABLE li_request FROM /pstech/bcintfmm.

  LOOP AT li_request ASSIGNING <lk_request>.
    AUTHORITY-CHECK OBJECT 'ZBC_INTF'
             ID '/PSTECH/II' FIELD <lk_request>-intfid
             ID 'ACTVT' FIELD '03'.
    IF sy-subrc NE 0.
      DELETE li_request INDEX sy-tabix.
    ENDIF.
  ENDLOOP.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield   = 'INTFID'   "field of internal table
      value_org  = 'S'
    TABLES
      value_tab  = li_request
      return_tab = li_return.

  LOOP AT li_return ASSIGNING <lk_return>.
    s_intfid-sign = 'I'.
    s_intfid-option = 'EQ'.
    s_intfid-low = <lk_return>-fieldval.
  ENDLOOP.

ENDFORM.                    "screen_intfid_searchvalue
*&---------------------------------------------------------------------*
*&      Form  data_display_screen
*&---------------------------------------------------------------------*
FORM data_display_screen.
  DATA lv_table_count TYPE i.

  DESCRIBE TABLE gi_intfms LINES lv_table_count.
  "SET TITLEBAR '000' WITH lv_table_count.
  "SET PF-STATUS 'STATUS_1001'.
  IF go_main_container IS INITIAL.
    CREATE OBJECT go_main_container
      EXPORTING
        container_name = 'DISPLAY_CTRL'.

    CREATE OBJECT go_main_split_container
      EXPORTING
        parent  = go_main_container
        rows    = 1
        columns = 2.

    go_main_alv_container = go_main_split_container->get_container( row = 1 column = 1 ).

    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container  = go_main_alv_container
          IMPORTING
            r_salv_table = go_main_alv_table
          CHANGING
            t_table      = gi_intfms ).
        PERFORM data_display_main_alv_config.
      CATCH cx_salv_msg.
    ENDTRY.
  ENDIF.
ENDFORM.                    " data_display_screen
*&---------------------------------------------------------------------*
*&      Form  data_display_settings
*&---------------------------------------------------------------------*
FORM data_display_settings.
  DATA: li_xml_filter_list TYPE vrm_values, lk_xml_filter_list TYPE vrm_value.
  lk_xml_filter_list-key = '1'. lk_xml_filter_list-text = 'OR'.  APPEND lk_xml_filter_list TO li_xml_filter_list.
  lk_xml_filter_list-key = '2'. lk_xml_filter_list-text = 'AND'. APPEND lk_xml_filter_list TO li_xml_filter_list.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'S_XMLO1'
      values = li_xml_filter_list.

  "View Toolbars
  DATA: lk_button  TYPE stb_button.
  CLEAR lk_button.
  lk_button-butn_type  = 0.
  lk_button-function   = 'NODEEXPAND'.
  lk_button-icon       = '@3S@'.
  lk_button-quickinfo  = 'Expand All Nodes'.
  APPEND lk_button TO gi_idoc_tree_button.

  CLEAR lk_button.
  lk_button-butn_type  = 0.
  lk_button-function   = 'NODECOLLAPSE'.
  lk_button-icon       = '@3T@'.
  lk_button-quickinfo  = 'Collapse All Nodes'.
  APPEND lk_button TO gi_idoc_tree_button.

  CLEAR lk_button.
  lk_button-butn_type  = 3.
  APPEND lk_button TO gi_idoc_tree_button.

  CLEAR lk_button.
  lk_button-butn_type  = 0.
  lk_button-function   = 'TREE_SEARCH'.
  lk_button-icon       = '@13@'.
  lk_button-quickinfo  = 'Find'.
  APPEND lk_button TO gi_idoc_tree_button.

  CLEAR lk_button.
  lk_button-butn_type  = 0.
  lk_button-function   = 'TREE_SEARCH_NEXT'.
  lk_button-icon       = '@4E@'.
  lk_button-quickinfo  = 'Find'.
  APPEND lk_button TO gi_idoc_tree_button.

  CLEAR lk_button.
  lk_button-butn_type  = 3.
  APPEND lk_button TO gi_idoc_tree_button.

  CLEAR lk_button.
  lk_button-butn_type  = 0.
  lk_button-icon       = '@2L@'.
  lk_button-function   = 'EDITPROCESS'.
  lk_button-quickinfo  = 'Save and Process'.
  lk_button-text       = 'Save and Process'.
  APPEND lk_button TO gi_idoc_tree_button.

  CLEAR lk_button.
  lk_button-butn_type  = 0.
  lk_button-function   = 'NODEEXPAND'.
  lk_button-icon       = '@3S@'.
  lk_button-quickinfo  = 'Expand All Nodes'.
  APPEND lk_button TO gi_proxy_tree_button.

  CLEAR lk_button.
  lk_button-butn_type  = 0.
  lk_button-function   = 'NODECOLLAPSE'.
  lk_button-icon       = '@3T@'.
  lk_button-quickinfo  = 'Collapse All Nodes'.
  APPEND lk_button TO gi_proxy_tree_button.

  CLEAR lk_button.
  lk_button-butn_type  = 3.
  APPEND lk_button TO gi_proxy_tree_button.

  CLEAR lk_button.
  lk_button-butn_type  = 0.
  lk_button-function   = 'TREE_SEARCH'.
  lk_button-icon       = '@13@'.
  lk_button-quickinfo  = 'Find'.
  APPEND lk_button TO gi_proxy_tree_button.

  CLEAR lk_button.
  lk_button-butn_type  = 0.
  lk_button-function   = 'TREE_SEARCH_NEXT'.
  lk_button-icon       = '@4E@'.
  lk_button-quickinfo  = 'Find'.
  APPEND lk_button TO gi_proxy_tree_button.

  CLEAR lk_button.
  lk_button-butn_type  = 3.
  APPEND lk_button TO gi_proxy_tree_button.

  CLEAR lk_button.
  lk_button-butn_type  = 0.
  lk_button-function   = 'PROXY_XML'.
  lk_button-icon       = '@J4@'.
  lk_button-quickinfo  = 'XML Editor'.
  APPEND lk_button TO gi_proxy_tree_button.

  CLEAR lk_button.
  lk_button-butn_type  = 3.
  APPEND lk_button TO gi_proxy_tree_button.

  CLEAR lk_button.
  lk_button-butn_type  = 0.
  lk_button-icon       = '@2L@'.
  lk_button-function   = 'EDITPROCESS'.
  lk_button-quickinfo  = 'Save and Process'.
  lk_button-text       = 'Save and Process'.
  APPEND lk_button TO gi_proxy_tree_button.

  CLEAR lk_button.
  lk_button-butn_type  = 0.
  lk_button-function   = 'PROXY_XML_TOGGLE'.
  lk_button-icon       = '@3I@'.
  lk_button-quickinfo  = 'Toggle Display/Change'.
  APPEND lk_button TO gi_proxy_xml_button.

  CLEAR lk_button.
  lk_button-butn_type  = 0.
  lk_button-function   = 'PROXY_XML_STORE'.
  lk_button-icon       = '@49@'.
  lk_button-quickinfo  = 'Save to file ...'(005).
  APPEND lk_button TO gi_proxy_xml_button.

  CLEAR lk_button.
  lk_button-butn_type  = 3.
  APPEND lk_button TO gi_proxy_xml_button.

  CLEAR lk_button.
  lk_button-butn_type  = 0.
  lk_button-function   = 'PROXY_TREE'.
  lk_button-icon       = '@JG@'.
  lk_button-quickinfo  = 'XML Tree'.
  APPEND lk_button TO gi_proxy_xml_button.

  CLEAR lk_button.
  lk_button-butn_type  = 3.
  APPEND lk_button TO gi_proxy_xml_button.

  CLEAR lk_button.
  lk_button-butn_type  = 0.
  lk_button-function   = 'PROXY_XML_PRETTY'.
  lk_button-icon       = '@FZ@'.
  lk_button-quickinfo  = 'Format and Indent'.
  lk_button-text       = 'Format and Indent'.
  APPEND lk_button TO gi_proxy_xml_button.

  CLEAR lk_button.
  lk_button-butn_type  = 3.
  APPEND lk_button TO gi_proxy_xml_button.

  CLEAR lk_button.
  lk_button-butn_type  = 0.
  lk_button-icon       = '@2L@'.
  lk_button-function   = 'EDITPROCESS'.
  lk_button-quickinfo  = 'Save and Process'.
  lk_button-text       = 'Save and Process'.
  APPEND lk_button TO gi_proxy_xml_button.

  "Tree Context
  DATA: lk_context TYPE uac_s_context_menu.

  CLEAR lk_context.
  lk_context-menu_item_id       = 'XML_DUPNODE'.
  lk_context-menu_function_code = 'XML_DUPNODE'.
  lk_context-is_separator       = '0'.
  lk_context-disabled           = '0'.
  lk_context-value              = 'Duplicate Node'.
  APPEND lk_context TO gi_proxy_tree_context.

  CLEAR lk_context.
  lk_context-menu_item_id       = '00002'.
  lk_context-is_separator       = '1'.
  APPEND lk_context TO gi_proxy_tree_context.

  CLEAR lk_context.
  lk_context-menu_item_id       = 'XML_ADDELEM'.
  lk_context-menu_function_code = 'XML_ADDELEM'.
  lk_context-is_separator       = '0'.
  lk_context-disabled           = '0'.
  lk_context-value              = 'Add Element'.
  APPEND lk_context TO gi_proxy_tree_context.

  CLEAR lk_context.
  lk_context-menu_item_id       = 'XML_ADDELEM2'.
  lk_context-menu_function_code = 'XML_ADDELEM2'.
  lk_context-is_separator       = '0'.
  lk_context-disabled           = '0'.
  lk_context-value              = 'Add Sub-Element'.
  APPEND lk_context TO gi_proxy_tree_context.

  CLEAR lk_context.
  lk_context-menu_item_id       = 'XML_ADDATTR'.
  lk_context-menu_function_code = 'XML_ADDATTR'.
  lk_context-is_separator       = '0'.
  lk_context-disabled           = '0'.
  lk_context-value              = 'Add Attribute'.
  APPEND lk_context TO gi_proxy_tree_context.

  CLEAR lk_context.
  lk_context-menu_item_id       = '00005'.
  lk_context-is_separator       = '1'.
  lk_context-disabled           = '0'.
  APPEND lk_context TO gi_proxy_tree_context.

  CLEAR lk_context.
  lk_context-menu_item_id       = 'XML_DELNODE'.
  lk_context-menu_function_code = 'XML_DELNODE'.
  lk_context-is_separator       = '0'.
  lk_context-disabled           = '0'.
  lk_context-value              = 'Delete Node'.
  lk_context-icon               = '@18@'.
  APPEND lk_context TO gi_proxy_tree_context.

  CLEAR lk_context.
  lk_context-menu_item_id       = 'IDOC_DUPNODE'.
  lk_context-menu_function_code = 'IDOC_DUPNODE'.
  lk_context-is_separator       = '0'.
  lk_context-disabled           = '0'.
  lk_context-value              = 'Duplicate Structure'.
  APPEND lk_context TO gi_idoc_tree_context.

  CLEAR lk_context.
  lk_context-menu_item_id       = '00001'.
  lk_context-is_separator       = '1'.
  lk_context-disabled           = '0'.
  APPEND lk_context TO gi_idoc_tree_context.

  CLEAR lk_context.
  lk_context-menu_item_id       = 'IDOC_ADDSTR1'.
  lk_context-menu_function_code = 'IDOC_ADDSTR1'.
  lk_context-is_separator       = '0'.
  lk_context-disabled           = '0'.
  lk_context-value              = 'Add Structure'.
  APPEND lk_context TO gi_idoc_tree_context.

  CLEAR lk_context.
  lk_context-menu_item_id       = 'IDOC_ADDSTR2'.
  lk_context-menu_function_code = 'IDOC_ADDSTR2'.
  lk_context-is_separator       = '0'.
  lk_context-disabled           = '0'.
  lk_context-value              = 'Add Sub-Structure'.
  APPEND lk_context TO gi_idoc_tree_context.

  CLEAR lk_context.
  lk_context-menu_item_id       = '00002'.
  lk_context-is_separator       = '1'.
  lk_context-disabled           = '0'.
  APPEND lk_context TO gi_idoc_tree_context.

  CLEAR lk_context.
  lk_context-menu_item_id       = 'IDOC_DELNODE'.
  lk_context-menu_function_code = 'IDOC_DELNODE'.
  lk_context-is_separator       = '0'.
  lk_context-disabled           = '0'.
  lk_context-value              = 'Delete Structure'.
  APPEND lk_context TO gi_idoc_tree_context.


ENDFORM.                    "data_display_settings
*&---------------------------------------------------------------------*
*&      Form  DATA_DISPLAY_COMMANDS
*&---------------------------------------------------------------------*
FORM data_display_commands  USING lv_okcode TYPE syucomm.
  DATA: lv_popup_answer    TYPE c LENGTH 1.
  DATA: lv_table_count     TYPE i.
  DATA: lv_total_count_msg TYPE string.
  DATA: lo_alv_selections  TYPE REF TO cl_salv_selections.
  DATA: li_alv_rows        TYPE salv_t_row.
  DATA: lk_intfms          TYPE gtk_intfms.
  DATA: li_intfms          TYPE gti_intfms.
  DATA: lk_intfms_table    TYPE /pstech/bcintfms.
  DATA: li_intfms_table    TYPE STANDARD TABLE OF /pstech/bcintfms.
  DATA: lv_tree_node_id    TYPE uac_node_id.
  DATA: li_tree_nodes      TYPE uac_t_node_long.
  DATA: li_submit_param    TYPE rsparams_tt.
  DATA: lk_submit_param    TYPE rsparams.

  FIELD-SYMBOLS <lk_tree_nodes> TYPE uac_s_node_long.
  FIELD-SYMBOLS <lk_alv_rows> LIKE LINE OF li_alv_rows.
  FIELD-SYMBOLS <lk_intfms>   TYPE gtk_intfms.

  CLEAR : lv_popup_answer, lv_table_count.

  CASE lv_okcode.
    WHEN 'EXIT' OR 'CANC' OR 'BACK' OR 'OK' OR 'E' OR 'ENDE' OR 'ECAN'.
      SET SCREEN 0.
      LEAVE SCREEN.

    WHEN 'COUNT'.
      DESCRIBE TABLE gi_intfms LINES lv_table_count.
      lv_total_count_msg = lv_table_count.
      CONCATENATE 'Total Messages:' lv_total_count_msg INTO lv_total_count_msg SEPARATED BY space.
      CALL FUNCTION 'POPUP_TO_INFORM'
        EXPORTING
          titel = 'Message Total'
          txt1  = lv_total_count_msg
          txt2  = ' '.

    WHEN 'XMLTABLE'.
      go_main_alv_table->get_metadata( ).
      lo_alv_selections = go_main_alv_table->get_selections( ).
      li_alv_rows = lo_alv_selections->get_selected_rows( ).
      CLEAR: lk_submit_param, li_submit_param[].
      LOOP AT li_alv_rows ASSIGNING <lk_alv_rows>.
        READ TABLE gi_intfms ASSIGNING <lk_intfms> INDEX <lk_alv_rows>.
        IF <lk_intfms>-proxy_msgguid IS NOT INITIAL.
          lk_submit_param-selname = 'S_GUID'.
          lk_submit_param-kind    = 'S'.
          lk_submit_param-sign    = 'I'.
          lk_submit_param-option  = 'EQ'.
          lk_submit_param-low     = <lk_intfms>-proxy_msgguid.
          APPEND lk_submit_param TO li_submit_param.
        ENDIF.
      ENDLOOP.

      CLEAR lk_submit_param.
      lk_submit_param-selname = 'S_RFCGRP'.
      lk_submit_param-kind    = 'P'.
      lk_submit_param-sign    = 'I'.
      lk_submit_param-option  = 'EQ'.
      lk_submit_param-low     = s_parrfc.
      APPEND lk_submit_param TO li_submit_param.

      CLEAR lk_submit_param.
      lk_submit_param-selname = 'S_MAXPAR'.
      lk_submit_param-kind    = 'P'.
      lk_submit_param-sign    = 'I'.
      lk_submit_param-option  = 'EQ'.
      lk_submit_param-low     = s_parmax.
      APPEND lk_submit_param TO li_submit_param.
      SUBMIT /pstech/bci_intf_xmltable VIA SELECTION-SCREEN WITH SELECTION-TABLE li_submit_param AND RETURN.


      "ALV commands
    WHEN 'DELE'.
      go_main_alv_table->get_metadata( ).
      lo_alv_selections = go_main_alv_table->get_selections( ).
      li_alv_rows = lo_alv_selections->get_selected_rows( ).
      IF li_alv_rows[] IS NOT INITIAL.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            text_question         = 'Are you sure you want to delete these indexes ?'(f01)
            text_button_1         = 'Yes'(f02)
            icon_button_1         = '@01@'
            text_button_2         = 'No'(f03)
            icon_button_2         = '@02@'
            default_button        = '1'
            display_cancel_button = ' '
          IMPORTING
            answer                = lv_popup_answer.
      ENDIF.

      IF lv_popup_answer = '1'.
        LOOP AT li_alv_rows ASSIGNING <lk_alv_rows>.
          READ TABLE gi_intfms ASSIGNING <lk_intfms> INDEX <lk_alv_rows>.
          IF sy-subrc = 0.
            lk_intfms_table = <lk_intfms>.
            APPEND lk_intfms_table TO li_intfms_table.
          ENDIF.
        ENDLOOP.
        LOOP AT li_intfms_table INTO lk_intfms_table.

          AUTHORITY-CHECK OBJECT 'ZBC_INTF'
                ID '/PSTECH/II' FIELD lk_intfms_table-interface_id
                ID 'ACTVT' FIELD '06'.
          IF sy-subrc = 0.
            DELETE gi_intfms
              WHERE interface_id    EQ lk_intfms_table-interface_id AND
                    interface_key1  EQ lk_intfms_table-interface_key1 AND
                    interface_key2  EQ lk_intfms_table-interface_key2 AND
                    interface_key3  EQ lk_intfms_table-interface_key3 AND
                    interface_key4  EQ lk_intfms_table-interface_key4 AND
                    interface_msgid EQ lk_intfms_table-interface_msgid.
            lv_table_count = lv_table_count + 1.
          ELSE.
            DELETE li_intfms_table FROM lk_intfms_table.
          ENDIF.
        ENDLOOP.
        DELETE /pstech/bcintfms FROM TABLE li_intfms_table.

        MESSAGE s011(sv) WITH lv_table_count.
        CLEAR li_alv_rows[].

        go_main_alv_table->get_metadata( ).
        lo_alv_selections = go_main_alv_table->get_selections( ).
        lo_alv_selections->set_selected_rows( li_alv_rows[] ).
        go_main_alv_table->refresh( ).
      ENDIF.

    WHEN 'COPYPROCESS'.
      go_main_alv_table->get_metadata( ).
      lo_alv_selections = go_main_alv_table->get_selections( ).
      li_alv_rows = lo_alv_selections->get_selected_rows( ).
      IF li_alv_rows[] IS NOT INITIAL.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            text_question         = 'Are you sure you want to copy & process these indexes ?'(f04)
            text_button_1         = 'Yes'(f02)
            icon_button_1         = '@01@'
            text_button_2         = 'No'(f03)
            icon_button_2         = '@02@'
            default_button        = '1'
            display_cancel_button = ' '
          IMPORTING
            answer                = lv_popup_answer.
      ENDIF.
      IF lv_popup_answer = '1'.
        LOOP AT li_alv_rows ASSIGNING <lk_alv_rows>.
          READ TABLE gi_intfms  ASSIGNING <lk_intfms> INDEX <lk_alv_rows>.
          IF sy-subrc = 0.
            lv_table_count = lv_table_count + 1.
            CASE <lk_intfms>-interface_type.
              WHEN '0'.
                PERFORM data_process_proxy_copyproc USING <lk_intfms>.
              WHEN '1'.
                PERFORM data_process_idoc_copyproc USING <lk_intfms>.
            ENDCASE.
          ENDIF.
        ENDLOOP.
        MESSAGE s020(xms_moni) WITH lv_table_count 'messages copied & processed'.
      ENDIF.

    WHEN 'RESTART'.
      go_main_alv_table->get_metadata( ).
      lo_alv_selections = go_main_alv_table->get_selections( ).
      li_alv_rows = lo_alv_selections->get_selected_rows( ).
      CLEAR : li_intfms[].
      IF li_alv_rows[] IS NOT INITIAL.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            text_question         = 'Are you sure you want to restart error messages from these indexes ?'(f06)
            text_button_1         = 'Yes'(f02)
            icon_button_1         = '@01@'
            text_button_2         = 'No'(f03)
            icon_button_2         = '@02@'
            default_button        = '1'
            display_cancel_button = ' '
          IMPORTING
            answer                = lv_popup_answer.
      ENDIF.
      IF lv_popup_answer = '1'.
        LOOP AT li_alv_rows ASSIGNING <lk_alv_rows>.
          READ TABLE gi_intfms ASSIGNING <lk_intfms> INDEX <lk_alv_rows>.
          IF sy-subrc = 0.
            lv_table_count = lv_table_count + 1.
            CASE <lk_intfms>-interface_type.
              WHEN '0'.
                PERFORM data_process_proxy_restart USING <lk_intfms>.
              WHEN '1'.
                APPEND <lk_intfms> TO li_intfms.
            ENDCASE.
          ENDIF.
        ENDLOOP.

        IF li_intfms[] IS NOT INITIAL.
          PERFORM data_process_idoc_restart USING li_intfms[].
        ENDIF.

        MESSAGE s020(xms_moni) WITH lv_table_count 'messages copied & processed'.
      ENDIF.

    WHEN 'REFRSH'.
      CLEAR gi_intfms[].
      PERFORM data_retrieve.
      go_main_alv_table->refresh( ).

    WHEN 'TREE_SEARCH'.
      IF go_proxy_tree IS NOT INITIAL.
        PERFORM data_display_tree_search CHANGING go_proxy_tree.
      ENDIF.
      IF go_idoc_tree IS NOT INITIAL.
        PERFORM data_display_tree_search CHANGING go_idoc_tree.
      ENDIF.

    WHEN 'TREE_SEARCH_NEXT'.
      IF go_proxy_tree IS NOT INITIAL.
        PERFORM data_display_tree_search_next CHANGING go_proxy_tree.
      ENDIF.
      IF go_idoc_tree IS NOT INITIAL.
        PERFORM data_display_tree_search_next CHANGING go_idoc_tree.
      ENDIF.

    WHEN 'TREE_HIDE_EMPTY_FIELD'.
      IF go_idoc_tree IS NOT INITIAL.
        PERFORM data_display_tree_search_next CHANGING go_idoc_tree.
      ENDIF.
    WHEN 'TREE_SHOW_EMPTY_FIELD'.
      IF go_idoc_tree IS NOT INITIAL.
        PERFORM data_display_tree_search_next CHANGING go_idoc_tree.
      ENDIF.

    WHEN 'PROXY_XML'.
      IF go_proxy_tree IS NOT INITIAL.
        PERFORM data_display_proxy_tree2xml USING go_proxy_tree CHANGING gv_xml_document.
      ENDIF.
      PERFORM data_display_proxy_xml USING gk_intfms.

    WHEN 'PROXY_TREE'.
      IF go_proxy_xml IS NOT INITIAL.
        gv_xml_document = go_proxy_xml->get_xstring( ).
      ENDIF.
      PERFORM data_display_proxy_tree USING gk_intfms.


      "XML Commands
    WHEN 'PROXY_XML_TOGGLE'.
      IF go_proxy_xml IS NOT INITIAL.
        IF go_proxy_xml->get_change_mode( ) = abap_true.
          go_proxy_xml->set_change_mode( abap_false ).
        ELSE.
          go_proxy_xml->pretty_print( ).
          go_proxy_xml->set_change_mode( abap_true ).
        ENDIF.
      ENDIF.

    WHEN 'PROXY_XML_STORE'.
      IF go_proxy_xml IS NOT INITIAL.
        DATA : lv_payload TYPE xstring.
        lv_payload = go_proxy_xml->get_xstring( ).
        TRY.
            cl_proxy_adapter_test=>download_payload( lv_payload ).
          CATCH cx_proxy_gen_error.                     "#EC NO_HANDLER
        ENDTRY.
      ENDIF.

    WHEN 'PROXY_XML_PRETTY'.
      IF go_proxy_xml IS NOT INITIAL.
        go_proxy_xml->pretty_print( ).
      ENDIF.

    WHEN 'EDITPROCESS'.
      CLEAR lv_popup_answer.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          text_question         = 'Are you sure you want to process this message ?'(f05)
          text_button_1         = 'Yes'(f02)
          icon_button_1         = '@01@'
          text_button_2         = 'No'(f03)
          icon_button_2         = '@02@'
          default_button        = '1'
          display_cancel_button = ' '
        IMPORTING
          answer                = lv_popup_answer.
      IF lv_popup_answer = '1'.
        CASE gk_intfms-interface_type.
          WHEN '0'.
            IF go_proxy_xml IS NOT INITIAL.
              gv_xml_document = go_proxy_xml->get_xstring( ).
            ENDIF.
            IF go_proxy_tree IS NOT INITIAL.
              PERFORM data_display_proxy_tree2xml USING go_proxy_tree CHANGING gv_xml_document.
            ENDIF.
            PERFORM data_process_proxy_editproc USING gk_intfms.
            MESSAGE s020(xms_moni) WITH 'XML message saved & processed'.
          WHEN '1'.
            IF go_idoc_tree IS NOT INITIAL.
              PERFORM data_process_idoc_edittree.
              MESSAGE s020(xms_moni) WITH 'IDoc message saved & processed'.
            ENDIF.
        ENDCASE.
      ENDIF.

      "Tree Commands
    WHEN 'NODEEXPAND'.
      IF go_idoc_tree IS NOT INITIAL.
        go_idoc_tree->get_selected_node( IMPORTING ed_node_id = lv_tree_node_id ).
        go_idoc_tree->expand_node( id_node_id = lv_tree_node_id id_levels = lcl_gui_tree=>cd_entire_subtree ).
      ENDIF.
      IF go_proxy_tree IS NOT INITIAL.
        go_proxy_tree->get_selected_node( IMPORTING ed_node_id = lv_tree_node_id ).
        go_proxy_tree->expand_node( id_node_id = lv_tree_node_id id_levels = lcl_gui_tree=>cd_entire_subtree ).
      ENDIF.

    WHEN 'NODECOLLAPSE'.
      IF go_idoc_tree IS NOT INITIAL.
        go_idoc_tree->get_selected_node( IMPORTING ed_node_id = lv_tree_node_id ).
        IF lv_tree_node_id IS INITIAL.
          li_tree_nodes = go_idoc_tree->get_nodes( ).
          LOOP AT li_tree_nodes ASSIGNING <lk_tree_nodes> WHERE parent_node IS INITIAL.
            go_idoc_tree->collapse_node( id_node_id = <lk_tree_nodes>-node_id ).
          ENDLOOP.
        ELSE.
          go_idoc_tree->collapse_node( id_node_id = lv_tree_node_id ).
        ENDIF.
      ENDIF.
      IF go_proxy_tree IS NOT INITIAL.
        go_proxy_tree->get_selected_node( IMPORTING ed_node_id = lv_tree_node_id ).
        IF lv_tree_node_id IS INITIAL.
          li_tree_nodes = go_proxy_tree->get_nodes( ).
          LOOP AT li_tree_nodes ASSIGNING <lk_tree_nodes> WHERE parent_node IS INITIAL.
            go_proxy_tree->collapse_node( id_node_id = <lk_tree_nodes>-node_id ).
          ENDLOOP.
        ELSE.
          go_proxy_tree->collapse_node( id_node_id = lv_tree_node_id ).
        ENDIF.
      ENDIF.

  ENDCASE.
ENDFORM.                    " DATA_DISPLAY_COMMANDS
*&---------------------------------------------------------------------*
*&      Form  data_display_tree_commands
*&---------------------------------------------------------------------*
FORM data_display_tree_commands USING lo_tree            TYPE REF TO lcl_gui_tree
                                      lv_tree_nodeid_sel TYPE uac_node_id
                                      lv_okcode          TYPE uac_menu_function_code.
  TYPES: BEGIN OF ltk_idoc_segments,
           segtyp TYPE edilsegtyp,
         END OF ltk_idoc_segments.
  DATA: lv_tree_nodeid_new      TYPE uac_node_id,
        lv_tree_nodeid_new_elem TYPE uac_node_id,
        lv_tree_nodeid_new_attr TYPE uac_node_id.
  DATA: lk_tree_node_new        TYPE uac_s_node_long,
        lk_tree_node_sel        TYPE uac_s_node_long,
        lk_tree_node_parent     TYPE uac_s_node_long,
        lk_tree_node_last       TYPE uac_s_node_long.

  DATA: li_idoc_segments        TYPE STANDARD TABLE OF ltk_idoc_segments.
  DATA: lv_idoc_parseg          TYPE idocsyn-parseg.
  DATA: lk_idoc_control         TYPE edidc.
  DATA: li_idoc_segment_sel     TYPE rsdm_f4_return_values.
  DATA: li_tree_nodes           TYPE uac_t_node_long.
  DATA: lv_idoc_node_curr       TYPE uac_node_id,
        lv_idoc_node_last       TYPE uac_node_id.
  DATA: lo_idoc_segment         TYPE REF TO data.
  DATA: lo_data_typedescr       TYPE REF TO cl_abap_typedescr.
  DATA: li_data_fields          TYPE dd_x031l_table.
  DATA: lv_idoc_last_num        TYPE n LENGTH 6.
  FIELD-SYMBOLS <lk_data_fields>      TYPE x031l.
  FIELD-SYMBOLS:<lk_idoc_segment>     TYPE any,
                <lk_idoc_field>       TYPE any.
  FIELD-SYMBOLS <lk_idoc_segment_sel> TYPE ddshretval.

  CASE lv_okcode.
    WHEN 'XML_DUPNODE' OR 'IDOC_DUPNODE'.
      lv_tree_nodeid_new = lo_tree->copy_subtree( id_source_node_id = lv_tree_nodeid_sel id_target_node_id = lv_tree_nodeid_sel id_with_cells = '1' ).
      lo_tree->change_node( id_node_id = lv_tree_nodeid_new id_editable = '1' id_u_editable = '1' id_style_id = lk_tree_node_new-style_id ).
      lo_tree->expand_node( id_node_id = lv_tree_nodeid_new ).
      lo_tree->set_selected_node( id_node_id = lv_tree_nodeid_new ).
    WHEN 'XML_DELNODE' OR 'IDOC_DELNODE'.
      lo_tree->remove_subtree( id_node_id = lv_tree_nodeid_sel ).
    WHEN 'XML_ADDELEM'.
      lv_tree_nodeid_new_elem = lo_tree->add_node( id_relat_node = lv_tree_nodeid_sel id_relationship = lcl_gui_tree=>cd_following_sibling id_value = ''  id_is_leaf = '1'  id_editable = '1' id_style_id = 'XML_ELEM' ).
      lo_tree->add_cell( id_node_id = lv_tree_nodeid_new_elem id_column_id = '1' id_editable = '1' id_value = '' ).
      lo_tree->set_selected_node( id_node_id = lv_tree_nodeid_new_elem ).
    WHEN 'XML_ADDELEM2'.
      lo_tree->get_node( EXPORTING id_node_id = lv_tree_nodeid_sel IMPORTING es_node = lk_tree_node_sel ).
      IF lk_tree_node_sel-style_id = 'XML_ELEM'.
        lo_tree->change_node( id_node_id = lv_tree_nodeid_sel id_editable = '0' id_u_editable = '0' id_style_id = 'XML_STRUC' ).
        lo_tree->change_cell( id_node_id = lv_tree_nodeid_sel id_column_id = '1' id_value = '' id_u_value = '1' id_editable = '0' id_u_editable = '1' ).
      ENDIF.
      IF lk_tree_node_sel-is_leaf = '1'.
        lo_tree->change_node_leaf( id_node_id = lv_tree_nodeid_sel id_is_leaf = '0' ).
        lv_tree_nodeid_new = lo_tree->copy_subtree( id_source_node_id = lv_tree_nodeid_sel id_target_node_id = lv_tree_nodeid_sel id_with_cells = '1' ).
        lv_tree_nodeid_new_elem = lo_tree->add_node( id_relat_node = lv_tree_nodeid_new id_relationship = lcl_gui_tree=>cd_last_child id_value = ''  id_is_leaf = '1'  id_editable = '1' id_style_id = 'XML_ELEM' ).
        lo_tree->add_cell( id_node_id = lv_tree_nodeid_new_elem id_column_id = '1' id_editable = '1' id_value = '' ).
        lo_tree->expand_node( id_node_id = lv_tree_nodeid_new ).
        lo_tree->remove_subtree( id_node_id = lv_tree_nodeid_sel ).
        lo_tree->set_selected_node( id_node_id = lv_tree_nodeid_new_elem ).
      ELSE.
        lv_tree_nodeid_new_elem = lo_tree->add_node( id_relat_node = lv_tree_nodeid_sel id_relationship = lcl_gui_tree=>cd_last_child id_value = ''  id_is_leaf = '1'  id_editable = '1' id_style_id = 'XML_ELEM' ).
        lo_tree->add_cell( id_node_id = lv_tree_nodeid_new_elem id_column_id = '1' id_editable = '1' id_value = '' ).
        lo_tree->expand_node( id_node_id = lv_tree_nodeid_new ).
        lo_tree->set_selected_node( id_node_id = lv_tree_nodeid_new_elem ).
      ENDIF.
    WHEN 'XML_ADDATTR'.
      lo_tree->get_node( EXPORTING id_node_id = lv_tree_nodeid_sel IMPORTING es_node = lk_tree_node_sel ).
      IF lk_tree_node_sel-is_leaf = '1'.
        lo_tree->change_node_leaf( id_node_id = lv_tree_nodeid_sel id_is_leaf = '0' ).
        lv_tree_nodeid_new = lo_tree->copy_subtree( id_source_node_id = lv_tree_nodeid_sel id_target_node_id = lv_tree_nodeid_sel id_with_cells = '1' ).
        lv_tree_nodeid_new_attr = lo_tree->add_node( id_relat_node = lv_tree_nodeid_new id_relationship = lcl_gui_tree=>cd_last_child id_value = ''  id_is_leaf = '1'  id_editable = '1' id_style_id = 'XML_ATTR' ).
        lo_tree->add_cell( id_node_id = lv_tree_nodeid_new_attr id_column_id = '1' id_editable = '1' id_value = '' ).
        lo_tree->expand_node( id_node_id = lv_tree_nodeid_new ).
        lo_tree->remove_subtree( id_node_id = lv_tree_nodeid_sel ).
        lo_tree->set_selected_node( id_node_id = lv_tree_nodeid_new_attr ).
      ELSE.
        lv_tree_nodeid_new_attr = lo_tree->add_node( id_relat_node = lv_tree_nodeid_sel id_relationship = lcl_gui_tree=>cd_last_child id_value = ''  id_is_leaf = '1'  id_editable = '1' id_style_id = 'XML_ATTR' ).
        lo_tree->add_cell( id_node_id = lv_tree_nodeid_new_attr id_column_id = '1' id_editable = '1' id_value = '' ).
        lo_tree->expand_node( id_node_id = lv_tree_nodeid_new ).
        lo_tree->set_selected_node( id_node_id = lv_tree_nodeid_new_attr ).
      ENDIF.
    WHEN 'IDOC_ADDSTR1' OR 'IDOC_ADDSTR2'.
      lo_tree->get_node( EXPORTING id_node_id = lv_tree_nodeid_sel IMPORTING es_node = lk_tree_node_sel ).
      IF lk_tree_node_sel-style_id EQ 'IDOC_NODE'.
        lo_tree->get_node( EXPORTING id_node_id = lk_tree_node_sel-parent_node  IMPORTING es_node = lk_tree_node_sel ).
      ENDIF.

      IF lv_okcode = 'IDOC_ADDSTR1'.
        lo_tree->get_node( EXPORTING id_node_id = lk_tree_node_sel-parent_node  IMPORTING es_node = lk_tree_node_parent ).
      ENDIF.

      IF lk_tree_node_sel-value NE 'Control Record' OR lk_tree_node_sel-value NE 'Status Records'.
        SELECT SINGLE * INTO lk_idoc_control FROM edidc WHERE docnum = gk_intfms-idoc_docnum.

        IF lv_okcode = 'IDOC_ADDSTR1'.
          IF lk_tree_node_parent-value EQ 'Data Records'.
            SELECT segtyp INTO TABLE li_idoc_segments FROM idocsyn WHERE idoctyp = lk_idoc_control-idoctp AND nr = 1.
          ELSE.
            IF lk_tree_node_parent-value IS NOT INITIAL.
              lv_idoc_parseg = lk_tree_node_parent-value.
              SELECT segtyp INTO TABLE li_idoc_segments FROM idocsyn WHERE idoctyp = lk_idoc_control-idoctp AND parseg = lv_idoc_parseg.
            ENDIF.
          ENDIF.
        ELSE.
          IF lk_tree_node_sel-value = 'Data Records'.
            SELECT segtyp INTO TABLE li_idoc_segments FROM idocsyn WHERE idoctyp = lk_idoc_control-idoctp AND nr = 1.
          ELSE.
            IF lk_tree_node_sel-value IS NOT INITIAL.
              lv_idoc_parseg = lk_tree_node_sel-value.
              SELECT segtyp INTO TABLE li_idoc_segments FROM idocsyn WHERE idoctyp = lk_idoc_control-idoctp AND parseg = lv_idoc_parseg.
            ENDIF.
          ENDIF.
        ENDIF.

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield   = 'SEGTYP'
            value_org  = 'S'
          TABLES
            value_tab  = li_idoc_segments
            return_tab = li_idoc_segment_sel.
      ENDIF.

      READ TABLE li_idoc_segment_sel ASSIGNING <lk_idoc_segment_sel> INDEX 1.
      IF sy-subrc = 0.
        CREATE DATA lo_idoc_segment TYPE (<lk_idoc_segment_sel>-fieldval).
        ASSIGN lo_idoc_segment->* TO <lk_idoc_segment>.
        lo_data_typedescr = cl_abap_typedescr=>describe_by_name( <lk_idoc_segment_sel>-fieldval ).
        li_data_fields = lo_data_typedescr->get_ddic_object( ).
        li_tree_nodes = lo_tree->get_nodes( ).
        DELETE li_tree_nodes WHERE node_id NP 'EDIDD*'.
        SORT li_tree_nodes BY node_id DESCENDING.
        READ TABLE li_tree_nodes INTO lk_tree_node_last INDEX 1.
        lv_idoc_last_num = lk_tree_node_last-node_id+6 + 1.
        CONCATENATE 'EDIDD' lv_idoc_last_num INTO lv_idoc_node_curr.
        IF lv_okcode = 'IDOC_ADDSTR1'.
          lo_tree->add_node( id_node_id = lv_idoc_node_curr id_relat_node = lk_tree_node_sel-node_id id_relationship = lcl_gui_tree=>cd_following_sibling id_value = <lk_idoc_segment_sel>-fieldval id_style_id = 'IDOC_STRUC' ).
        ELSE.
          lo_tree->add_node( id_node_id = lv_idoc_node_curr id_relat_node = lk_tree_node_sel-node_id id_relationship = lcl_gui_tree=>cd_last_child id_value = <lk_idoc_segment_sel>-fieldval id_style_id = 'IDOC_STRUC' ).
        ENDIF.
        LOOP AT li_data_fields ASSIGNING <lk_data_fields>.
          ASSIGN COMPONENT <lk_data_fields>-fieldname OF STRUCTURE <lk_idoc_segment> TO <lk_idoc_field>.
          lv_idoc_node_last = lo_tree->add_node( id_value = <lk_data_fields>-fieldname id_relat_node = lv_idoc_node_curr id_relationship = lcl_gui_tree=>cd_last_child id_is_leaf = '1' id_style_id = 'IDOC_NODE' ).
          lo_tree->add_cell( id_node_id = lv_idoc_node_last id_column_id = '1' id_editable = '1' id_value = '' id_style_id = 'CELL1' ).
        ENDLOOP.
        lo_tree->expand_node( id_node_id = lv_idoc_node_curr ).
      ENDIF.
  ENDCASE.
*  lo_tree->update_frontend( ).
ENDFORM.                    "data_display_tree_commands
*&---------------------------------------------------------------------*
*&      Form  data_display_idoc_tree
*&---------------------------------------------------------------------*
FORM data_display_idoc_tree USING lk_intfms TYPE gtk_intfms.
  DATA: li_idoc_toolbar_event   TYPE cntl_simple_events,
        lk_idoc_toolbar_event   TYPE cntl_simple_event.
  DATA: lo_idoc_toolbar_handle   TYPE REF TO lcl_tool_events_handle.
  DATA: li_idoc_data            TYPE STANDARD TABLE OF edid4.
  DATA: li_idoc_status          TYPE STANDARD TABLE OF edids.
  DATA: lk_idoc_control         TYPE edidc.
  DATA: lo_idoc_segment         TYPE REF TO data.
  DATA: lo_data_typedescr       TYPE REF TO cl_abap_typedescr.
  DATA: li_data_fields          TYPE dd_x031l_table.
  DATA: lv_idoc_node_curr       TYPE uac_node_id,
        lv_idoc_node_main       TYPE uac_node_id,
        lv_idoc_node_last       TYPE uac_node_id.
  DATA: lv_idoc_status_statva   TYPE edi_statva.
  DATA: lv_idoc_status_stalight TYPE edi_slight.
  DATA: lv_idoc_status_node     TYPE string,
        lv_idoc_status_nodetext TYPE string,
        lv_idoc_status_nodedate TYPE string.
  DATA: lv_idoc_status_nodetime TYPE c LENGTH 8.
  DATA: lv_tree_editable        TYPE uac_flag.
  FIELD-SYMBOLS: <lk_idoc_data>        TYPE edid4.
  FIELD-SYMBOLS: <lk_idoc_status>      TYPE edids.
  FIELD-SYMBOLS: <lk_data_fields>      TYPE x031l.
  FIELD-SYMBOLS: <lk_idoc_field>       TYPE any,
                 <lk_idoc_segment>     TYPE any.

  AUTHORITY-CHECK OBJECT 'ZBC_INTF'
         ID '/PSTECH/II' FIELD lk_intfms-interface_id
         ID 'ACTVT' FIELD '02'.
  IF sy-subrc = 0.
    lv_tree_editable = '1'.
  ELSE.
    lv_tree_editable = '0'.
  ENDIF.


  CLEAR: gi_tree_search_result, gv_tree_search_index.
  IF go_log_table IS NOT INITIAL OR go_proxy_xml IS NOT INITIAL OR go_proxy_tree IS NOT INITIAL.
    go_main_split_container->remove_control( row = 1 column = 2 ).
    FREE: go_main_view_container, go_log_table, go_proxy_xml, go_proxy_tree, go_idoc_tree.
  ENDIF.


  IF go_main_view_container IS INITIAL.
    go_main_view_container = go_main_split_container->get_container( row = 1 column = 2 ).
    CLEAR: go_view_split_container, go_view_tool_container, go_view_obj_container, go_view_toolbar.
  ENDIF.

  IF go_view_split_container IS INITIAL.
    CREATE OBJECT go_view_split_container
      EXPORTING
        parent  = go_main_view_container
        rows    = 2
        columns = 1.
    go_view_split_container->set_row_mode(   mode = cl_gui_splitter_container=>mode_absolute ).
    go_view_split_container->set_row_sash(   id = 1 type = cl_gui_splitter_container=>type_sashvisible value = cl_gui_splitter_container=>false ).
    go_view_split_container->set_row_sash(   id = 1 type = cl_gui_splitter_container=>type_movable     value = cl_gui_splitter_container=>false ).
    go_view_split_container->set_row_height( id = 1 height = 24 ).
    go_view_split_container->set_border( border = ' ' ).
    go_view_tool_container  = go_view_split_container->get_container( row = 1 column = 1 ).
    go_view_obj_container  = go_view_split_container->get_container( row = 2 column = 1 ).

    CREATE OBJECT go_view_toolbar
      EXPORTING
        parent = go_view_tool_container.
    go_view_toolbar->add_button_group( gi_idoc_tree_button ).
    lk_idoc_toolbar_event-eventid = cl_gui_toolbar=>m_id_function_selected.  lk_idoc_toolbar_event-appl_event = ' '.
    APPEND lk_idoc_toolbar_event TO li_idoc_toolbar_event.
    go_view_toolbar->set_registered_events( li_idoc_toolbar_event ).
    CREATE OBJECT lo_idoc_toolbar_handle.
    SET HANDLER lo_idoc_toolbar_handle->command     FOR go_view_toolbar.
  ENDIF.

  IF go_idoc_tree IS NOT INITIAL.
    go_idoc_tree->free( ).
    CLEAR go_idoc_tree.
  ENDIF.

  IF go_idoc_tree IS INITIAL.
    CREATE OBJECT go_idoc_tree
      EXPORTING
        id_parent    = go_view_obj_container
        id_tree_text = 'IDoc'.
    PERFORM data_display_idoc_tree_config USING lv_tree_editable.
  ENDIF.

  gk_intfms = lk_intfms.

  "Create Main IDoc Node
  go_idoc_tree->add_node( id_node_id = 'IDOC' id_relationship = lcl_gui_tree=>cd_last_child id_value = 'IDoc' id_style_id = 'IDOC' ).
  go_idoc_tree->add_cell( id_node_id = 'IDOC' id_column_id = '1' id_editable = '0' id_value = lk_intfms-idoc_docnum ).

  "Create IDoc Control Record
  SELECT SINGLE * INTO lk_idoc_control    FROM edidc WHERE docnum = lk_intfms-idoc_docnum.
  lo_data_typedescr = cl_abap_typedescr=>describe_by_name( 'EDIDC' ).
  li_data_fields = lo_data_typedescr->get_ddic_object( ).
  go_idoc_tree->add_node( id_node_id = 'EDIDC' id_relat_node = 'IDOC' id_relationship = lcl_gui_tree=>cd_last_child id_value = 'Control Record' id_style_id = 'IDOC_STRUC' ).
  LOOP AT li_data_fields ASSIGNING <lk_data_fields> FROM 2.
    ASSIGN COMPONENT <lk_data_fields>-fieldname OF STRUCTURE lk_idoc_control TO <lk_idoc_field>.
    lv_idoc_node_last = go_idoc_tree->add_node( id_value = <lk_data_fields>-fieldname id_relat_node = 'EDIDC' id_relationship = lcl_gui_tree=>cd_last_child id_is_leaf = '1' id_style_id = 'IDOC_NODE' ).
    go_idoc_tree->add_cell( id_node_id = lv_idoc_node_last id_column_id = '1' id_editable = lv_tree_editable id_value = <lk_idoc_field> ).
  ENDLOOP.

  "Create IDoc Data Records
  SELECT * INTO TABLE li_idoc_data FROM edid4 WHERE docnum = lk_intfms-idoc_docnum.
  go_idoc_tree->add_node( id_node_id = 'EDIDD000000' id_relat_node = 'IDOC' id_relationship = lcl_gui_tree=>cd_last_child id_value = 'Data Records' id_style_id = 'IDOC_STRUC' ).
  LOOP AT li_idoc_data ASSIGNING <lk_idoc_data>.
    CLEAR lo_idoc_segment.

    CREATE DATA lo_idoc_segment TYPE (<lk_idoc_data>-segnam).
    ASSIGN lo_idoc_segment->* TO <lk_idoc_segment>.
    <lk_idoc_segment> = <lk_idoc_data>-sdata.

    lo_data_typedescr = cl_abap_typedescr=>describe_by_name( <lk_idoc_data>-segnam ).
    li_data_fields = lo_data_typedescr->get_ddic_object( ).

    CONCATENATE 'EDIDD' <lk_idoc_data>-psgnum INTO lv_idoc_node_last.
    CONCATENATE 'EDIDD' <lk_idoc_data>-segnum INTO lv_idoc_node_curr.

    go_idoc_tree->add_node( id_node_id = lv_idoc_node_curr id_relat_node = lv_idoc_node_last id_relationship = lcl_gui_tree=>cd_last_child id_value = <lk_idoc_data>-segnam id_style_id = 'IDOC_STRUC' ).

    LOOP AT li_data_fields ASSIGNING <lk_data_fields>.
      ASSIGN COMPONENT <lk_data_fields>-fieldname OF STRUCTURE <lk_idoc_segment> TO <lk_idoc_field>.
      lv_idoc_node_last = go_idoc_tree->add_node( id_value = <lk_data_fields>-fieldname id_relat_node = lv_idoc_node_curr id_relationship = lcl_gui_tree=>cd_last_child id_is_leaf = '1' id_style_id = 'IDOC_NODE' ).
      go_idoc_tree->add_cell( id_node_id = lv_idoc_node_last id_column_id = '1' id_editable = lv_tree_editable id_value = <lk_idoc_field> id_style_id = 'CELL1' ).
    ENDLOOP.
  ENDLOOP.

  "IDoc Statuses
  SELECT * INTO TABLE li_idoc_status    FROM edids WHERE docnum = lk_intfms-idoc_docnum.
  SORT li_idoc_status BY countr DESCENDING.
  go_idoc_tree->add_node( id_node_id = 'EDIDS' id_relat_node = 'IDOC' id_relationship = lcl_gui_tree=>cd_last_child id_value = 'Status Records' id_style_id = 'IDOC_STRUC' ).
  LOOP AT li_idoc_status ASSIGNING <lk_idoc_status>.
    CLEAR: lv_idoc_status_statva, lv_idoc_status_stalight, lv_idoc_status_nodedate, lv_idoc_status_nodetime, lv_idoc_status_nodetext.
    SELECT SINGLE statva   INTO lv_idoc_status_statva   FROM teds3    WHERE status = <lk_idoc_status>-status.
    SELECT SINGLE stalight INTO lv_idoc_status_stalight FROM stalight WHERE statva = lv_idoc_status_statva.

    CALL FUNCTION 'CONVERSION_EXIT_MODAT_OUTPUT'
      EXPORTING
        input  = <lk_idoc_status>-logdat
      IMPORTING
        output = lv_idoc_status_nodedate.
    .
    CALL FUNCTION 'CONVERSION_EXIT_TIMLO_OUTPUT'
      EXPORTING
        input  = <lk_idoc_status>-logtim
      IMPORTING
        output = lv_idoc_status_nodetime.

    CONCATENATE <lk_idoc_status>-status lv_idoc_status_nodedate lv_idoc_status_nodetime INTO lv_idoc_status_node SEPARATED BY space.

    CASE lv_idoc_status_stalight.
      WHEN '1'.
        lv_idoc_node_last = go_idoc_tree->add_node( id_value = lv_idoc_status_node id_relat_node = 'EDIDS' id_relationship = lcl_gui_tree=>cd_last_child id_is_leaf = '1' id_style_id = 'EDIDS_W' ).
      WHEN '2'.
        lv_idoc_node_last = go_idoc_tree->add_node( id_value = lv_idoc_status_node id_relat_node = 'EDIDS' id_relationship = lcl_gui_tree=>cd_last_child id_is_leaf = '1' id_style_id = 'EDIDS_S' ).
      WHEN '3'.
        lv_idoc_node_last = go_idoc_tree->add_node( id_value = lv_idoc_status_node id_relat_node = 'EDIDS' id_relationship = lcl_gui_tree=>cd_last_child id_is_leaf = '1' id_style_id = 'EDIDS_E' ).
    ENDCASE.

    MESSAGE ID <lk_idoc_status>-stamid TYPE 'I' NUMBER <lk_idoc_status>-stamno WITH <lk_idoc_status>-stapa1 <lk_idoc_status>-stapa2 <lk_idoc_status>-stapa3 <lk_idoc_status>-stapa4 INTO lv_idoc_status_nodetext.
    go_idoc_tree->add_cell( id_node_id = lv_idoc_node_last id_column_id = '1' id_editable = '0' id_value = lv_idoc_status_nodetext ).
  ENDLOOP.

  go_idoc_tree->expand_node( id_node_id = 'EDIDD000000' id_levels  = 2 ).
  go_idoc_tree->expand_node( id_node_id = 'EDIDS' id_levels  = 1 ).
  go_idoc_tree->set_selected_node( id_node_id = 'IDOC' ).



  PERFORM data_display_tree_update CHANGING go_idoc_tree.
ENDFORM.                    "data_display_idoc
*&---------------------------------------------------------------------*
*&      Form  data_display_idoc_tree_config
*&---------------------------------------------------------------------*
FORM data_display_idoc_tree_config USING lv_edit TYPE uac_flag.
  DATA: lk_node_style       TYPE uac_s_node_style.
  DATA: lk_column_style     TYPE uac_s_column_style.
  DATA: lk_cell_style       TYPE uac_s_cell_style.
  DATA: lo_idoc_tree_handle TYPE REF TO lcl_tree_events_handle.

  lk_column_style-font_style        = 0.
  lk_column_style-alignment         = 0.
  go_idoc_tree->add_column_style( is_column_style = lk_column_style id_style_id = 'COL2' ).
  go_idoc_tree->add_column( id_value  = 'Value' id_style_id = 'COL2' ).

  lk_cell_style-class               = 1.
  lk_cell_style-font_style          = 0.
  lk_cell_style-alignment           = 0.
  lk_cell_style-decimals            = 0.
  lk_cell_style-max_length          = 255.
  lk_cell_style-text_color          = 'FF0000'.
  lk_cell_style-background_color    = 'FFFFFF'.
  go_idoc_tree->add_cell_style( is_cell_style = lk_cell_style id_style_id = 'CELL2' ).


  lk_cell_style-class               = 1.
  lk_cell_style-font_style          = 0.
  lk_cell_style-alignment           = 0.
  lk_cell_style-decimals            = 0.
  lk_cell_style-max_length          = 255.
  lk_cell_style-text_color          = '000000'.
  lk_cell_style-background_color    = 'FFFFFF'.
  go_idoc_tree->add_cell_style( is_cell_style = lk_cell_style id_style_id = 'CELL1' ).

  lk_node_style-font              = 1.
  lk_node_style-icon_open         = '@G5@'.
  lk_node_style-icon_closed       = '@G5@'.
  lk_node_style-text_color        = '000000'.
  lk_node_style-background_color  = 'FFFFFF'.
  lk_node_style-max_length        = 30.
  go_idoc_tree->add_node_style( is_node_style = lk_node_style id_style_id = 'IDOC' ).


  lk_node_style-font              = 1.
  lk_node_style-icon_open         = '@HP@'.
  lk_node_style-icon_closed       = '@HP@'.
  lk_node_style-text_color        = '000000'.
  lk_node_style-background_color  = 'FFFFFF'.
  lk_node_style-max_length        = 30.
  go_idoc_tree->add_node_style( is_node_style = lk_node_style id_style_id = 'IDOC_STRUC' ).

  lk_node_style-font              = 0.
  lk_node_style-icon_open         = '@HO@'.
  lk_node_style-icon_closed       = '@HO@'.
  lk_node_style-text_color        = '000000'.
  lk_node_style-background_color  = 'FFFFFF'.
  lk_node_style-max_length        = 30.
  go_idoc_tree->add_node_style( is_node_style = lk_node_style id_style_id = 'IDOC_NODE' ).

  lk_node_style-font              = 0.
  lk_node_style-icon_open         = '@5B@'.
  lk_node_style-icon_closed       = '@5B@'.
  lk_node_style-text_color        = '000000'.
  lk_node_style-background_color  = 'FFFFFF'.
  lk_node_style-max_length        = 2.
  go_idoc_tree->add_node_style( is_node_style = lk_node_style id_style_id = 'EDIDS_S' ).
  lk_node_style-font              = 0.
  lk_node_style-icon_open         = '@5D@'.
  lk_node_style-icon_closed       = '@5D@'.
  lk_node_style-text_color        = '000000'.
  lk_node_style-background_color  = 'FFFFFF'.
  lk_node_style-max_length        = 2.
  go_idoc_tree->add_node_style( is_node_style = lk_node_style id_style_id = 'EDIDS_W' ).
  lk_node_style-font              = 0.
  lk_node_style-icon_open         = '@5C@'.
  lk_node_style-icon_closed       = '@5C@'.
  lk_node_style-text_color        = '000000'.
  lk_node_style-background_color  = 'FFFFFF'.
  lk_node_style-max_length        = 2.
  go_idoc_tree->add_node_style( is_node_style = lk_node_style id_style_id = 'EDIDS_E' ).
  lk_node_style-font              = 0.
  lk_node_style-icon_open         = '@BZ@'.
  lk_node_style-icon_closed       = '@BZ@'.
  lk_node_style-text_color        = '000000'.
  lk_node_style-background_color  = 'FFFFFF'.
  lk_node_style-max_length        = 2.
  go_idoc_tree->add_node_style( is_node_style = lk_node_style id_style_id = 'EDIDS_I' ).

  go_idoc_tree->register_node_ctxtmn_requested( ).
  go_idoc_tree->register_node_ctxtmn_selected( ).
  go_idoc_tree->register_node_1st_open( ).

  CREATE OBJECT lo_idoc_tree_handle.
  IF lv_edit = '1'.
    SET HANDLER lo_idoc_tree_handle->node_menu_request     FOR go_idoc_tree.
    SET HANDLER lo_idoc_tree_handle->node_menu_select      FOR go_idoc_tree.
  ENDIF.
  SET HANDLER lo_idoc_tree_handle->node_open             FOR go_idoc_tree.
ENDFORM.                    "data_display_idoc_settings

*----------------------------------------------------------------------*
*       CLASS lcl_gui_tree IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_gui_tree IMPLEMENTATION.
  METHOD change_node_leaf.
    FIELD-SYMBOLS: <lk_node> TYPE uac_s_node_long.
    READ TABLE mt_nodes ASSIGNING <lk_node>  WITH TABLE KEY  tree_id = id_tree_id node_id = id_node_id.
    IF sy-subrc = 0.
      <lk_node>-is_leaf = id_is_leaf.
    ENDIF.
  ENDMETHOD.                    "change_node_leaf
  METHOD constructor.
    CALL METHOD super->constructor
      EXPORTING
        id_parent              = id_parent
        id_lifetime            = id_lifetime
        id_name                = id_name
        id_style               = id_style
        id_class_id            = id_class_id
        id_expand_no_children  = id_expand_no_children
      EXCEPTIONS
        error_cntl_create      = 1
        gui_type_not_supported = 2
        cntl_error             = 3
        OTHERS                 = 4.

    IF sy-subrc <> 0.
      RAISE error_cntl_create.
    ENDIF.

    CALL METHOD _add_tree
      EXPORTING
        id_column_text = id_tree_text
      IMPORTING
        ed_tree_id     = md_tree_id
      EXCEPTIONS
        cntl_error     = 1
        system_error   = 2
        OTHERS         = 3.

    CASE sy-subrc.
      WHEN 1. RAISE cntl_error.
      WHEN 2. RAISE system_error.
      WHEN 3. RAISE cntl_error.
    ENDCASE.

    CALL METHOD _display_tree
      EXPORTING
        id_multi_selection = id_multi_selection
      EXCEPTIONS
        cntl_error         = 1
        system_error       = 2.

    CASE sy-subrc.
      WHEN 1. RAISE cntl_error.
      WHEN 2. RAISE system_error.
    ENDCASE.
  ENDMETHOD.                    "constructor
  METHOD register_events .
    DATA: ld_application_event TYPE abap_bool.

    CASE application_event.
      WHEN true.  ld_application_event = abap_true.
      WHEN false. ld_application_event = abap_false.
    ENDCASE.

    DEFINE register.
      if &1 eq true.

        call method register_event_for_id
          exporting
            id_event_id   = &2
            id_appl_event = ld_application_event
          exceptions
            cntl_error    = 1.

        if sy-subrc ne 0.
          raise cntl_error.
        endif.

      endif.
    END-OF-DEFINITION.
    register     node_selected              ev_node_selected.
    register     node_double_click          ev_node_double_click.
    register     node_value_changed         ev_on_node_value_changed.
    register     node_context_menu          ev_node_context_menu_request.
    register     node_context_menu          ev_node_context_menu_selected.
    register     multi_selection_change     ev_multi_selection_change.
    register     keypressed                 ev_cell_keypressed.
    register     keypressed                 ev_node_keypressed.
    register     cell_selected              ev_cell_selected.
    register     cell_double_click          ev_cell_double_click.
    register     cell_value_changed         ev_on_cell_value_changed.
    register     cell_context_menu          ev_cell_context_menu_requested.
    register     cell_context_menu          ev_cell_context_menu_selected.
    register     cell_dropdown              ev_cell_dropdown_requested.
    register     cell_dropdown              ev_cell_dropdown_selected.
    register     column_double_click        ev_column_double_click.
    register     column_context_menu        ev_column_context_menu_request.
    register     column_context_menu        ev_column_context_menu_selectd.
    register     group_context_menu         ev_group_item_cntxt_request.
    register     group_context_menu         ev_group_item_cntxt_selected.
    register     sort_order_changed         ev_on_sort_order_changed.
    register     column_order_changed       ev_on_column_order_changed.
    register     group_changes              ev_group_item_added.
    register     group_changes              ev_group_item_removed.
    register     group_changes              ev_group_order_changed.
    register     control_context_menu       ev_ctrl_ctxt_menu_request.
    register     control_context_menu       ev_ctrl_ctxt_menu_selectd.
  ENDMETHOD.                    "

  METHOD set_styles.
    IF NOT it_node_style IS INITIAL.

      CALL METHOD send_table_to_dp
        EXPORTING
          p_property_name = 'NodeStyle'
          pt_data         = it_node_style
        EXCEPTIONS
          dp_error        = 1
          OTHERS          = 2.

      IF sy-subrc <> 0.
        RAISE cntl_error.
      ENDIF.

    ENDIF.


    IF NOT it_cell_style IS INITIAL.

      CALL METHOD send_table_to_dp
        EXPORTING
          p_property_name = 'CellStyle'
          pt_data         = it_cell_style
        EXCEPTIONS
          dp_error        = 1
          OTHERS          = 2.

      IF sy-subrc <> 0.
        RAISE cntl_error.
      ENDIF.

    ENDIF.

    IF NOT it_column_style IS INITIAL.
      CALL METHOD send_table_to_dp
        EXPORTING
          p_property_name = 'ColumnStyle'
          pt_data         = it_column_style
        EXCEPTIONS
          dp_error        = 1
          OTHERS          = 2.

      IF sy-subrc <> 0.
        RAISE cntl_error.
      ENDIF.

    ENDIF.

  ENDMETHOD.                    "set_styles
ENDCLASS.                    "lcl_gui_tree IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_alv_events_handle IMPLEMENTATION.
  METHOD command.
    PERFORM data_display_commands USING e_salv_function.
  ENDMETHOD.                    "on_user_command
  METHOD linkclick.
    FIELD-SYMBOLS: <lk_intfms> TYPE gtk_intfms.
    CASE column.
      WHEN 'INTF_MSGID_ICO'.
        CLEAR gv_xml_document.
        READ TABLE gi_intfms ASSIGNING <lk_intfms> INDEX row.
        IF sy-subrc = 0.
          CASE <lk_intfms>-interface_type.
            WHEN '0'.
              PERFORM data_display_proxy_tree USING <lk_intfms>.
            WHEN '1'.
              PERFORM data_display_idoc_tree USING <lk_intfms>.
          ENDCASE.
        ENDIF.

      WHEN 'INTF_ALOG_ICO'.
        IF go_log_table IS NOT INITIAL.
          CLEAR : gi_log_table.
          go_log_table->refresh( ).
        ENDIF.
        READ TABLE gi_intfms ASSIGNING <lk_intfms> INDEX row.
        IF sy-subrc = 0.
          PERFORM data_display_log_alv USING <lk_intfms>.
        ENDIF.

    ENDCASE.
  ENDMETHOD.                    "doubleclick
  METHOD doubleclick.
    FIELD-SYMBOLS: <lk_intfms> TYPE gtk_intfms.
    CASE column.
      WHEN 'INTERFACE_MSGID'.
        READ TABLE gi_intfms ASSIGNING <lk_intfms> INDEX row.
        IF sy-subrc = 0.
          PERFORM data_display_proxy_xml USING <lk_intfms>.
        ENDIF.
      WHEN 'LOG_MSGTY' OR 'LOG_MSGID' OR 'LOG_MSGNO' OR 'LOG_MSGTXT'.
        READ TABLE gi_intfms ASSIGNING <lk_intfms> INDEX row.
        IF sy-subrc = 0.
          PERFORM data_display_log_alv USING <lk_intfms>.
        ENDIF.
      WHEN 'INTERFACE_KEY1' OR 'INTERFACE_KEY2' OR 'INTERFACE_KEY3' OR 'INTERFACE_KEY4'.
    ENDCASE.
  ENDMETHOD.                    "doubleclick
ENDCLASS.                    "lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS lcl_tool_events_handle IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_tool_events_handle IMPLEMENTATION.
  METHOD command.
    PERFORM data_display_commands USING fcode.
  ENDMETHOD.                    "on_user_command
ENDCLASS.               "lcl_tool_events_handle
*----------------------------------------------------------------------*
*       CLASS lcl_tree_events_handle IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_tree_events_handle IMPLEMENTATION.
  METHOD node_open.
    IF go_proxy_tree IS NOT INITIAL.
      go_proxy_tree->column_optimize( id_column_id = '&Hierarchy' ).
      go_proxy_tree->column_optimize( id_column_id = '1' ).
    ENDIF.
    IF go_idoc_tree IS NOT INITIAL.
      go_idoc_tree->column_optimize( id_column_id = '&Hierarchy' ).
      go_idoc_tree->column_optimize( id_column_id = '1' ).
    ENDIF.
  ENDMETHOD.                    "node_open
  METHOD node_menu_request.
    IF go_proxy_tree IS NOT INITIAL.
      go_proxy_tree->display_context_menu( gi_proxy_tree_context ).
    ENDIF.
    IF go_idoc_tree IS NOT INITIAL.
      go_idoc_tree->display_context_menu( gi_idoc_tree_context ).
    ENDIF.
  ENDMETHOD.                    "node_menu_request
  METHOD node_menu_select.
    IF go_proxy_tree IS NOT INITIAL.
      PERFORM data_display_tree_commands USING go_proxy_tree node_id fcode.
    ENDIF.
    IF go_idoc_tree IS NOT INITIAL.
      PERFORM data_display_tree_commands USING go_idoc_tree node_id fcode.
    ENDIF.
  ENDMETHOD.                    "node_menu_select
ENDCLASS.                    "lcl_tree_events_handle IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS_IDOC_COPYPROC
*&---------------------------------------------------------------------*
FORM data_process_idoc_copyproc  USING lk_intfms TYPE gtk_intfms.
  DATA: lk_idoc_control          TYPE edidc.
  DATA: li_idoc_control          TYPE STANDARD TABLE OF edidc.
  DATA: li_idoc_edid4            TYPE STANDARD TABLE OF edid4.
  DATA: li_idoc_data             TYPE STANDARD TABLE OF edidd.
  DATA: lk_idoc_process_data_in  TYPE tede2.
  DATA: lk_intfms_new            TYPE gtk_intfms.
  DATA: lv_idoc_subrc            TYPE sysubrc.
  FIELD-SYMBOLS: <lk_idoc_edid4> TYPE edid4.
  FIELD-SYMBOLS: <lk_idoc_data>  TYPE edidd.

  "Create IDoc Control Record
  SELECT SINGLE * INTO lk_idoc_control  FROM edidc WHERE docnum = lk_intfms-idoc_docnum.
  CLEAR lk_idoc_control-docnum.

  "Create IDoc Data Records
  SELECT * INTO TABLE li_idoc_edid4 FROM edid4 WHERE docnum = lk_intfms-idoc_docnum..
  LOOP AT li_idoc_edid4 ASSIGNING <lk_idoc_edid4>.
    APPEND INITIAL LINE TO li_idoc_data ASSIGNING <lk_idoc_data>.
    <lk_idoc_data>-segnum = <lk_idoc_edid4>-segnum.
    <lk_idoc_data>-segnam = <lk_idoc_edid4>-segnam.
    <lk_idoc_data>-psgnum = <lk_idoc_edid4>-psgnum.
    <lk_idoc_data>-hlevel = <lk_idoc_edid4>-hlevel.
    <lk_idoc_data>-dtint2 = <lk_idoc_edid4>-dtint2.
    <lk_idoc_data>-sdata  = <lk_idoc_edid4>-sdata.
  ENDLOOP.

  CASE lk_idoc_control-direct.
    WHEN '1'.
      CALL FUNCTION 'IDOC_OUTBOUND_WRITE_TO_DB'
        TABLES
          int_edidd      = li_idoc_data
        CHANGING
          int_edidc      = lk_idoc_control
        EXCEPTIONS
          idoc_not_saved = 1
          OTHERS         = 2.

      APPEND lk_idoc_control TO li_idoc_control.
      CLEAR li_idoc_data[].

      CALL FUNCTION 'EDI_OUTPUT_NEW'
        EXPORTING
          onl_option = 'B'
          error_flag = ' '
        TABLES
          i_edidc    = li_idoc_control
          i_edidd    = li_idoc_data
        EXCEPTIONS
          OTHERS     = 0.
    WHEN '2'.
      CALL FUNCTION 'IDOC_INBOUND_WRITE_TO_DB'
        EXPORTING
          pi_return_data_flag     = ' '
        IMPORTING
          pe_idoc_number          = lk_idoc_control-docnum
          pe_state_of_processing  = lv_idoc_subrc
          pe_inbound_process_data = lk_idoc_process_data_in
        TABLES
          t_data_records          = li_idoc_data
        CHANGING
          pc_control_record       = lk_idoc_control
        EXCEPTIONS
          idoc_not_saved          = 1
          OTHERS                  = 2.
      IF lv_idoc_subrc = 0.
        APPEND lk_idoc_control TO li_idoc_control.
        CALL FUNCTION 'IDOC_START_INBOUND'
          EXPORTING
            pi_inbound_process_data = lk_idoc_process_data_in
            pi_called_online        = 'X'
            succ_show_flag          = 'X'
          TABLES
            t_control_records       = li_idoc_control
          EXCEPTIONS
            OTHERS                  = 1.
      ENDIF.
  ENDCASE.

  IF lk_intfms-interface_dir = 'O'.
    lk_intfms_new = lk_intfms.
    lk_intfms_new-interface_msgid = lk_idoc_control-docnum.
    lk_intfms_new-execute_date = sy-datum.
    lk_intfms_new-execute_time = sy-uzeit.
    MODIFY /pstech/bcintfms FROM lk_intfms_new.
  ENDIF.
ENDFORM.                    " DATA_PROCESS_IDOC_COPYPROC
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS_IDOC_EDITTREE
*&---------------------------------------------------------------------*
FORM data_process_idoc_edittree.
  DATA li_idoc_tree_nodes      TYPE uac_t_node_long.
  DATA lk_idoc_control         TYPE edidc.
  DATA li_idoc_control         TYPE STANDARD TABLE OF edidc.
  DATA li_idoc_data            TYPE STANDARD TABLE OF edidd.
  DATA lk_idoc_process_data_in TYPE tede2.
  DATA lv_idoc_subrc           TYPE sysubrc.
  DATA lk_idoc_data            TYPE edidd.
  DATA lv_idoc_tree_value      TYPE string.
  DATA lk_intfms_new           TYPE gtk_intfms.
  DATA lo_data_struc           TYPE REF TO data.
  FIELD-SYMBOLS: <lk_idoc_tree_nodes> TYPE uac_s_node_long.
  FIELD-SYMBOLS: <lv_data_field>      TYPE any,
                 <lv_data_struc>      TYPE any,
                 <lv_tree_value>      TYPE any.

  li_idoc_tree_nodes = go_idoc_tree->get_nodes( ).

  LOOP AT li_idoc_tree_nodes ASSIGNING <lk_idoc_tree_nodes>.
    CLEAR: lv_idoc_tree_value.
    CASE <lk_idoc_tree_nodes>-parent_node(5).
      WHEN 'EDIDC'.
        ASSIGN COMPONENT <lk_idoc_tree_nodes>-value OF STRUCTURE lk_idoc_control TO <lv_data_field>.
        go_idoc_tree->get_cell( EXPORTING id_node_id = <lk_idoc_tree_nodes>-node_id id_column_id = '1' IMPORTING ed_value = lv_idoc_tree_value ).
        <lv_data_field> = lv_idoc_tree_value.
      WHEN 'EDIDD'.
        IF <lk_idoc_tree_nodes>-node_id(5) = 'EDIDD'.
          IF lo_data_struc IS NOT INITIAL.
            lk_idoc_data-sdata = <lv_data_struc>.
            APPEND lk_idoc_data TO li_idoc_data.
            CLEAR: lo_data_struc.
            UNASSIGN: <lv_data_struc>.
          ENDIF.
          lk_idoc_data-segnam = <lk_idoc_tree_nodes>-value.
          CREATE DATA lo_data_struc TYPE (<lk_idoc_tree_nodes>-value).
          ASSIGN lo_data_struc->* TO  <lv_data_struc>.
        ELSE.
          ASSIGN COMPONENT <lk_idoc_tree_nodes>-value OF STRUCTURE <lv_data_struc> TO <lv_data_field>.
          go_idoc_tree->get_cell( EXPORTING id_node_id = <lk_idoc_tree_nodes>-node_id id_column_id = '1' IMPORTING ed_value = lv_idoc_tree_value ).
          <lv_data_field> = lv_idoc_tree_value.
        ENDIF.
    ENDCASE.
  ENDLOOP.

  "Append last node
  lk_idoc_data-sdata = <lv_data_struc>.
  APPEND lk_idoc_data TO li_idoc_data.
  CLEAR: lo_data_struc.
  UNASSIGN: <lv_data_struc>.

  "Remove Previous DocNum
  CLEAR lk_idoc_control-docnum.

  CASE lk_idoc_control-direct.
    WHEN '1'.
      CALL FUNCTION 'IDOC_OUTBOUND_WRITE_TO_DB'
        TABLES
          int_edidd      = li_idoc_data
        CHANGING
          int_edidc      = lk_idoc_control
        EXCEPTIONS
          idoc_not_saved = 1
          OTHERS         = 2.
      APPEND lk_idoc_control TO li_idoc_control.
      CLEAR li_idoc_data[].

      CALL FUNCTION 'EDI_OUTPUT_NEW'
        EXPORTING
          onl_option = 'B'
          error_flag = ' '
        TABLES
          i_edidc    = li_idoc_control
          i_edidd    = li_idoc_data
        EXCEPTIONS
          OTHERS     = 0.
    WHEN '2'.
      CALL FUNCTION 'IDOC_INBOUND_WRITE_TO_DB'
        EXPORTING
          pi_return_data_flag     = ' '
        IMPORTING
          pe_idoc_number          = lk_idoc_control-docnum
          pe_state_of_processing  = lv_idoc_subrc
          pe_inbound_process_data = lk_idoc_process_data_in
        TABLES
          t_data_records          = li_idoc_data
        CHANGING
          pc_control_record       = lk_idoc_control
        EXCEPTIONS
          idoc_not_saved          = 1
          OTHERS                  = 2.
      IF lv_idoc_subrc = 0.
        APPEND lk_idoc_control TO li_idoc_control.
        CALL FUNCTION 'IDOC_START_INBOUND'
          EXPORTING
            pi_inbound_process_data = lk_idoc_process_data_in
            pi_called_online        = 'X'
            succ_show_flag          = 'X'
          TABLES
            t_control_records       = li_idoc_control
          EXCEPTIONS
            OTHERS                  = 1.
      ENDIF.
  ENDCASE.

  IF gk_intfms-interface_dir = 'O'.
    lk_intfms_new = gk_intfms.
    lk_intfms_new-interface_msgid = lk_idoc_control-docnum.
    lk_intfms_new-execute_date = sy-datum.
    lk_intfms_new-execute_time = sy-uzeit.
    MODIFY /pstech/bcintfms FROM lk_intfms_new.
  ENDIF.

ENDFORM.                    " DATA_PROCESS_IDOC_EDITPROC
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS_IDOC_RESTART
*&---------------------------------------------------------------------*
FORM data_process_idoc_restart USING li_intfms TYPE gti_intfms.
  DATA li_idoc_docnum  TYPE bdrg_doc_tab.
  DATA li_idoc_control TYPE STANDARD TABLE OF edidc.
  FIELD-SYMBOLS: <lk_idoc_control> TYPE edidc.
  FIELD-SYMBOLS: <lk_idoc_docnum>  TYPE bdrg_doc.

  IF li_intfms[] IS NOT INITIAL.
    SELECT * INTO TABLE li_idoc_control FROM edidc FOR ALL ENTRIES IN li_intfms WHERE docnum = li_intfms-idoc_docnum.
  ENDIF.

  "Status 30
  CLEAR: li_idoc_docnum[].
  LOOP AT li_idoc_control ASSIGNING <lk_idoc_control> WHERE status = '30'.
    APPEND INITIAL LINE TO li_idoc_docnum ASSIGNING <lk_idoc_docnum>.
    <lk_idoc_docnum>-sign = 'I'.
    <lk_idoc_docnum>-option = 'EQ'.
    <lk_idoc_docnum>-low = <lk_idoc_control>-docnum.
  ENDLOOP.
  IF li_idoc_docnum[] IS NOT INITIAL.
    SUBMIT rseout00 WITH docnum IN li_idoc_docnum AND RETURN.
  ENDIF.

  " Status 02 04 05 25 29
  CLEAR: li_idoc_docnum[].
  LOOP AT li_idoc_control ASSIGNING <lk_idoc_control> WHERE status = '02' OR status = '04' OR status = '05' OR status = '25' OR status = '29'.
    APPEND INITIAL LINE TO li_idoc_docnum ASSIGNING <lk_idoc_docnum>.
    <lk_idoc_docnum>-sign = 'I'.
    <lk_idoc_docnum>-option = 'EQ'.
    <lk_idoc_docnum>-low = <lk_idoc_control>-docnum.
  ENDLOOP.
  IF li_idoc_docnum[] IS NOT INITIAL.
    SUBMIT rbdagain WITH so_docnu IN li_idoc_docnum WITH p_output = ' ' AND RETURN.
  ENDIF.

  " Status 26
  CLEAR: li_idoc_docnum[].
  LOOP AT li_idoc_control ASSIGNING <lk_idoc_control>  WHERE status = '26'.
    APPEND INITIAL LINE TO li_idoc_docnum ASSIGNING <lk_idoc_docnum>.
    <lk_idoc_docnum>-sign = 'I'.
    <lk_idoc_docnum>-option = 'EQ'.
    <lk_idoc_docnum>-low = <lk_idoc_control>-docnum.
  ENDLOOP.
  IF li_idoc_docnum[] IS NOT INITIAL.
    SUBMIT rbdsyneo WITH so_docnu IN li_idoc_docnum WITH p_output = ' '  AND RETURN.
  ENDIF.

  " Status 32
  CLEAR: li_idoc_docnum[].
  LOOP AT li_idoc_control ASSIGNING <lk_idoc_control> WHERE status = '32'.
    APPEND INITIAL LINE TO li_idoc_docnum ASSIGNING <lk_idoc_docnum>.
    <lk_idoc_docnum>-sign = 'I'.
    <lk_idoc_docnum>-option = 'EQ'.
    <lk_idoc_docnum>-low = <lk_idoc_control>-docnum.
  ENDLOOP.
  IF li_idoc_docnum[] IS NOT INITIAL.
    SUBMIT rbdagaie WITH p_idoc IN li_idoc_docnum WITH p_direct = '1' WITH p_output = ' ' AND RETURN.
  ENDIF.

  " Status 51
  CLEAR: li_idoc_docnum[].
  LOOP AT li_idoc_control ASSIGNING <lk_idoc_control>  WHERE status = '51'.
    APPEND INITIAL LINE TO li_idoc_docnum ASSIGNING <lk_idoc_docnum>.
    <lk_idoc_docnum>-sign = 'I'.
    <lk_idoc_docnum>-option = 'EQ'.
    <lk_idoc_docnum>-low = <lk_idoc_control>-docnum.
  ENDLOOP.
  IF li_idoc_docnum[] IS NOT INITIAL.
    SUBMIT rbdmani2 WITH so_docnu IN li_idoc_docnum WITH p_output = ' ' AND RETURN.
  ENDIF.

  " Status 56 61 63 65
  CLEAR: li_idoc_docnum[].
  LOOP AT li_idoc_control ASSIGNING <lk_idoc_control> WHERE status = '56' OR status = '61' OR status = '63' OR status = '65'.
    APPEND INITIAL LINE TO li_idoc_docnum ASSIGNING <lk_idoc_docnum>.
    <lk_idoc_docnum>-sign = 'I'.
    <lk_idoc_docnum>-option = 'EQ'.
    <lk_idoc_docnum>-low = <lk_idoc_control>-docnum.
  ENDLOOP.
  IF li_idoc_docnum[] IS NOT INITIAL.
    SUBMIT rbdagai2 WITH so_docnu IN li_idoc_docnum WITH p_output = ' ' AND RETURN.
  ENDIF.

  " Status 60
  CLEAR: li_idoc_docnum[].
  LOOP AT li_idoc_control ASSIGNING <lk_idoc_control> WHERE status = '60'.
    APPEND INITIAL LINE TO li_idoc_docnum ASSIGNING <lk_idoc_docnum>.
    <lk_idoc_docnum>-sign = 'I'.
    <lk_idoc_docnum>-option = 'EQ'.
    <lk_idoc_docnum>-low = <lk_idoc_control>-docnum.
  ENDLOOP.
  IF li_idoc_docnum[] IS NOT INITIAL.
    SUBMIT rbdsynei WITH so_docnu IN li_idoc_docnum WITH p_output = ' ' AND RETURN.
  ENDIF.

  " Status 64 66
  CLEAR: li_idoc_docnum[].
  LOOP AT li_idoc_control ASSIGNING <lk_idoc_control> WHERE status = '64' OR status = '66'.
    APPEND INITIAL LINE TO li_idoc_docnum ASSIGNING <lk_idoc_docnum>.
    <lk_idoc_docnum>-sign = 'I'.
    <lk_idoc_docnum>-option = 'EQ'.
    <lk_idoc_docnum>-low = <lk_idoc_control>-docnum.
  ENDLOOP.
  IF li_idoc_docnum[] IS NOT INITIAL.
    SUBMIT rbdapp01 WITH docnum IN li_idoc_docnum WITH p_output = ' ' AND RETURN.
  ENDIF.

  " Status 69
  CLEAR: li_idoc_docnum[].
  LOOP AT li_idoc_control ASSIGNING <lk_idoc_control> WHERE status = '69'.
    APPEND INITIAL LINE TO li_idoc_docnum ASSIGNING <lk_idoc_docnum>.
    <lk_idoc_docnum>-sign = 'I'.
    <lk_idoc_docnum>-option = 'EQ'.
    <lk_idoc_docnum>-low = <lk_idoc_control>-docnum.
  ENDLOOP.
  IF li_idoc_docnum[] IS NOT INITIAL.
    SUBMIT rbdagaie  WITH p_idoc IN li_idoc_docnum WITH p_direct = '2' WITH p_output = ' ' AND RETURN.
  ENDIF.
ENDFORM.                    "DATA_PROCESS_IDOC_RESTART
