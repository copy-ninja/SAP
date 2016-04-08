report  /pstech/bci_intf_monitor.

************************ Selection Screen ******************************
data: s_intfms type /pstech/bcintfms.
data: s_intfmm type /pstech/bcintfmm.
data: s_xmlstr type c length 255.
selection-screen begin of block b10 with frame title text-b10.
select-options: s_intfid for s_intfms-interface_id no intervals.
select-options: s_intfk1 for s_intfms-interface_key1.
select-options: s_intfk2 for s_intfms-interface_key2.
select-options: s_intfk3 for s_intfms-interface_key3.
select-options: s_intfk4 for s_intfms-interface_key4.
select-options: s_intfmi for s_intfms-interface_msgid.
select-options: s_intfty for s_intfmm-interface_type no intervals.
select-options: s_intfdi for s_intfmm-interface_dir no intervals.
selection-screen begin of line.
selection-screen comment (31) text-a02 for field s_exfrdt.
parameters: s_exfrdt type datum default sy-datum.
selection-screen comment 44(1) text-a03 for field s_exfrtm.
parameters: s_exfrtm type uzeit.
selection-screen end of line.
selection-screen begin of line.
selection-screen comment (31) text-a04 for field s_extodt.
parameters: s_extodt type datum default sy-datum.
selection-screen comment 44(1) text-a03 for field s_extotm.
parameters: s_extotm type uzeit default '240000'.
selection-screen end of line.
parameters: s_intfmx type tbmaxsel default '500'.
selection-screen begin of block b11 with frame title text-b11.
selection-screen begin of line.
selection-screen comment (8) text-a05 for field s_xmle1.
select-options: s_xmle1 for s_xmlstr visible length 15 no-extension no intervals lower case.
selection-screen comment 35(8) text-a06 for field s_xmlv1.
select-options: s_xmlv1 for s_xmlstr visible length 15 no-extension no intervals lower case.
parameters: s_xmlo1 as listbox visible length 8 default '1'.
selection-screen end of line.
selection-screen begin of line.
selection-screen comment (8) text-a05 for field s_xmle2.
select-options: s_xmle2 for s_xmlstr visible length 15 no-extension no intervals lower case.
selection-screen comment 35(8) text-a06 for field s_xmlv2.
select-options: s_xmlv2 for s_xmlstr visible length 15 no-extension no intervals lower case.
selection-screen end of line.
selection-screen end of block b11.
selection-screen end of block b10.
selection-screen begin of block b20 with frame title text-b20.
selection-screen begin of block b22 with frame title text-b22.
parameters: s_parrfc type spta_rfcgr memory id spta_rfcgr.
parameters: s_parmax type syindex default '0'.
selection-screen end of block b22.
selection-screen begin of block b21 with frame title text-b21.
parameters: s_alvvar type disvariant-variant.
selection-screen end of block b21.
selection-screen end of block b20.

************************ Data Definitions ****************************
class lcl_alv_events_handle definition.
  public section.
    methods command     for event added_function of cl_salv_events       importing e_salv_function.
    methods doubleclick for event double_click   of cl_salv_events_table importing row column.
    methods linkclick   for event link_click     of cl_salv_events_table importing row column.
endclass.                    "lcl_alv_events_handle DEFINITION
*
class lcl_tool_events_handle definition.
  public section.
    methods command     for event function_selected of cl_gui_toolbar importing fcode.
endclass.                    "lcl_tool_events_handle DEFINITION
*
class lcl_tree_events_handle definition.
  public section.
    methods node_open         for event open_folder_for_1st_time   of /pstech/cl_bc_gui_edit_tree importing tree_id node_id.
    methods node_menu_request for event node_context_menu_request  of /pstech/cl_bc_gui_edit_tree importing tree_id node_id.
    methods node_menu_select  for event node_context_menu_selected of /pstech/cl_bc_gui_edit_tree importing tree_id node_id fcode.
endclass.                    "lcl_tree_events_handle DEFINITION
type-pools: spta, vrm.
types: begin of gtk_intfms.
        include type /pstech/bcintfms.
types:  interface_type   type /pstech/bcintfmm-interface_type,
        interface_dir    type /pstech/bcintfmm-interface_dir,
        interface_desc   type /pstech/bcintfmm-interface_desc,
        key1_datatype    type /pstech/bcintfmm-key1_datatype,
        key2_datatype    type /pstech/bcintfmm-key2_datatype,
        key3_datatype    type /pstech/bcintfmm-key3_datatype,
        key4_datatype    type /pstech/bcintfmm-key4_datatype.
types:  intf_msgid_ico   type c length 4,
        intf_istat_ico   type c length 4, "interface stat
        intf_astat_ico   type c length 4, "application stat
        intf_alog_ico    type c length 4, "application log stat
        idoc_docnum      type edidc-docnum,
        proxy_msgguid    type sxmspmast-msgguid,
        proxy_pid        type sxmspmast-pid,
        proxy_msgstate   type sxmspmast-msgstate,
        proxy_eo_refid   type sxmspmast-eo_refid,
        proxy_eo_refval  type sxmspmast-eo_refval,
        proxy_exetimest  type sxmspmast-exetimest,
        proxy_inittimest type sxmspmast-inittimest,
        proxy_sendtimest type sxmspmast-sendtimest,
        log_msgty        type symsgty,
        log_msgid        type symsgid,
        log_msgno        type symsgno,
        log_msgtxt       type c length 500,
       end of gtk_intfms.
types: gti_intfms type standard table of gtk_intfms.
types: begin of gtk_log_table,
         log_ico         type c length 4,
         log_msg         type c length 500,
       end of gtk_log_table.
types: begin of gtk_parallel_data,
           import type gtk_intfms occurs 0,
           export type gtk_intfms occurs 0,
         end of gtk_parallel_data.
types: begin of gtk_parallel_struc,
           intfms type gtk_intfms,
           xmle1  like s_xmle1 occurs 0,
           xmlv1  like s_xmlv1 occurs 0,
           xmle2  like s_xmle2 occurs 0,
           xmlv2  like s_xmlv2 occurs 0,
           xmlo1  type c,
         end of gtk_parallel_struc.

data: gi_log_table                type standard table of gtk_log_table.
data: go_main_alv_table           type ref to cl_salv_table,
      go_log_table                type ref to cl_salv_table.
data: go_proxy_xml                type ref to cl_proxy_xml_edit.
data: go_idoc_tree                type ref to /pstech/cl_bc_gui_edit_tree,
      go_proxy_tree               type ref to /pstech/cl_bc_gui_edit_tree.
data: go_main_container           type ref to cl_gui_custom_container.
data: go_main_alv_container       type ref to cl_gui_container,
      go_main_view_container      type ref to cl_gui_container,
      go_view_tool_container      type ref to cl_gui_container,
      go_view_obj_container       type ref to cl_gui_container.
data: go_main_split_container     type ref to cl_gui_splitter_container,
      go_view_split_container     type ref to cl_gui_splitter_container.
data: go_view_toolbar             type ref to cl_gui_toolbar.

data: gi_proxy_xml_button         type ttb_button,
      gi_proxy_tree_button        type ttb_button,
      gi_idoc_tree_button         type ttb_button.

data: gi_proxy_tree_context       type uac_t_context_menu,
      gi_idoc_tree_context        type uac_t_context_menu.

data: gi_tree_search_result       type uac_t_node_long.
data: gv_tree_search_index        type sytabix.

data: gv_xml_document             type xstring.

data: gi_intfms          type standard table of gtk_intfms.
data: gk_intfms_par      type gtk_parallel_data.
data: gk_intfms          type gtk_intfms.

initialization.
  perform screen_alvvar_default changing s_alvvar.
  perform data_display_settings.

at selection-screen on value-request for s_alvvar.
  perform screen_alvvar_searchvalue changing s_alvvar.

at selection-screen on value-request for s_intfid-low.
  perform screen_intfid_searchvalue.

start-of-selection.
  perform data_retrieve.

  call screen 1001.

*&---------------------------------------------------------------------*
*&      Form  data_retrieve
*&---------------------------------------------------------------------*
form data_retrieve.
  data  li_intfms                type standard table of gtk_intfms.
  data  li_intfmm                type standard table of /pstech/bcintfmm.
  field-symbols: <lk_intfms>     type gtk_intfms.
  field-symbols: <lk_intfmm>     type /pstech/bcintfmm.

  select * into table li_intfmm from /pstech/bcintfmm
    where interface_id    in s_intfid   and
          interface_type  in s_intfty   and
          interface_dir   in s_intfdi.

  loop at li_intfmm assigning <lk_intfmm>.
    authority-check object 'ZBC_INTF'
             id '/PSTECH/II' field <lk_intfmm>-interface_id
             id 'ACTVT' field '03'.
    if sy-subrc ne 0.
      delete li_intfmm index sy-tabix.
    endif.
  endloop.
  sort li_intfmm by interface_id.

  if li_intfmm[] is not initial.
    select * up to s_intfmx rows
      into table gi_intfms from /pstech/bcintfms
      for all entries in li_intfmm
      where interface_id    eq li_intfmm-interface_id and
            interface_key1  in s_intfk1   and
            interface_key2  in s_intfk2   and
            interface_key3  in s_intfk3   and
            interface_key4  in s_intfk4   and
            interface_msgid in s_intfmi   and
        ( ( execute_date    eq s_exfrdt   and
            execute_time    ge s_exfrtm ) or
            execute_date    gt s_exfrdt ) and
        ( ( execute_date    eq s_extodt   and
            execute_time    le s_extotm ) or
            execute_date    lt s_extodt ).
  endif.

  loop at gi_intfms assigning <lk_intfms>.
    read table li_intfmm assigning <lk_intfmm> with key interface_id = <lk_intfms>-interface_id binary search.
    if sy-subrc = 0.
      <lk_intfms>-interface_type = <lk_intfmm>-interface_type.
      <lk_intfms>-interface_dir  = <lk_intfmm>-interface_dir.
      <lk_intfms>-interface_desc = <lk_intfmm>-interface_desc.
      <lk_intfms>-key1_datatype  = <lk_intfmm>-key1_datatype.
      <lk_intfms>-key2_datatype  = <lk_intfmm>-key2_datatype.
      <lk_intfms>-key3_datatype  = <lk_intfmm>-key3_datatype.
      <lk_intfms>-key4_datatype  = <lk_intfmm>-key4_datatype.
    endif.

    case <lk_intfms>-interface_type.
      when '0'.
        <lk_intfms>-proxy_msgguid  = <lk_intfms>-interface_msgid.
        <lk_intfms>-intf_msgid_ico = '@R4@'.
      when '1'.
        <lk_intfms>-idoc_docnum = <lk_intfms>-interface_msgid.
        <lk_intfms>-intf_msgid_ico = '@G5@'.
    endcase.
  endloop.

  if s_parrfc is initial.
    perform data_retrieve_serial.
  else.
    perform data_retrieve_parallel.
  endif.

  sort gi_intfms by execute_date ascending execute_time ascending proxy_sendtimest ascending.
endform.                    "data_retrieve
*&---------------------------------------------------------------------*
*&      Form  data_retrieve_parallel
*&---------------------------------------------------------------------*
form data_retrieve_parallel.
  gk_intfms_par-import[] = gi_intfms[].
  call function 'SPTA_PARA_PROCESS_START_2'
    exporting
      server_group             = s_parrfc
      max_no_of_tasks          = s_parmax
      before_rfc_callback_form = 'DATA_RETRIEVE_PARA_PIPELINING'
      in_rfc_callback_form     = 'DATA_RETRIEVE_PARA_PROCESS'
      after_rfc_callback_form  = 'DATA_RETRIEVE_PARA_SYNC'
      callback_prog            = sy-repid.
  gi_intfms[] = gk_intfms_par-export[].
endform.                    "data_retrieve_parallel
*&---------------------------------------------------------------------*
*&      Form  data_retrieve_para_pipelining
*&---------------------------------------------------------------------*
form data_retrieve_para_pipelining using lk_before_rfc_imp  type spta_t_before_rfc_imp
                               changing lk_before_rfc_exp  type spta_t_before_rfc_exp
                                        li_rfcdata         type spta_t_indxtab
                                        li_object_failed   type spta_t_failed_objects
                                        li_object_pending  type spta_t_pending_objects
                                        lk_user_param.
  data: lk_intfms_par type gtk_parallel_struc.

  if gk_intfms_par-import[] is initial.
    clear lk_before_rfc_exp-start_rfc. exit.
  else.
    lk_before_rfc_exp-start_rfc = 'X'.
  endif.

  read table gk_intfms_par-import into lk_intfms_par-intfms index 1.
  lk_intfms_par-xmle1[] = s_xmle1[].
  lk_intfms_par-xmlv1[] = s_xmlv1[].
  lk_intfms_par-xmle2[] = s_xmle2[].
  lk_intfms_par-xmlv2[] = s_xmlv2[].
  lk_intfms_par-xmlo1   = s_xmlo1.
  delete gk_intfms_par-import index 1.

  call function 'SPTA_INDX_PACKAGE_ENCODE'
    exporting
      data    = lk_intfms_par
    importing
      indxtab = li_rfcdata.

endform.                    "data_retrieve_para_pipelining
*&---------------------------------------------------------------------*
*&      Form  data_retrieve_para_process
*&---------------------------------------------------------------------*
form data_retrieve_para_process using lk_in_rfc_imp  type spta_t_in_rfc_imp
                            changing lk_in_rfc_exp  type spta_t_in_rfc_exp
                                     li_rfcdata     type spta_t_indxtab.
  data: lk_intfms_par type gtk_parallel_struc.
  data: lk_log_msg               type bal_s_msg.
  data: lk_log_msghndl           type balmsghndl.
  data  li_log_header            type balhdr_t.
  data: lk_log_header            type balhdr.
  data: lv_idoc_status           type edi_status.
  data: lv_idoc_status_statva    type edi_statva.
  data: lv_idoc_status_stalight  type edi_slight.
  data: li_msg_payload           type sxms_messagepayload_tab.
  data: lo_xms_persist_adm       type ref to cl_xms_persist_adm.
  data: li_xml_contents          type srt_xml_data_tab.
  data: lv_xml_tagname           type string,
        lv_xml_tagnamespace      type string.
  data: lv_xml_filter1 type c,
        lv_xml_filter2 type c.
  field-symbols: <lk_xml_contents> type srt_xml_data.
  field-symbols: <lk_msg_payload>  type sxms_messagepayload.


  call function 'SPTA_INDX_PACKAGE_DECODE'
    exporting
      indxtab = li_rfcdata
    importing
      data    = lk_intfms_par.

  "Proxy
  case lk_intfms_par-intfms-interface_type .
    when '0'.
      if ( lk_intfms_par-xmle1 is not initial and lk_intfms_par-xmlv1 is not initial ) or
         ( lk_intfms_par-xmle2 is not initial and lk_intfms_par-xmlv2 is not initial ).
        create object lo_xms_persist_adm.
      endif.
      select single pid msgstate eo_refid eo_refval exetimest inittimest sendtimest from sxmspmast
        into (lk_intfms_par-intfms-proxy_pid,       lk_intfms_par-intfms-proxy_msgstate,   lk_intfms_par-intfms-proxy_eo_refid, lk_intfms_par-intfms-proxy_eo_refval,
              lk_intfms_par-intfms-proxy_exetimest, lk_intfms_par-intfms-proxy_inittimest, lk_intfms_par-intfms-proxy_sendtimest)
        where msgguid = lk_intfms_par-intfms-proxy_msgguid.
      select single icon_id from sxmsmstat into lk_intfms_par-intfms-intf_istat_ico where msgstate = lk_intfms_par-intfms-proxy_msgstate.
      clear: lv_xml_filter1, lv_xml_filter2.

      if ( lk_intfms_par-xmle1 is not initial and lk_intfms_par-xmlv1 is not initial ) or
         ( lk_intfms_par-xmle2 is not initial and lk_intfms_par-xmlv2 is not initial ).
        clear: li_msg_payload[], li_xml_contents[].
        try.
            lo_xms_persist_adm->get_xi_payload(
              exporting
                im_msgguid         = lk_intfms_par-intfms-proxy_msgguid
                im_pid             = lk_intfms_par-intfms-proxy_pid
                im_version         = '000'
              importing
                ex_messagepayload  = li_msg_payload ).
          catch cx_xms_syserr_persist.
        endtry.

        read table li_msg_payload assigning <lk_msg_payload> index 1.
        if sy-subrc = 0.
          cl_soap_xml_parser=>get_data( exporting xdoc = <lk_msg_payload>-payload  importing data = li_xml_contents ).
          loop at li_xml_contents assigning <lk_xml_contents>.
            clear: lv_xml_tagname, lv_xml_tagnamespace.
            split <lk_xml_contents>-tag_name at ':' into lv_xml_tagnamespace lv_xml_tagname.
            if lv_xml_tagname is initial. lv_xml_tagname = lv_xml_tagnamespace. endif.
            if ( lk_intfms_par-xmle1 is not initial and lk_intfms_par-xmlv1 is not initial ) and ( lv_xml_tagname in lk_intfms_par-xmle1 and <lk_xml_contents>-tag_value in lk_intfms_par-xmlv1 ) .
              lv_xml_filter1 = 'X'.
            endif.
            if ( lk_intfms_par-xmle2 is not initial and lk_intfms_par-xmlv2 is not initial ) and ( lv_xml_tagname in lk_intfms_par-xmle2 and <lk_xml_contents>-tag_value in lk_intfms_par-xmlv2 ) .
              lv_xml_filter2 = 'X'.
            endif.
          endloop.
        endif.

        if ( lk_intfms_par-xmle1 is not initial and lk_intfms_par-xmlv1 is not initial ) and ( lk_intfms_par-xmle2 is not initial and lk_intfms_par-xmlv2 is not initial ).
          if lk_intfms_par-xmlo1 = '1'. "Or
            if lv_xml_filter1 is not initial or  lv_xml_filter2 is not initial. else. clear lk_intfms_par-intfms. endif.
          else.
            if lv_xml_filter1 is not initial and lv_xml_filter2 is not initial. else. clear lk_intfms_par-intfms. endif.
          endif.
        else.
          if ( lk_intfms_par-xmle1 is not initial and lk_intfms_par-xmlv1 is not initial ) and lv_xml_filter1 is initial.
            clear lk_intfms_par-intfms.
          endif.
          if ( lk_intfms_par-xmle2 is not initial and lk_intfms_par-xmlv2 is not initial ) and lv_xml_filter2 is initial.
            clear lk_intfms_par-intfms.
          endif.
        endif.
      endif.

    when '1'.
      "IDoc
      select single status   into lv_idoc_status          from edidc    where docnum = lk_intfms_par-intfms-idoc_docnum.
      select single statva   into lv_idoc_status_statva   from teds3    where status = lv_idoc_status.
      select single stalight into lv_idoc_status_stalight from stalight where statva = lv_idoc_status_statva.
      case lv_idoc_status_stalight.
        when '1'.
          lk_intfms_par-intfms-intf_istat_ico = '@5D@'.
        when '2'.
          lk_intfms_par-intfms-intf_istat_ico = '@5B@'.
        when '3'.
          lk_intfms_par-intfms-intf_istat_ico = '@5C@'.
      endcase.
  endcase.

  "Logs
  if lk_intfms_par-intfms-lognumber is not initial.
    select single * into lk_log_header from balhdr where lognumber = lk_intfms_par-intfms-lognumber.
    call function 'BAL_LOG_EXIST'
      exporting
        i_log_handle  = lk_log_header-log_handle
      exceptions
        log_not_found = 1.
    if sy-subrc ne 0.
      append lk_log_header to li_log_header.
      call function 'BAL_DB_LOAD'
        exporting
          i_t_log_header = li_log_header
        exceptions
          others         = 0.
    endif.

    lk_log_msghndl-log_handle = lk_log_header-log_handle.
    lk_log_msghndl-msgnumber  = lk_log_header-last_msgnr.

    if lk_log_header-last_msgnr > 1.
      lk_intfms_par-intfms-intf_alog_ico = '@DH@'.
    endif.

    call function 'BAL_LOG_MSG_READ'
      exporting
        i_s_msg_handle = lk_log_msghndl
      importing
        e_s_msg        = lk_log_msg
      exceptions
        log_not_found  = 1
        msg_not_found  = 2
        others         = 3.
    case lk_log_msg-msgty.
      when 'A' or 'E'. lk_intfms_par-intfms-intf_astat_ico = '@5C@'.
      when 'I' or 'S'. lk_intfms_par-intfms-intf_astat_ico = '@5B@'.
      when 'W'       . lk_intfms_par-intfms-intf_astat_ico = '@5D@'.
    endcase.
    message id lk_log_msg-msgid type lk_log_msg-msgty number lk_log_msg-msgno with lk_log_msg-msgv1 lk_log_msg-msgv2 lk_log_msg-msgv3 lk_log_msg-msgv4 into lk_intfms_par-intfms-log_msgtxt.
  endif.

  call function 'SPTA_INDX_PACKAGE_ENCODE'
    exporting
      data    = lk_intfms_par-intfms
    importing
      indxtab = li_rfcdata.

endform.                    "data_retrieve_para_process
*&---------------------------------------------------------------------*
*&      Form  data_retrieve_para_sync
*&---------------------------------------------------------------------*
form data_retrieve_para_sync using li_rfcdata         type spta_t_indxtab
                                  lv_rfcsubrc        type sy-subrc
                                  lv_rfcmsg          type spta_t_rfcmsg
                                  li_object_pending  type spta_t_pending_objects
                                  lk_after_rfc_imp   type spta_t_after_rfc_imp
                         changing lk_after_rfc_exp   type spta_t_after_rfc_exp
                                  lv_user_param.
  data: lk_intfms type gtk_intfms.

  call function 'SPTA_INDX_PACKAGE_DECODE'
    exporting
      indxtab = li_rfcdata
    importing
      data    = lk_intfms.

  if lk_intfms is not initial.
    append lk_intfms to gk_intfms_par-export.
  endif.

endform.                    "data_retrieve_par_sync
*&---------------------------------------------------------------------*
*&      Form  data_retrieve_serial
*&---------------------------------------------------------------------*
form data_retrieve_serial.
  types: begin of ltk_proxy_info,
           msgguid                type sxmspmast-msgguid,
           pid                    type sxmspmast-pid,
           msgstate               type sxmspmast-msgstate,
           eo_refid               type sxmspmast-eo_refid,
           eo_refval              type sxmspmast-eo_refval,
           exetimest              type sxmspmast-exetimest,
           inittimest             type sxmspmast-inittimest,
           sendtimest             type sxmspmast-sendtimest,
          end of ltk_proxy_info.
  types: begin of ltk_idoc_info,
          docnum                 type edidc-docnum,
          status                 type edidc-status,
          end of ltk_idoc_info.

  data: lk_log_msg               type bal_s_msg.
  data: lk_log_msghndl           type balmsghndl.
  data  li_log_header            type balhdr_t.
  data: li_log_handle            type bal_t_logh,
        li_log_loaded            type bal_t_logh.
  data  li_intfms                type standard table of gtk_intfms.
  data  li_proxy_info            type standard table of ltk_proxy_info.
  data  li_edidc                 type standard table of ltk_idoc_info.
  data  li_intfmm                type standard table of /pstech/bcintfmm.
  data: lv_idoc_status_statva    type edi_statva.
  data: lv_idoc_status_stalight  type edi_slight.
  data: li_msg_payload            type sxms_messagepayload_tab.
  data: lo_xms_persist_adm       type ref to cl_xms_persist_adm.
  data: li_xml_contents           type srt_xml_data_tab.
  data: lv_xml_tagname            type string,
        lv_xml_tagnamespace       type string.
  data: lv_xml_filter1 type c,
        lv_xml_filter2 type c.
  field-symbols: <lk_intfms>     type gtk_intfms.
  field-symbols: <lk_intfmm>     type /pstech/bcintfmm.
  field-symbols: <lk_proxy_info> type ltk_proxy_info.
  field-symbols: <lk_idoc_info>  type ltk_idoc_info.
  field-symbols: <lk_log_header> type balhdr.
  field-symbols: <lk_xml_contents> type srt_xml_data.
  field-symbols: <lk_msg_payload>  type sxms_messagepayload.

  "Proxy
  li_intfms[] = gi_intfms[].
  delete li_intfms where interface_type ne '0'.
  delete gi_intfms where interface_type eq '0'.
  if li_intfms[] is not initial.
    if ( s_xmle1 is not initial and s_xmlv1 is not initial ) or
       ( s_xmle2 is not initial and s_xmlv2 is not initial ).
      create object lo_xms_persist_adm.
    endif.
    select msgguid pid msgstate eo_refid eo_refval exetimest inittimest sendtimest from sxmspmast into table li_proxy_info for all entries in li_intfms
      where msgguid = li_intfms-proxy_msgguid.
    loop at li_intfms assigning <lk_intfms>.
      clear: lv_xml_filter1, lv_xml_filter2.
      read table li_proxy_info assigning <lk_proxy_info> with key msgguid = <lk_intfms>-proxy_msgguid.
      if sy-subrc = 0.
        select single icon_id from sxmsmstat into <lk_intfms>-intf_istat_ico where msgstate = <lk_proxy_info>-msgstate.
        <lk_intfms>-proxy_pid        = <lk_proxy_info>-pid.
        <lk_intfms>-proxy_msgstate   = <lk_proxy_info>-msgstate.
        <lk_intfms>-proxy_eo_refid   = <lk_proxy_info>-eo_refid.
        <lk_intfms>-proxy_eo_refval  = <lk_proxy_info>-eo_refval.
        <lk_intfms>-proxy_exetimest  = <lk_proxy_info>-exetimest.
        <lk_intfms>-proxy_inittimest = <lk_proxy_info>-inittimest.
        <lk_intfms>-proxy_sendtimest = <lk_proxy_info>-sendtimest.

        "XML filters
        if ( s_xmle1 is not initial and s_xmlv1 is not initial ) or
           ( s_xmle2 is not initial and s_xmlv2 is not initial ).
          clear: li_msg_payload[], li_xml_contents[].
          try.
              lo_xms_persist_adm->get_xi_payload(
                exporting
                  im_msgguid         = <lk_intfms>-proxy_msgguid
                  im_pid             = <lk_intfms>-proxy_pid
                  im_version         = '000'
                importing
                  ex_messagepayload  = li_msg_payload ).
            catch cx_xms_syserr_persist.
          endtry.

          read table li_msg_payload assigning <lk_msg_payload> index 1.
          if sy-subrc = 0.

            cl_soap_xml_parser=>get_data( exporting xdoc = <lk_msg_payload>-payload  importing data = li_xml_contents ).
            loop at li_xml_contents assigning <lk_xml_contents>.
              clear: lv_xml_tagname, lv_xml_tagnamespace.
              split <lk_xml_contents>-tag_name at ':' into lv_xml_tagnamespace lv_xml_tagname.
              if lv_xml_tagname is initial. lv_xml_tagname = lv_xml_tagnamespace. endif.

              if ( s_xmle1 is not initial and s_xmlv1 is not initial ) and ( lv_xml_tagname in s_xmle1 and <lk_xml_contents>-tag_value in s_xmlv1 ) .
                lv_xml_filter1 = 'X'.
              endif.
              if ( s_xmle2 is not initial and s_xmlv2 is not initial ) and ( lv_xml_tagname in s_xmle2 and <lk_xml_contents>-tag_value in s_xmlv2 ) .
                lv_xml_filter2 = 'X'.
              endif..
            endloop.
          endif.

          if     ( s_xmle1 is not initial and s_xmlv1 is not initial ) and ( s_xmle2 is not initial and s_xmlv2 is not initial ) .
            if     s_xmlo1 = '1'. "Or
              if lv_xml_filter1 is not initial or lv_xml_filter2 is not initial.   append <lk_intfms> to gi_intfms.  endif.
            else.
              if lv_xml_filter1 is not initial and lv_xml_filter2 is not initial.  append <lk_intfms> to gi_intfms.  endif.
            endif.
          else.
            if ( s_xmle1 is not initial and s_xmlv1 is not initial ) and lv_xml_filter1 is not initial.
              append <lk_intfms> to gi_intfms.
            endif.
            if ( s_xmle2 is not initial and s_xmlv2 is not initial ) and lv_xml_filter2 is not initial.
              append <lk_intfms> to gi_intfms.
            endif.
          endif.
        else.
          append <lk_intfms> to gi_intfms.
        endif.
      endif.
    endloop.
  endif.

  "IDoc
  li_intfms[] = gi_intfms[].
  delete li_intfms where interface_type ne '1'.
  if li_intfms[] is not initial.
    select docnum status from edidc into table li_edidc for all entries in li_intfms where docnum = li_intfms-idoc_docnum.
    loop at gi_intfms assigning <lk_intfms> where interface_type eq '1'.
      read table li_edidc assigning <lk_idoc_info> with key docnum = <lk_intfms>-idoc_docnum.
      if sy-subrc = 0.
        select single statva   into lv_idoc_status_statva   from teds3    where status = <lk_idoc_info>-status.
        select single stalight into lv_idoc_status_stalight from stalight where statva = lv_idoc_status_statva.
        case lv_idoc_status_stalight.
          when '1'.
            <lk_intfms>-intf_istat_ico = '@5D@'.
          when '2'.
            <lk_intfms>-intf_istat_ico = '@5B@'.
          when '3'.
            <lk_intfms>-intf_istat_ico = '@5C@'.
        endcase.
      endif.
    endloop.
  endif.

  "Logs
  li_intfms[] = gi_intfms[].
  delete li_intfms where lognumber is initial.
  if li_intfms[] is not initial.
    select * into table li_log_header from balhdr for all entries in li_intfms where lognumber = li_intfms-lognumber.
    loop at li_log_header assigning <lk_log_header>.
      call function 'BAL_LOG_EXIST'
        exporting
          i_log_handle  = <lk_log_header>-log_handle
        exceptions
          log_not_found = 1.
      if sy-subrc = 0.
        delete li_log_header.
      endif.
    endloop.

    "load logs from database
    call function 'BAL_DB_LOAD'
      exporting
        i_t_log_header = li_log_header
      exceptions
        others         = 0.
  endif.

  loop at gi_intfms assigning <lk_intfms> where lognumber is not initial.
    read table li_log_header assigning <lk_log_header> with table key mandant = sy-mandt lognumber = <lk_intfms>-lognumber.
    if sy-subrc = 0.
      lk_log_msghndl-log_handle = <lk_log_header>-log_handle.
      lk_log_msghndl-msgnumber  = <lk_log_header>-last_msgnr.

      if <lk_log_header>-last_msgnr > 1.
        <lk_intfms>-intf_alog_ico = '@DH@'.
      endif.

      call function 'BAL_LOG_MSG_READ'
        exporting
          i_s_msg_handle = lk_log_msghndl
        importing
          e_s_msg        = lk_log_msg
        exceptions
          log_not_found  = 1
          msg_not_found  = 2
          others         = 3.
      case lk_log_msg-msgty.
        when 'A' or 'E'. <lk_intfms>-intf_astat_ico = '@5C@'.
        when 'I' or 'S'. <lk_intfms>-intf_astat_ico = '@5B@'.
        when 'W'       . <lk_intfms>-intf_astat_ico = '@5D@'.
      endcase.
      message id lk_log_msg-msgid type lk_log_msg-msgty number lk_log_msg-msgno with lk_log_msg-msgv1 lk_log_msg-msgv2 lk_log_msg-msgv3 lk_log_msg-msgv4 into <lk_intfms>-log_msgtxt.
    endif.
  endloop.

endform.                    "data_retrieve_serial
*&---------------------------------------------------------------------*
*&      Form  data_display_main_alv_config
*&---------------------------------------------------------------------*
form data_display_main_alv_config.
  data lo_alv_functions      type ref to cl_salv_functions.
  data lo_alv_columns        type ref to cl_salv_columns_table.
  data lo_alv_column         type ref to cl_salv_column_table.
  data lo_alv_layout         type ref to cl_salv_layout.
  data lo_alv_events         type ref to cl_salv_events_table.
  data lo_alv_selections     type ref to cl_salv_selections.
  data lo_alv_events_handle  type ref to lcl_alv_events_handle.
  data lo_alv_functions_list type ref to cl_salv_functions_list.
  data lk_alv_layout_key     type salv_s_layout_key.
  data lo_elem_type          type ref to cl_abap_elemdescr.
  data lk_elem_ddic          type dfies.
  data lo_msg                type ref to cx_root.
  data lv_msg                type string.
  data: lv_alv_longtext type scrtext_l.
  data: lv_alv_medtext  type scrtext_m.
  field-symbols: <lk_intfms> type gtk_intfms.

  try.
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


      read table gi_intfms assigning <lk_intfms> index 1.
      if sy-subrc = 0.
        lo_alv_column ?= lo_alv_columns->get_column( 'INTERFACE_KEY1' ).
        lo_alv_column->set_long_text( text-k01 ).
        lo_alv_column->set_medium_text( text-k01 ).
        lo_alv_column->set_short_text( text-k01 ).
        if <lk_intfms>-key1_datatype is not initial.
          lo_elem_type ?= cl_abap_elemdescr=>describe_by_name( <lk_intfms>-key1_datatype ).
          lk_elem_ddic = lo_elem_type->get_ddic_field( ).
          lo_alv_column->set_long_text( lk_elem_ddic-scrtext_l ).
          lo_alv_column->set_medium_text( lk_elem_ddic-scrtext_m ).
          lo_alv_column->set_short_text( lk_elem_ddic-scrtext_s ).
        endif.

        lo_alv_column ?= lo_alv_columns->get_column( 'INTERFACE_KEY2' ).
        lo_alv_column->set_long_text( text-k02 ).
        lo_alv_column->set_medium_text( text-k02 ).
        lo_alv_column->set_short_text( text-k02 ).
        if <lk_intfms>-key2_datatype is not initial.
          lo_elem_type ?= cl_abap_elemdescr=>describe_by_name( <lk_intfms>-key2_datatype ).
          lk_elem_ddic = lo_elem_type->get_ddic_field( ).
          lo_alv_column->set_long_text( lk_elem_ddic-scrtext_l ).
          lo_alv_column->set_medium_text( lk_elem_ddic-scrtext_m ).
          lo_alv_column->set_short_text( lk_elem_ddic-scrtext_s ).
        endif.

        lo_alv_column ?= lo_alv_columns->get_column( 'INTERFACE_KEY3' ).
        lo_alv_column->set_long_text( text-k03 ).
        lo_alv_column->set_medium_text( text-k03 ).
        lo_alv_column->set_short_text( text-k03 ).
        if <lk_intfms>-key3_datatype is not initial.
          lo_elem_type ?= cl_abap_elemdescr=>describe_by_name( <lk_intfms>-key3_datatype ).
          lk_elem_ddic = lo_elem_type->get_ddic_field( ).
          lo_alv_column->set_long_text( lk_elem_ddic-scrtext_l ).
          lo_alv_column->set_medium_text( lk_elem_ddic-scrtext_m ).
          lo_alv_column->set_short_text( lk_elem_ddic-scrtext_s ).
        endif.

        lo_alv_column ?= lo_alv_columns->get_column( 'INTERFACE_KEY4' ).
        lo_alv_column->set_long_text( text-k04 ).
        lo_alv_column->set_medium_text( text-k04 ).
        lo_alv_column->set_short_text( text-k04 ).
        if <lk_intfms>-key4_datatype is not initial.
          lo_elem_type ?= cl_abap_elemdescr=>describe_by_name( <lk_intfms>-key4_datatype ).
          lk_elem_ddic = lo_elem_type->get_ddic_field( ).
          lo_alv_column->set_long_text( lk_elem_ddic-scrtext_l ).
          lo_alv_column->set_medium_text( lk_elem_ddic-scrtext_m ).
          lo_alv_column->set_short_text( lk_elem_ddic-scrtext_s ).
        endif.
      endif.

      "Layouts
      lo_alv_layout = go_main_alv_table->get_layout( ).
      lk_alv_layout_key-report = sy-repid.
      lo_alv_layout->set_key( lk_alv_layout_key ).
      lo_alv_layout->set_save_restriction( lo_alv_layout->restrict_none ).
      lo_alv_layout->set_default( abap_true ).
      if s_alvvar is not initial.
        lo_alv_layout->set_initial_layout( s_alvvar ).
      endif.

      "Events
      lo_alv_events = go_main_alv_table->get_event( ).
      create object lo_alv_events_handle.
      set handler lo_alv_events_handle->command     for lo_alv_events.
      set handler lo_alv_events_handle->doubleclick for lo_alv_events.
      set handler lo_alv_events_handle->linkclick   for lo_alv_events.

      go_main_alv_table->display( ).
    catch cx_salv_not_found cx_salv_wrong_call cx_salv_existing.
  endtry.

endform.                    "data_display_alv_settings
*&---------------------------------------------------------------------*
*&      Form  data_display_proxy_tree
*&---------------------------------------------------------------------*
form data_display_proxy_tree using lk_intfms type gtk_intfms.
  data: li_proxy_toolbar_event    type cntl_simple_events,
        lk_proxy_toolbar_event    type cntl_simple_event.
  data: lo_proxy_toolbar_handle   type ref to lcl_tool_events_handle.
  data: li_msg_payload            type sxms_messagepayload_tab.
  data: lo_xms_persist_adm        type ref to cl_xms_persist_adm.
  data: li_proxy_tree_button      type ttb_button.
  data: lo_proxy_tree_handle type ref to lcl_tree_events_handle.
  field-symbols: <lk_msg_payload> type sxms_messagepayload.

  clear: gi_tree_search_result, gv_tree_search_index.


  "Clear off other objects
  if go_log_table is not initial or go_proxy_xml is not initial or go_idoc_tree is not initial.
    go_main_split_container->remove_control( row = 1 column = 2 ).
    free: go_main_view_container, go_log_table, go_proxy_xml, go_idoc_tree, go_proxy_tree.
  endif.

  "Create view container
  if go_main_view_container is initial.
    go_main_view_container = go_main_split_container->get_container( row = 1 column = 2 ).
    clear: go_view_split_container, go_view_tool_container, go_view_obj_container, go_view_toolbar.
  endif.

  "Create split toobar & objects, then set Toolbars
  if go_view_split_container is initial.
    create object go_view_split_container
      exporting
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

    create object go_view_toolbar
      exporting
        parent = go_view_tool_container.
    li_proxy_tree_button[] = gi_proxy_tree_button[].
    go_view_toolbar->add_button_group( li_proxy_tree_button ).
    lk_proxy_toolbar_event-eventid = cl_gui_toolbar=>m_id_function_selected.  lk_proxy_toolbar_event-appl_event = ' '.
    append lk_proxy_toolbar_event to li_proxy_toolbar_event.
    go_view_toolbar->set_registered_events( li_proxy_toolbar_event ).
    create object lo_proxy_toolbar_handle.
    set handler lo_proxy_toolbar_handle->command     for go_view_toolbar.
  endif.

  "Reinitialize when selected again.
  if go_proxy_tree is not initial.
    go_proxy_tree->free( ).
    clear go_proxy_tree.
  endif.

  "Create Tree
  if go_proxy_tree is initial.
    create object go_proxy_tree
      exporting
        id_parent    = go_view_obj_container
        id_tree_text = 'XML'.
    perform data_display_proxy_tree_config.
    go_proxy_tree->register_node_ctxtmn_requested( ).
    go_proxy_tree->register_node_ctxtmn_selected( ).
    go_proxy_tree->register_node_1st_open( ).

    create object lo_proxy_tree_handle.
    authority-check object 'ZBC_INTF'
             id '/PSTECH/II' field lk_intfms-interface_id
             id 'ACTVT' field '02'.
    if sy-subrc = 0.
      set handler lo_proxy_tree_handle->node_menu_request     for go_proxy_tree.
      set handler lo_proxy_tree_handle->node_menu_select      for go_proxy_tree.
    endif.
    set handler lo_proxy_tree_handle->node_open             for go_proxy_tree.
  endif.

  if gv_xml_document is initial.
    "Retrieve XML
    create object lo_xms_persist_adm.
    try.
        lo_xms_persist_adm->get_xi_payload(
          exporting
            im_msgguid         = lk_intfms-proxy_msgguid
            im_pid             = lk_intfms-proxy_pid
            im_version         = '000'
          importing
            ex_messagepayload  = li_msg_payload ).
      catch cx_xms_syserr_persist.
    endtry.

    "Retrieve Payload & show XML
    read table li_msg_payload assigning <lk_msg_payload> index 1.
    if sy-subrc = 0.
      gk_intfms = lk_intfms.
      perform data_display_proxy_xml2tree using <lk_msg_payload>-payload changing go_proxy_tree.
    endif.
  else.
    perform data_display_proxy_xml2tree using gv_xml_document changing go_proxy_tree.
  endif.

endform.                    "data_display_proxy_tree
*&---------------------------------------------------------------------*
*&      Form  data_display_proxy_tree2xml
*&---------------------------------------------------------------------*
form data_display_proxy_tree2xml using lo_tree type ref to /pstech/cl_bc_gui_edit_tree
                              changing lv_xml  type xstring.
  types: begin of ltk_xml_node,
          tree_node type uac_node_id,
          xml_node  type ref to if_ixml_mini_node,
          value     type string,
         end of ltk_xml_node.
  data: li_tree_nodes      type uac_t_node_long,
        li_tree_nodes_sort type uac_t_node_long.

  data: lk_tree_cell  type uac_s_cell_long.
  data: li_xml_nodes  type standard table of ltk_xml_node.
  data: lk_xml_node   type ltk_xml_node.
  data: lv_xml_node_name  type string,
        lv_xml_node_value type string.
  field-symbols: <lk_tree_node>       type uac_s_node_long.
  field-symbols: <lk_tree_node_sort>  type uac_s_node_long.
  field-symbols: <lk_xml_node>        type ltk_xml_node.
  field-symbols: <lv_xml_node_value>  type any.

  data lv_string type string.
  data: lo_ixml_dom         type ref to if_ixml_mini_dom.
  data: lo_ixml_render      type ref to if_ixml_mini_renderer.
  data: lo_ixml_node        type ref to if_ixml_mini_node.

  lo_ixml_dom = cl_ixml_mini=>create_dom( ).
  lo_ixml_render = cl_ixml_mini=>create_renderer( ).

  li_tree_nodes = lo_tree->get_nodes( ).
  perform  data_display_tree_sort using '0' li_tree_nodes changing li_tree_nodes_sort.
  loop at li_tree_nodes_sort assigning <lk_tree_node>.
    clear lk_xml_node.
    read table li_xml_nodes assigning <lk_xml_node> with key tree_node = <lk_tree_node>-parent_node.
    if sy-subrc = 0.
      lo_tree->get_cell( exporting id_node_id = <lk_tree_node>-node_id id_column_id = '1' importing es_cell = lk_tree_cell ).
      assign lk_tree_cell-dref_value->* to <lv_xml_node_value>.
      lv_xml_node_name = <lk_tree_node>-value.
      lv_xml_node_value = <lv_xml_node_value>.

      lk_xml_node-tree_node = <lk_tree_node>-node_id.
      case <lk_tree_node>-style_id.
        when 'XML_STRUC'.
          lo_ixml_dom->add_element( exporting name = lv_xml_node_name parent = <lk_xml_node>-xml_node  importing new_node = lk_xml_node-xml_node ).
        when 'XML_ELEM'.
          lo_ixml_dom->add_element( exporting name = lv_xml_node_name parent = <lk_xml_node>-xml_node  importing new_node = lk_xml_node-xml_node ).
          lk_xml_node-value = lv_xml_node_value.
        when 'XML_ATTR'.
          lo_ixml_dom->add_attribute( exporting name = lv_xml_node_name value = lv_xml_node_value parent = <lk_xml_node>-xml_node ).
      endcase.

      append lk_xml_node to li_xml_nodes.
    else.
      lv_xml_node_name = <lk_tree_node>-value.
      lk_xml_node-tree_node = <lk_tree_node>-node_id.
      lo_ixml_dom->add_element( exporting name = lv_xml_node_name  importing new_node = lk_xml_node-xml_node ).
      append lk_xml_node to li_xml_nodes.
    endif.
  endloop.

  loop at li_xml_nodes assigning <lk_xml_node> where value is not initial.
    lo_ixml_dom->add_text( exporting value = <lk_xml_node>-value parent = <lk_xml_node>-xml_node ).
  endloop.
  clear lv_xml.

  lo_ixml_render->render_xstring( exporting dom = lo_ixml_dom importing stream = lv_xml ).

endform.                    "data_display_proxy_tree2xml
*&---------------------------------------------------------------------*
*&      Form  data_display_tree_sort
*&---------------------------------------------------------------------*
form  data_display_tree_sort using lv_nodeid          type uac_node_id
                                   li_tree_nodes      type uac_t_node_long
                          changing li_tree_nodes_sort type uac_t_node_long.

  field-symbols: <lk_tree_node_curr> type uac_s_node_long,
                 <lk_tree_node_next> type uac_s_node_long.
  read table li_tree_nodes assigning <lk_tree_node_curr> with key node_id = lv_nodeid.
  if sy-subrc = 0.
    if <lk_tree_node_curr>-parent_node is initial.
      append <lk_tree_node_curr> to li_tree_nodes_sort.
    endif.
    if <lk_tree_node_curr>-first_child is not initial.
      read table li_tree_nodes assigning <lk_tree_node_next> with key node_id = <lk_tree_node_curr>-first_child.
      if sy-subrc = 0.
        append <lk_tree_node_next> to li_tree_nodes_sort.
        perform data_display_tree_sort using <lk_tree_node_next>-node_id li_tree_nodes changing li_tree_nodes_sort.
      endif.
    endif.
    if <lk_tree_node_curr>-next_sibling is not initial.
      read table li_tree_nodes assigning <lk_tree_node_next> with key node_id = <lk_tree_node_curr>-next_sibling.
      if sy-subrc = 0.
        append <lk_tree_node_next> to li_tree_nodes_sort.
        perform data_display_tree_sort using <lk_tree_node_next>-node_id li_tree_nodes changing li_tree_nodes_sort.
      endif.
    endif.
  endif.
endform.                    "data_display_tree_sort
*&---------------------------------------------------------------------*
*&      Form  data_display_proxy_xml2tree
*&---------------------------------------------------------------------*
form data_display_proxy_xml2tree using lv_xml type xstring
                              changing lo_tree type ref to /pstech/cl_bc_gui_edit_tree.
  types: begin of ltk_tree_nodes,
           level  type i,
           nodeid type uac_node_id,
         end of ltk_tree_nodes.
  data: li_proxy_xml                 type srt_xml_data_tab.
  data: lk_proxy_xml                 type srt_xml_data.
  data: lv_tree_node_lastid          type uac_node_id.
  data: lv_tree_node_lastparent      type uac_s_node_long.
  data: lv_tree_node_style           type uac_style_id.
  data: lv_tree_node_relat           type uac_relationship.
  data: lv_tree_node_leaf            type uac_flag.
  data: li_tree_nodes                type sorted table of ltk_tree_nodes with unique key level.
  data: lk_tree_nodes                type ltk_tree_nodes.
  data: lv_tree_editable             type uac_flag.
  data: lv_proxy_next_tabix          type sytabix.
  field-symbols: <lk_proxy_xml>      type srt_xml_data.
  field-symbols: <lk_proxy_xml_next> type srt_xml_data.

  authority-check object 'ZBC_INTF'
         id '/PSTECH/II' field gk_intfms-interface_id
         id 'ACTVT' field '02'.
  if sy-subrc = 0.
    lv_tree_editable = '1'.
  else.
    lv_tree_editable = '0'.
  endif.

  cl_soap_xml_parser=>get_data( exporting xdoc = lv_xml importing data = li_proxy_xml ).
  delete li_proxy_xml where tag_name is initial.

  clear lv_tree_node_lastid.
  loop at li_proxy_xml assigning <lk_proxy_xml>.
    lv_proxy_next_tabix = sy-tabix + 1.
    unassign <lk_proxy_xml_next>.
    read table li_proxy_xml assigning <lk_proxy_xml_next> index lv_proxy_next_tabix.

    lv_tree_node_leaf = 0.

    case <lk_proxy_xml>-tag_type.
      when 'NODE_ONLY'.
        lv_tree_node_style = 'XML_ELEM'.
      when 'DATA'.
        lv_tree_node_style = 'XML_ELEM'.
        lv_tree_node_leaf = 1.
      when 'ATTRIBUTE'.
        lv_tree_node_style = 'XML_ATTR'.
        <lk_proxy_xml>-tag_level = <lk_proxy_xml>-tag_level + 1.
        lv_tree_node_leaf = 1.
      when others.
        continue.
    endcase.

    if <lk_proxy_xml_next> is assigned.
      if <lk_proxy_xml_next>-tag_type = 'ATTRIBUTE' and <lk_proxy_xml>-tag_type ne 'ATTRIBUTE' .
        lv_tree_node_leaf = 0.
      endif.
    endif.

    if lk_proxy_xml-tag_level lt <lk_proxy_xml>-tag_level.
      if <lk_proxy_xml>-tag_type ne 'ATTRIBUTE'.
        lo_tree->get_node( exporting id_node_id = lv_tree_node_lastid importing es_node = lv_tree_node_lastparent ).
        lo_tree->change_node( id_node_id = lv_tree_node_lastparent-node_id id_style_id = 'XML_STRUC' ).
      endif.
      lv_tree_node_relat = '6'. "last child
    elseif lk_proxy_xml-tag_level gt <lk_proxy_xml>-tag_level.
      read table li_tree_nodes into lk_tree_nodes with key level = <lk_proxy_xml>-tag_level.
      lv_tree_node_lastid = lk_tree_nodes-nodeid.
      lv_tree_node_relat = '2'. "siblings
    else.
      if <lk_proxy_xml>-tag_type ne 'ATTRIBUTE' and lk_proxy_xml-tag_type eq 'ATTRIBUTE'.
        lv_tree_node_lastparent = go_proxy_tree->get_parent( id_node_id = lv_tree_node_lastid ).
        lo_tree->change_node( id_node_id = lv_tree_node_lastparent-node_id id_style_id = 'XML_STRUC' ).
      endif.
      lv_tree_node_relat = '2'. "siblings
    endif.

    lv_tree_node_lastid = lo_tree->add_node( id_relat_node = lv_tree_node_lastid id_is_leaf = lv_tree_node_leaf  id_relationship = lv_tree_node_relat id_value = <lk_proxy_xml>-tag_name id_style_id = lv_tree_node_style ).
    lo_tree->add_cell( id_node_id = lv_tree_node_lastid id_column_id = '1' id_editable = lv_tree_editable id_value = <lk_proxy_xml>-tag_value ).

    lk_tree_nodes-level = <lk_proxy_xml>-tag_level.
    lk_tree_nodes-nodeid = lv_tree_node_lastid.

    if <lk_proxy_xml>-tag_type ne 'ATTRIBUTE'.
      read table li_tree_nodes with key level = <lk_proxy_xml>-tag_level transporting no fields.
      if sy-subrc = 0.
        modify table li_tree_nodes from lk_tree_nodes.
      else.
        append lk_tree_nodes to li_tree_nodes.
      endif.
    endif.

    lk_proxy_xml = <lk_proxy_xml>.
  endloop.
  lo_tree->expand_node( id_node_id = '1' ).
  perform data_display_tree_update changing lo_tree.
endform.                    "data_display_proxy_xml2tree
*&---------------------------------------------------------------------*
*&      Form  data_display_tree_update
*&---------------------------------------------------------------------*
form data_display_tree_update changing lo_tree type ref to /pstech/cl_bc_gui_edit_tree.
  data: li_tree_nodes type uac_t_node_long.
  field-symbols <lk_tree_nodes> type uac_s_node_long.

  if go_proxy_tree is not initial.
    li_tree_nodes = lo_tree->get_nodes( ).
    loop at li_tree_nodes assigning <lk_tree_nodes>.
      case <lk_tree_nodes>-style_id.
        when 'XML_STRUC' or 'XML_STRUC2' .
          lo_tree->change_cell( id_node_id = <lk_tree_nodes>-node_id id_column_id = '1' id_editable = '0' id_u_editable = '1' ).
      endcase.
    endloop.
  endif.

  lo_tree->column_optimize( id_column_id = '&Hierarchy' ).
  lo_tree->column_optimize( id_column_id = '1' ).
*  lo_tree->update_frontend( ).
endform.                    "data_display_tree_update
*&---------------------------------------------------------------------*
*&      Form  data_display_tree_search
*&---------------------------------------------------------------------*
form data_display_tree_search changing lo_tree type ref to /pstech/cl_bc_gui_edit_tree.
  data: lk_popup_struc      type sval.
  data: li_popup_struc      type standard table of sval.
  data: lv_search_pattern   type uac_value.
  data: li_tree_node        type uac_t_node_long,
        li_tree_node_sort   type uac_t_node_long,
        li_tree_result_node type uac_t_node_long.
  data: li_tree_result_cell type uac_t_cell_long.
  field-symbols : <lk_tree_result>      type uac_s_node_long.

  lk_popup_struc-tabname = 'RSDXX'.
  lk_popup_struc-fieldname = 'FINDSTR'.
  lk_popup_struc-novaluehlp = 'X'.
  append lk_popup_struc to li_popup_struc.

  clear: gi_tree_search_result, gv_tree_search_index.

  call function 'POPUP_GET_VALUES'
    exporting
      popup_title     = 'Find'
      no_value_check  = 'X'
    tables
      fields          = li_popup_struc
    exceptions
      error_in_fields = 1
      others          = 2.

  read table li_popup_struc into lk_popup_struc index 1.
  if lk_popup_struc-value is not initial.
    concatenate '*' lk_popup_struc-value '*' into lv_search_pattern.
    lo_tree->find_item_by_pattern( exporting id_pattern = lv_search_pattern importing et_nodes = li_tree_result_node et_cells = li_tree_result_cell ).
  endif.


  if li_tree_result_node[] is not initial or li_tree_result_cell[] is not initial.
    li_tree_node = lo_tree->get_nodes( ).
    read table li_tree_node assigning <lk_tree_result> with key parent_node = ''.
    if sy-subrc = 0.
      perform data_display_tree_sort using <lk_tree_result>-node_id li_tree_node changing li_tree_node_sort.
      loop at li_tree_node_sort assigning <lk_tree_result>.
        read table li_tree_result_node transporting no fields with key node_id = <lk_tree_result>-node_id.
        if sy-subrc = 0.
          append  <lk_tree_result> to gi_tree_search_result.
        else.
          read table li_tree_result_cell transporting no fields with key node_id = <lk_tree_result>-node_id.
          if sy-subrc = 0.
            append  <lk_tree_result> to gi_tree_search_result .
          endif.
        endif.
      endloop.
    endif.
  endif.

  perform data_display_tree_search_next changing lo_tree.

endform.                    "data_display_tree_search
*&---------------------------------------------------------------------*
*&      Form  data_display_tree_search_next
*&---------------------------------------------------------------------*
form data_display_tree_search_next changing lo_tree type ref to /pstech/cl_bc_gui_edit_tree.
  field-symbols : <lk_tree_result>      type uac_s_node_long.
  if gi_tree_search_result[] is not initial.
    gv_tree_search_index = gv_tree_search_index + 1.
    read table gi_tree_search_result assigning <lk_tree_result> index gv_tree_search_index.
    if sy-subrc = 0.
      if <lk_tree_result>-expanded ne '1'.
        lo_tree->expand_node( id_node_id = <lk_tree_result>-node_id ).
      endif.
      lo_tree->set_selected_node( id_node_id = <lk_tree_result>-node_id ).
    else.
      clear gv_tree_search_index.
      message 'End of search' type 'I'.
    endif.
  endif.
endform.                    "data_display_tree_search_next
*&---------------------------------------------------------------------*
*&      Form  data_display_tree_hide_empty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LO_TREE    text
*----------------------------------------------------------------------*
form data_display_tree_hide_empty changing lo_tree type ref to /pstech/cl_bc_gui_edit_tree.

  data: li_tree_node        type uac_t_node_long,
        li_tree_node_sort   type uac_t_node_long,
        li_tree_result_node type uac_t_node_long.
  data: li_tree_result_cell type uac_t_cell_long.
  field-symbols : <lk_tree_result>      type uac_s_node_long.

  li_tree_node = lo_tree->get_nodes( ).

  read table li_tree_node assigning <lk_tree_result> with key parent_node = ''.
  if sy-subrc = 0.
    perform data_display_tree_sort using <lk_tree_result>-node_id li_tree_node changing li_tree_node_sort.
    loop at li_tree_node_sort assigning <lk_tree_result>.

    endloop.
  endif.

endform.                    "data_display_tree_hide_empty
*&---------------------------------------------------------------------*
*&      Form  data_display_tree_show_empty
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->LO_TREE    text
*----------------------------------------------------------------------*
form data_display_tree_show_empty changing lo_tree type ref to /pstech/cl_bc_gui_edit_tree.
  field-symbols : <lk_tree_result>      type uac_s_node_long.
  if gi_tree_search_result[] is not initial.
    gv_tree_search_index = gv_tree_search_index + 1.
    read table gi_tree_search_result assigning <lk_tree_result> index gv_tree_search_index.
    if sy-subrc = 0.
      if <lk_tree_result>-expanded ne '1'.
        lo_tree->expand_node( id_node_id = <lk_tree_result>-node_id ).
      endif.
      lo_tree->set_selected_node( id_node_id = <lk_tree_result>-node_id ).
    else.
      clear gv_tree_search_index.
      message 'End of search' type 'I'.
    endif.
  endif.
endform.                    "data_display_tree_show_empty
*&---------------------------------------------------------------------*
*&      Form  data_display_proxy_tree_config
*&---------------------------------------------------------------------*
form data_display_proxy_tree_config.
  data: lk_node_style        type uac_s_node_style.
  data: lk_column_style      type uac_s_column_style.
  data: lk_cell_style        type uac_s_cell_style.

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

endform.                    "data_display_idoc_settings
*&---------------------------------------------------------------------*
*&      Form  data_display_proxy_xml
*&---------------------------------------------------------------------*
form data_display_proxy_xml using lk_intfms type gtk_intfms.
  data: li_proxy_toolbar_event    type cntl_simple_events,
        lk_proxy_toolbar_event    type cntl_simple_event.
  data: lo_proxy_toolbar_handle   type ref to lcl_tool_events_handle.
  data: li_msg_payload     type sxms_messagepayload_tab.
  data: lo_xms_persist_adm type ref to cl_xms_persist_adm.
  field-symbols: <lk_msg_payload> type sxms_messagepayload.

  "Clear off other objects
  if go_log_table is not initial or go_idoc_tree is not initial or go_proxy_tree is not initial.
    go_main_split_container->remove_control( row = 1 column = 2 ).
    free: go_main_view_container, go_log_table, go_proxy_xml, go_idoc_tree, go_proxy_tree.
  endif.

  "Create view container
  if go_main_view_container is initial.
    go_main_view_container = go_main_split_container->get_container( row = 1 column = 2 ).
    clear: go_view_split_container, go_view_tool_container, go_view_obj_container, go_view_toolbar.
  endif.

  "Create split toobar & objects, then set Toolbars
  if go_view_split_container is initial.
    create object go_view_split_container
      exporting
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

    create object go_view_toolbar
      exporting
        parent = go_view_tool_container.
    go_view_toolbar->add_button_group( gi_proxy_xml_button ).
    lk_proxy_toolbar_event-eventid = cl_gui_toolbar=>m_id_function_selected.  lk_proxy_toolbar_event-appl_event = ' '.
    append lk_proxy_toolbar_event to li_proxy_toolbar_event.
    go_view_toolbar->set_registered_events( li_proxy_toolbar_event ).
    create object lo_proxy_toolbar_handle.
    set handler lo_proxy_toolbar_handle->command     for go_view_toolbar.
  endif.

  if go_proxy_xml is initial.
    create object go_proxy_xml
      exporting
        parent = go_view_obj_container.
    go_proxy_xml->set_source_type( 'XML' ).
    go_proxy_xml->set_content_type( 'application/xml' ).

    authority-check object 'ZBC_INTF'
             id '/PSTECH/II' field lk_intfms-interface_id
             id 'ACTVT' field '02'.
    if sy-subrc eq 0.
      go_proxy_xml->set_readonly( abap_false ).
    else.
      go_proxy_xml->set_readonly( abap_true ).
    endif.

    go_proxy_xml->set_change_mode( abap_true ).
  endif.

  "Retrieve XML
  if gv_xml_document is initial.
    create object lo_xms_persist_adm.
    try.
        lo_xms_persist_adm->get_xi_payload(
          exporting
            im_msgguid         = lk_intfms-proxy_msgguid
            im_pid             = lk_intfms-proxy_pid
            im_version         = '000'
          importing
            ex_messagepayload  = li_msg_payload ).
      catch cx_xms_syserr_persist.
    endtry.

    "Retrieve Payload & show XML
    read table li_msg_payload assigning <lk_msg_payload> index 1.
    if sy-subrc = 0.
      gk_intfms = lk_intfms.
      go_proxy_xml->set_xstring( <lk_msg_payload>-payload ).
    endif.
    go_proxy_xml->set_change_mode( abap_false ).
  else.
    go_proxy_xml->set_xstring( gv_xml_document ).
    go_proxy_xml->set_change_mode( abap_false ).
  endif.
endform.                    "data_display_xml
*&---------------------------------------------------------------------*
*&      Form  data_display_log_alv
*&---------------------------------------------------------------------*
form data_display_log_alv using lk_intfms type gtk_intfms.
  data li_log_msghndl            type bal_t_msgh.
  data li_log_handle             type bal_t_logh.
  data li_log_header             type balhdr_t.
  data lk_log_header             type balhdr.
  data lk_log_msg                type bal_s_msg.
  data lk_log_table              type gtk_log_table.
  data lo_alv_columns            type ref to cl_salv_columns_table.
  data lo_alv_column             type ref to cl_salv_column_table.
  field-symbols <lk_log_msghndl> type balmsghndl.

  "Make sure that there's a log number
  check lk_intfms-lognumber is not initial.

  "Cleanup
  if go_idoc_tree is not initial or go_proxy_xml is not initial or go_proxy_tree is not initial.
    go_main_split_container->remove_control( row = 1 column = 2 ).
    free: go_main_view_container, go_log_table, go_proxy_xml, go_idoc_tree, go_proxy_tree.
  endif.

  "Re-create the container
  if go_main_view_container is initial.
    go_main_view_container = go_main_split_container->get_container( row = 1 column = 2 ).
  endif.

  select single * into lk_log_header from balhdr where lognumber = lk_intfms-lognumber.
  select log_handle into table li_log_handle from balhdr where lognumber = lk_intfms-lognumber.

  call function 'BAL_LOG_EXIST'
    exporting
      i_log_handle  = lk_log_header-log_handle
    exceptions
      log_not_found = 1.
  if sy-subrc ne 0.
    append lk_log_header to li_log_header.
    call function 'BAL_DB_LOAD'
      exporting
        i_t_log_header = li_log_header
      exceptions
        others         = 0.
  endif.

  "retrieve msg handle
  call function 'BAL_GLB_SEARCH_MSG'
    exporting
      i_t_log_handle = li_log_handle
    importing
      e_t_msg_handle = li_log_msghndl
    exceptions
      msg_not_found  = 1
      others         = 2.
  check sy-subrc eq 0.

  clear gi_log_table.
  loop at li_log_msghndl assigning  <lk_log_msghndl> .
    call function 'BAL_LOG_MSG_READ'
      exporting
        i_s_msg_handle = <lk_log_msghndl>
      importing
        e_s_msg        = lk_log_msg
      exceptions
        log_not_found  = 1
        msg_not_found  = 2
        others         = 3.
    case lk_log_msg-msgty.
      when 'A' or 'E'. lk_log_table-log_ico = '@5C@'.
      when 'I' or 'S'. lk_log_table-log_ico = '@5B@'.
      when 'W'       . lk_log_table-log_ico = '@5D@'.
    endcase.
    message id lk_log_msg-msgid type lk_log_msg-msgty number lk_log_msg-msgno with lk_log_msg-msgv1 lk_log_msg-msgv2 lk_log_msg-msgv3 lk_log_msg-msgv4 into lk_log_table-log_msg.
    append lk_log_table to gi_log_table.
  endloop.

  if go_log_table is initial and gi_log_table[] is not initial .
    try.
        cl_salv_table=>factory(
          exporting
            r_container  = go_main_view_container
          importing
            r_salv_table = go_log_table
          changing
            t_table      = gi_log_table ).

        "Columns
        lo_alv_columns = go_log_table->get_columns( ).
        lo_alv_columns->set_optimize( abap_true ).

        lo_alv_column ?= lo_alv_columns->get_column( 'LOG_ICO' ).
        lo_alv_column->set_short_text( text-ks9 ).

        lo_alv_column ?= lo_alv_columns->get_column( 'LOG_MSG' ).
        lo_alv_column->set_short_text( text-kt1 ).

        go_log_table->display( ).
      catch cx_salv_msg cx_salv_not_found.
    endtry.
  else.
    go_log_table->refresh( ).
  endif.

endform.                    "data_display_log

*&---------------------------------------------------------------------*
*&      Module  STATUS_1001  OUTPUT
*&---------------------------------------------------------------------*
module status_1001 output.
  perform data_display_screen.
endmodule.                 " STATUS_1001  OUTPUT

*----------------------------------------------------------------------*
*  MODULE user_command_1001 INPUT
*----------------------------------------------------------------------*
module user_command_1001 input.
  perform data_display_commands using sy-ucomm.
  call method cl_gui_cfw=>flush.
endmodule.                 " USER_COMMAND_1001  INPUT
*&---------------------------------------------------------------------*
*&      Form  data_process_proxy_restart
*&---------------------------------------------------------------------*
form data_process_proxy_restart using lk_intfms type gtk_intfms.
  data lo_xms_main          type ref to cl_xms_main.
  data lx_xms_system_error  type ref to cx_xms_system_error. "#EC NEEDED

  lo_xms_main = cl_xms_main=>create_xmb( ).

* reinstantiate the message-object
  try.
      call method lo_xms_main->restart_error_message
        exporting
          im_message_guid = lk_intfms-proxy_msgguid
          im_version      = '000'
          im_pipeline_id  = lk_intfms-proxy_pid.
    catch cx_xms_system_error into lx_xms_system_error.
  endtry.

endform.                    "data_process_proxy_restart
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS_PROXY_COPYPROC
*&---------------------------------------------------------------------*
form data_process_proxy_copyproc using lk_intfms type gtk_intfms.
  data lv_guid_new          type sxmsguid.
  data lk_intfms_new        type /pstech/bcintfms.
  data lo_xms_main          type ref to cl_xms_main.
  data lo_xms_system_error  type ref to cx_xms_system_error. "#EC NEEDED
  data lo_xms_runtime       type ref to cl_xms_run_time_env.
  data lk_xms_runtime_eoref type sxmseoref.
  data lo_xms_message       type ref to if_xms_message.
  data lo_xms_message_xmb   type ref to if_xms_message_xmb.
  data l_timestamp          type timestamp.
  data lo_xms_hoplist       type ref to cl_xms_msghdr30_hoplist.
  data lv_xms_hoplist_count type i.
  data lo_xms_engine        type ref to if_xms_engine.
  data lv_adapter_type      type sxmspstype.

  lo_xms_main = cl_xms_main=>create_xmb( ).

  "Reinstantiate the message-object
  try.
      call method lo_xms_main->read_message_from_persist
        exporting
          im_message_guid = lk_intfms-proxy_msgguid
          im_version      = '000'
          im_pipeline_id  = lk_intfms-proxy_pid
        importing
          ex_message      = lo_xms_message.
    catch cx_xms_system_error into lo_xms_system_error.
  endtry.

  if lo_xms_message is not initial.
    call method cl_xms_main=>get_message_properties
      exporting
        im_message      = lo_xms_message
      importing
        ex_run_time_env = lo_xms_runtime.

    lk_xms_runtime_eoref-id = 'RESTART'.
    concatenate  sy-uname ':' lk_intfms-interface_msgid into lk_xms_runtime_eoref-val.
    lo_xms_runtime->set_user_name( sy-uname ).
    lo_xms_runtime->set_eo_ref_inbound( lk_xms_runtime_eoref ).

    lo_xms_message_xmb ?= lo_xms_message.

    "Create GUID for the message
    lv_guid_new = cl_xms_msg_util=>create_guid( ).

    "set attributes of the message-header
    lo_xms_message_xmb->set_message_id( lv_guid_new ).
    "set send date and time
    get time stamp field l_timestamp.
    lo_xms_message_xmb->set_time_sent( l_timestamp ).

    call method cl_xms_main=>get_message_properties
      exporting
        im_message = lo_xms_message
      importing
        ex_hoplist = lo_xms_hoplist.

    if lo_xms_hoplist is not initial.
      describe table lo_xms_hoplist->hoplist lines lv_xms_hoplist_count.
      delete  lo_xms_hoplist->hoplist from lv_xms_hoplist_count.
    endif.

    case lk_intfms-interface_dir.
      when 'I'.
        lv_adapter_type = 'PLAINHTTP'.
      when 'O'.
        lv_adapter_type = ''.
    endcase.

    try.
        lo_xms_engine = cl_xms_main=>create_engine( ).
        call method lo_xms_engine->enter_engine
          exporting
            im_execute_flag = '1'
            im_adapter_id   = lv_adapter_type
          changing
            ch_message      = lo_xms_message_xmb.

      catch cx_xms_system_error into lo_xms_system_error.
    endtry.
    commit work.
  endif.

  if lk_intfms-interface_dir = 'O'.
    lk_intfms_new = lk_intfms.
    lk_intfms_new-interface_msgid = lv_guid_new.
    lk_intfms_new-execute_date = sy-datum.
    lk_intfms_new-execute_time = sy-uzeit.
    modify /pstech/bcintfms from lk_intfms_new.
  endif.

endform.                    " DATA_PROCESS_PROXY_COPYPROC

*&---------------------------------------------------------------------*
*&      Form  data_process_proxy_editproc
*&---------------------------------------------------------------------*
form data_process_proxy_editproc using lk_intfms type gtk_intfms.
  data lv_contenttype       type string.
  data lv_guid_new          type sxmsguid.
  data lk_intfms_new        type /pstech/bcintfms.
  data lo_xms_main          type ref to cl_xms_main.
  data lo_xms_system_error  type ref to cx_xms_system_error. "#EC NEEDED
  data lo_xms_runtime       type ref to cl_xms_run_time_env.
  data lk_xms_runtime_eoref type sxmseoref.
  data lo_xms_message       type ref to if_xms_message.
  data lo_xms_message_xmb   type ref to if_xms_message_xmb.
  data lo_xms_payload       type ref to if_xms_payload.
  data lv_timestamp         type timestamp.
  data lo_xms_hoplist       type ref to cl_xms_msghdr30_hoplist.
  data lv_xms_hoplist_count type i.
  data lo_xms_engine        type ref to if_xms_engine.
  data lv_adapter_type      type sxmspstype.

  check gv_xml_document is not initial.

  lo_xms_main = cl_xms_main=>create_xmb( ).

* reinstantiate the message-object
  try.
      call method lo_xms_main->read_message_from_persist
        exporting
          im_message_guid = lk_intfms-proxy_msgguid
          im_version      = '000'
          im_pipeline_id  = lk_intfms-proxy_pid
        importing
          ex_message      = lo_xms_message.
    catch cx_xms_system_error into lo_xms_system_error.
  endtry.

  if lo_xms_message is not initial.
    "Create new GUID for the message
    lv_guid_new = cl_xms_msg_util=>create_guid( ).

    lo_xms_message_xmb ?= lo_xms_message.

    "Set Runtime Message
    call method cl_xms_main=>get_message_properties
      exporting
        im_message      = lo_xms_message
      importing
        ex_run_time_env = lo_xms_runtime.

    lk_xms_runtime_eoref-id = 'MODIFY'.
    concatenate  sy-uname ':' lk_intfms-interface_msgid into lk_xms_runtime_eoref-val.
    lo_xms_runtime->set_user_name( sy-uname ).
    lo_xms_runtime->set_eo_ref_inbound( lk_xms_runtime_eoref ).

    "set attributes of the message-header
    lo_xms_message_xmb->set_message_id( lv_guid_new ).
    "set send date and time
    get time stamp field lv_timestamp.
    lo_xms_message_xmb->set_time_sent( lv_timestamp ).

    "Remove last hoplist
    call method cl_xms_main=>get_message_properties
      exporting
        im_message = lo_xms_message
      importing
        ex_hoplist = lo_xms_hoplist.

    if lo_xms_hoplist is not initial.
      describe table lo_xms_hoplist->hoplist lines lv_xms_hoplist_count.
      delete  lo_xms_hoplist->hoplist from lv_xms_hoplist_count.
    endif.

    "Update Payload
    try.
        lo_xms_payload = lo_xms_message_xmb->get_payload_by_name( lo_xms_message_xmb->co_payloadname_main ).
        lv_contenttype = lo_xms_payload->getcontenttype( ).
        lo_xms_payload->setbinarycontent( data = gv_xml_document type = lv_contenttype ).

        case lk_intfms-interface_dir.
          when 'I'.
            lv_adapter_type = 'PLAINHTTP'.
          when 'O'.
            lv_adapter_type = ''.
        endcase.


        lo_xms_engine = cl_xms_main=>create_engine( ).
        call method lo_xms_engine->enter_engine
          exporting
            im_execute_flag = '1'
            im_adapter_id   = lv_adapter_type
          changing
            ch_message      = lo_xms_message_xmb.

      catch cx_xms_system_error into lo_xms_system_error.
    endtry.
    commit work.

    if lk_intfms-interface_dir = 'O'.
      lk_intfms_new = lk_intfms.
      lk_intfms_new-interface_msgid = lv_guid_new.
      lk_intfms_new-execute_date = sy-datum.
      lk_intfms_new-execute_time = sy-uzeit.
      modify /pstech/bcintfms from lk_intfms_new.
    endif.

  endif.


endform.                    " DATA_PROCESS_PROXY_EDITPROC


*&---------------------------------------------------------------------*
*&      Form  screen_alvvar_default
*&---------------------------------------------------------------------*
form screen_alvvar_default changing lv_alvvar type slis_vari.
  data: lk_alv_layout type salv_s_layout_info,
        lk_alv_key    type salv_s_layout_key.
  lk_alv_key-report = sy-repid.
  lk_alv_layout = cl_salv_layout_service=>get_default_layout( s_key = lk_alv_key ).
  lv_alvvar = lk_alv_layout-layout.
endform.                    "screen_alvvar_default
*&---------------------------------------------------------------------*
*&      Form  screen_alvvar_searchvalue
*&---------------------------------------------------------------------*
form screen_alvvar_searchvalue  changing lv_alvvar type slis_vari.
  data: lk_alv_layout type salv_s_layout_info,
        lk_alv_key    type salv_s_layout_key.
  lk_alv_key-report = sy-repid.
  lk_alv_layout = cl_salv_layout_service=>f4_layouts( s_key = lk_alv_key ).
  lv_alvvar = lk_alv_layout-layout.
endform.                    "screen_alvvar_searchvalue
*&---------------------------------------------------------------------*
*&      Form  screen_intfid_searchvalue
*&---------------------------------------------------------------------*
form screen_intfid_searchvalue.
  types: begin of ltk_request,
          intfid type /pstech/bcintfid,
          desc   type /pstech/bcintfmm-interface_desc,
         end of ltk_request.

  data: li_request           type standard table of ltk_request.
  data: li_return            type rsdm_f4_return_values.
  field-symbols <lk_return>  type ddshretval.
  field-symbols <lk_request> type ltk_request.

  select interface_id interface_desc into table li_request from /pstech/bcintfmm.

  loop at li_request assigning <lk_request>.
    authority-check object 'ZBC_INTF'
             id '/PSTECH/II' field <lk_request>-intfid
             id 'ACTVT' field '03'.
    if sy-subrc ne 0.
      delete li_request index sy-tabix.
    endif.
  endloop.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield   = 'INTFID'   "field of internal table
      value_org  = 'S'
    tables
      value_tab  = li_request
      return_tab = li_return.

  loop at li_return assigning <lk_return>.
    s_intfid-sign = 'I'.
    s_intfid-option = 'EQ'.
    s_intfid-low = <lk_return>-fieldval.
  endloop.

endform.                    "screen_intfid_searchvalue
*&---------------------------------------------------------------------*
*&      Form  data_display_screen
*&---------------------------------------------------------------------*
form data_display_screen.
  data lv_table_count type i.

  describe table gi_intfms lines lv_table_count.
  set titlebar '000' with lv_table_count.
  set pf-status 'STATUS_1001'.
  if go_main_container is initial.
    create object go_main_container
      exporting
        container_name = 'DISPLAY_CTRL'.

    create object go_main_split_container
      exporting
        parent  = go_main_container
        rows    = 1
        columns = 2.

    go_main_alv_container = go_main_split_container->get_container( row = 1 column = 1 ).

    try.
        cl_salv_table=>factory(
          exporting
            r_container  = go_main_alv_container
          importing
            r_salv_table = go_main_alv_table
          changing
            t_table      = gi_intfms ).
        perform data_display_main_alv_config.
      catch cx_salv_msg.
    endtry.
  endif.
endform.                    " data_display_screen
*&---------------------------------------------------------------------*
*&      Form  data_display_settings
*&---------------------------------------------------------------------*
form data_display_settings.
  data: li_xml_filter_list type vrm_values, lk_xml_filter_list type vrm_value.
  lk_xml_filter_list-key = '1'. lk_xml_filter_list-text = 'OR'.  append lk_xml_filter_list to li_xml_filter_list.
  lk_xml_filter_list-key = '2'. lk_xml_filter_list-text = 'AND'. append lk_xml_filter_list to li_xml_filter_list.

  call function 'VRM_SET_VALUES'
    exporting
      id     = 'S_XMLO1'
      values = li_xml_filter_list.

  "View Toolbars
  data: lk_button  type stb_button.
  clear lk_button.
  lk_button-butn_type  = 0.
  lk_button-function   = 'NODEEXPAND'.
  lk_button-icon       = '@3S@'.
  lk_button-quickinfo  = 'Expand All Nodes'.
  append lk_button to gi_idoc_tree_button.

  clear lk_button.
  lk_button-butn_type  = 0.
  lk_button-function   = 'NODECOLLAPSE'.
  lk_button-icon       = '@3T@'.
  lk_button-quickinfo  = 'Collapse All Nodes'.
  append lk_button to gi_idoc_tree_button.

  clear lk_button.
  lk_button-butn_type  = 3.
  append lk_button to gi_idoc_tree_button.

  clear lk_button.
  lk_button-butn_type  = 0.
  lk_button-function   = 'TREE_SEARCH'.
  lk_button-icon       = '@13@'.
  lk_button-quickinfo  = 'Find'.
  append lk_button to gi_idoc_tree_button.

  clear lk_button.
  lk_button-butn_type  = 0.
  lk_button-function   = 'TREE_SEARCH_NEXT'.
  lk_button-icon       = '@4E@'.
  lk_button-quickinfo  = 'Find'.
  append lk_button to gi_idoc_tree_button.

  clear lk_button.
  lk_button-butn_type  = 3.
  append lk_button to gi_idoc_tree_button.

  clear lk_button.
  lk_button-butn_type  = 0.
  lk_button-icon       = '@2L@'.
  lk_button-function   = 'EDITPROCESS'.
  lk_button-quickinfo  = 'Save and Process'.
  lk_button-text       = 'Save and Process'.
  append lk_button to gi_idoc_tree_button.

  clear lk_button.
  lk_button-butn_type  = 0.
  lk_button-function   = 'NODEEXPAND'.
  lk_button-icon       = '@3S@'.
  lk_button-quickinfo  = 'Expand All Nodes'.
  append lk_button to gi_proxy_tree_button.

  clear lk_button.
  lk_button-butn_type  = 0.
  lk_button-function   = 'NODECOLLAPSE'.
  lk_button-icon       = '@3T@'.
  lk_button-quickinfo  = 'Collapse All Nodes'.
  append lk_button to gi_proxy_tree_button.

  clear lk_button.
  lk_button-butn_type  = 3.
  append lk_button to gi_proxy_tree_button.

  clear lk_button.
  lk_button-butn_type  = 0.
  lk_button-function   = 'TREE_SEARCH'.
  lk_button-icon       = '@13@'.
  lk_button-quickinfo  = 'Find'.
  append lk_button to gi_proxy_tree_button.

  clear lk_button.
  lk_button-butn_type  = 0.
  lk_button-function   = 'TREE_SEARCH_NEXT'.
  lk_button-icon       = '@4E@'.
  lk_button-quickinfo  = 'Find'.
  append lk_button to gi_proxy_tree_button.

  clear lk_button.
  lk_button-butn_type  = 3.
  append lk_button to gi_proxy_tree_button.

  clear lk_button.
  lk_button-butn_type  = 0.
  lk_button-function   = 'PROXY_XML'.
  lk_button-icon       = '@J4@'.
  lk_button-quickinfo  = 'XML Editor'.
  append lk_button to gi_proxy_tree_button.

  clear lk_button.
  lk_button-butn_type  = 3.
  append lk_button to gi_proxy_tree_button.

  clear lk_button.
  lk_button-butn_type  = 0.
  lk_button-icon       = '@2L@'.
  lk_button-function   = 'EDITPROCESS'.
  lk_button-quickinfo  = 'Save and Process'.
  lk_button-text       = 'Save and Process'.
  append lk_button to gi_proxy_tree_button.

  clear lk_button.
  lk_button-butn_type  = 0.
  lk_button-function   = 'PROXY_XML_TOGGLE'.
  lk_button-icon       = '@3I@'.
  lk_button-quickinfo  = 'Toggle Display/Change'.
  append lk_button to gi_proxy_xml_button.

  clear lk_button.
  lk_button-butn_type  = 0.
  lk_button-function   = 'PROXY_XML_STORE'.
  lk_button-icon       = '@49@'.
  lk_button-quickinfo  = 'Save to file ...'(005).
  append lk_button to gi_proxy_xml_button.

  clear lk_button.
  lk_button-butn_type  = 3.
  append lk_button to gi_proxy_xml_button.

  clear lk_button.
  lk_button-butn_type  = 0.
  lk_button-function   = 'PROXY_TREE'.
  lk_button-icon       = '@JG@'.
  lk_button-quickinfo  = 'XML Tree'.
  append lk_button to gi_proxy_xml_button.

  clear lk_button.
  lk_button-butn_type  = 3.
  append lk_button to gi_proxy_xml_button.

  clear lk_button.
  lk_button-butn_type  = 0.
  lk_button-function   = 'PROXY_XML_PRETTY'.
  lk_button-icon       = '@FZ@'.
  lk_button-quickinfo  = 'Format and Indent'.
  lk_button-text       = 'Format and Indent'.
  append lk_button to gi_proxy_xml_button.

  clear lk_button.
  lk_button-butn_type  = 3.
  append lk_button to gi_proxy_xml_button.

  clear lk_button.
  lk_button-butn_type  = 0.
  lk_button-icon       = '@2L@'.
  lk_button-function   = 'EDITPROCESS'.
  lk_button-quickinfo  = 'Save and Process'.
  lk_button-text       = 'Save and Process'.
  append lk_button to gi_proxy_xml_button.

  "Tree Context
  data: lk_context type uac_s_context_menu.

  clear lk_context.
  lk_context-menu_item_id       = 'XML_DUPNODE'.
  lk_context-menu_function_code = 'XML_DUPNODE'.
  lk_context-is_separator       = '0'.
  lk_context-disabled           = '0'.
  lk_context-value              = 'Duplicate Node'.
  append lk_context to gi_proxy_tree_context.

  clear lk_context.
  lk_context-menu_item_id       = '00002'.
  lk_context-is_separator       = '1'.
  append lk_context to gi_proxy_tree_context.

  clear lk_context.
  lk_context-menu_item_id       = 'XML_ADDELEM'.
  lk_context-menu_function_code = 'XML_ADDELEM'.
  lk_context-is_separator       = '0'.
  lk_context-disabled           = '0'.
  lk_context-value              = 'Add Element'.
  append lk_context to gi_proxy_tree_context.

  clear lk_context.
  lk_context-menu_item_id       = 'XML_ADDELEM2'.
  lk_context-menu_function_code = 'XML_ADDELEM2'.
  lk_context-is_separator       = '0'.
  lk_context-disabled           = '0'.
  lk_context-value              = 'Add Sub-Element'.
  append lk_context to gi_proxy_tree_context.

  clear lk_context.
  lk_context-menu_item_id       = 'XML_ADDATTR'.
  lk_context-menu_function_code = 'XML_ADDATTR'.
  lk_context-is_separator       = '0'.
  lk_context-disabled           = '0'.
  lk_context-value              = 'Add Attribute'.
  append lk_context to gi_proxy_tree_context.

  clear lk_context.
  lk_context-menu_item_id       = '00005'.
  lk_context-is_separator       = '1'.
  lk_context-disabled           = '0'.
  append lk_context to gi_proxy_tree_context.

  clear lk_context.
  lk_context-menu_item_id       = 'XML_DELNODE'.
  lk_context-menu_function_code = 'XML_DELNODE'.
  lk_context-is_separator       = '0'.
  lk_context-disabled           = '0'.
  lk_context-value              = 'Delete Node'.
  lk_context-icon               = '@18@'.
  append lk_context to gi_proxy_tree_context.

  clear lk_context.
  lk_context-menu_item_id       = 'IDOC_DUPNODE'.
  lk_context-menu_function_code = 'IDOC_DUPNODE'.
  lk_context-is_separator       = '0'.
  lk_context-disabled           = '0'.
  lk_context-value              = 'Duplicate Structure'.
  append lk_context to gi_idoc_tree_context.

  clear lk_context.
  lk_context-menu_item_id       = '00001'.
  lk_context-is_separator       = '1'.
  lk_context-disabled           = '0'.
  append lk_context to gi_idoc_tree_context.

  clear lk_context.
  lk_context-menu_item_id       = 'IDOC_ADDSTR1'.
  lk_context-menu_function_code = 'IDOC_ADDSTR1'.
  lk_context-is_separator       = '0'.
  lk_context-disabled           = '0'.
  lk_context-value              = 'Add Structure'.
  append lk_context to gi_idoc_tree_context.

  clear lk_context.
  lk_context-menu_item_id       = 'IDOC_ADDSTR2'.
  lk_context-menu_function_code = 'IDOC_ADDSTR2'.
  lk_context-is_separator       = '0'.
  lk_context-disabled           = '0'.
  lk_context-value              = 'Add Sub-Structure'.
  append lk_context to gi_idoc_tree_context.

  clear lk_context.
  lk_context-menu_item_id       = '00002'.
  lk_context-is_separator       = '1'.
  lk_context-disabled           = '0'.
  append lk_context to gi_idoc_tree_context.

  clear lk_context.
  lk_context-menu_item_id       = 'IDOC_DELNODE'.
  lk_context-menu_function_code = 'IDOC_DELNODE'.
  lk_context-is_separator       = '0'.
  lk_context-disabled           = '0'.
  lk_context-value              = 'Delete Structure'.
  append lk_context to gi_idoc_tree_context.


endform.                    "data_display_settings
*&---------------------------------------------------------------------*
*&      Form  DATA_DISPLAY_COMMANDS
*&---------------------------------------------------------------------*
form data_display_commands  using lv_okcode type syucomm.
  data: lv_popup_answer    type c length 1.
  data: lv_table_count     type i.
  data: lv_total_count_msg type string.
  data: lo_alv_selections  type ref to cl_salv_selections.
  data: li_alv_rows        type salv_t_row.
  data: lk_intfms          type gtk_intfms.
  data: li_intfms          type gti_intfms.
  data: lk_intfms_table    type /pstech/bcintfms.
  data: li_intfms_table    type standard table of /pstech/bcintfms.
  data: lv_tree_node_id    type uac_node_id.
  data: li_tree_nodes      type uac_t_node_long.
  data: li_submit_param    type rsparams_tt.
  data: lk_submit_param    type rsparams.

  field-symbols <lk_tree_nodes> type uac_s_node_long.
  field-symbols <lk_alv_rows> like line of li_alv_rows.
  field-symbols <lk_intfms>   type gtk_intfms.

  clear : lv_popup_answer, lv_table_count.

  case lv_okcode.
    when 'EXIT' or 'CANC' or 'BACK' or 'OK'.
      set screen 0.
      leave screen.

    when 'COUNT'.
      describe table gi_intfms lines lv_table_count.
      lv_total_count_msg = lv_table_count.
      concatenate 'Total Messages:' lv_total_count_msg into lv_total_count_msg separated by space.
      call function 'POPUP_TO_INFORM'
        exporting
          titel = 'Message Total'
          txt1  = lv_total_count_msg
          txt2  = ' '.

    when 'XMLTABLE'.
      go_main_alv_table->get_metadata( ).
      lo_alv_selections = go_main_alv_table->get_selections( ).
      li_alv_rows = lo_alv_selections->get_selected_rows( ).
      clear: lk_submit_param, li_submit_param[].
      loop at li_alv_rows assigning <lk_alv_rows>.
        read table gi_intfms assigning <lk_intfms> index <lk_alv_rows>.
        if <lk_intfms>-proxy_msgguid is not initial.
          lk_submit_param-selname = 'S_GUID'.
          lk_submit_param-kind    = 'S'.
          lk_submit_param-sign    = 'I'.
          lk_submit_param-option  = 'EQ'.
          lk_submit_param-low     = <lk_intfms>-proxy_msgguid.
          append lk_submit_param to li_submit_param.
        endif.
      endloop.

      clear lk_submit_param.
      lk_submit_param-selname = 'S_RFCGRP'.
      lk_submit_param-kind    = 'P'.
      lk_submit_param-sign    = 'I'.
      lk_submit_param-option  = 'EQ'.
      lk_submit_param-low     = s_parrfc.
      append lk_submit_param to li_submit_param.

      clear lk_submit_param.
      lk_submit_param-selname = 'S_MAXPAR'.
      lk_submit_param-kind    = 'P'.
      lk_submit_param-sign    = 'I'.
      lk_submit_param-option  = 'EQ'.
      lk_submit_param-low     = s_parmax.
      append lk_submit_param to li_submit_param.
      submit /pstech/bci_intf_xmltable via selection-screen with selection-table li_submit_param and return.


      "ALV commands
    when 'DELE'.
      go_main_alv_table->get_metadata( ).
      lo_alv_selections = go_main_alv_table->get_selections( ).
      li_alv_rows = lo_alv_selections->get_selected_rows( ).
      if li_alv_rows[] is not initial.
        call function 'POPUP_TO_CONFIRM'
          exporting
            text_question         = 'Are you sure you want to delete these indexes ?'(f01)
            text_button_1         = 'Yes'(f02)
            icon_button_1         = '@01@'
            text_button_2         = 'No'(f03)
            icon_button_2         = '@02@'
            default_button        = '1'
            display_cancel_button = ' '
          importing
            answer                = lv_popup_answer.
      endif.

      if lv_popup_answer = '1'.
        loop at li_alv_rows assigning <lk_alv_rows>.
          read table gi_intfms assigning <lk_intfms> index <lk_alv_rows>.
          if sy-subrc = 0.
            lk_intfms_table = <lk_intfms>.
            append lk_intfms_table to li_intfms_table.
          endif.
        endloop.
        loop at li_intfms_table into lk_intfms_table.

          authority-check object 'ZBC_INTF'
                id '/PSTECH/II' field lk_intfms_table-interface_id
                id 'ACTVT' field '06'.
          if sy-subrc = 0.
            delete gi_intfms
              where interface_id    eq lk_intfms_table-interface_id and
                    interface_key1  eq lk_intfms_table-interface_key1 and
                    interface_key2  eq lk_intfms_table-interface_key2 and
                    interface_key3  eq lk_intfms_table-interface_key3 and
                    interface_key4  eq lk_intfms_table-interface_key4 and
                    interface_msgid eq lk_intfms_table-interface_msgid.
            lv_table_count = lv_table_count + 1.
          else.
            delete li_intfms_table from lk_intfms_table.
          endif.
        endloop.
        delete /pstech/bcintfms from table li_intfms_table.

        message s011(sv) with lv_table_count.
        clear li_alv_rows[].

        go_main_alv_table->get_metadata( ).
        lo_alv_selections = go_main_alv_table->get_selections( ).
        lo_alv_selections->set_selected_rows( li_alv_rows[] ).
        go_main_alv_table->refresh( ).
      endif.

    when 'COPYPROCESS'.
      go_main_alv_table->get_metadata( ).
      lo_alv_selections = go_main_alv_table->get_selections( ).
      li_alv_rows = lo_alv_selections->get_selected_rows( ).
      if li_alv_rows[] is not initial.
        call function 'POPUP_TO_CONFIRM'
          exporting
            text_question         = 'Are you sure you want to copy & process these indexes ?'(f04)
            text_button_1         = 'Yes'(f02)
            icon_button_1         = '@01@'
            text_button_2         = 'No'(f03)
            icon_button_2         = '@02@'
            default_button        = '1'
            display_cancel_button = ' '
          importing
            answer                = lv_popup_answer.
      endif.
      if lv_popup_answer = '1'.
        loop at li_alv_rows assigning <lk_alv_rows>.
          read table gi_intfms  assigning <lk_intfms> index <lk_alv_rows>.
          if sy-subrc = 0.
            lv_table_count = lv_table_count + 1.
            case <lk_intfms>-interface_type.
              when '0'.
                perform data_process_proxy_copyproc using <lk_intfms>.
              when '1'.
                perform data_process_idoc_copyproc using <lk_intfms>.
            endcase.
          endif.
        endloop.
        message s020(xms_moni) with lv_table_count 'messages copied & processed'.
      endif.

    when 'RESTART'.
      go_main_alv_table->get_metadata( ).
      lo_alv_selections = go_main_alv_table->get_selections( ).
      li_alv_rows = lo_alv_selections->get_selected_rows( ).
      clear : li_intfms[].
      if li_alv_rows[] is not initial.
        call function 'POPUP_TO_CONFIRM'
          exporting
            text_question         = 'Are you sure you want to restart error messages from these indexes ?'(f06)
            text_button_1         = 'Yes'(f02)
            icon_button_1         = '@01@'
            text_button_2         = 'No'(f03)
            icon_button_2         = '@02@'
            default_button        = '1'
            display_cancel_button = ' '
          importing
            answer                = lv_popup_answer.
      endif.
      if lv_popup_answer = '1'.
        loop at li_alv_rows assigning <lk_alv_rows>.
          read table gi_intfms assigning <lk_intfms> index <lk_alv_rows>.
          if sy-subrc = 0.
            lv_table_count = lv_table_count + 1.
            case <lk_intfms>-interface_type.
              when '0'.
                perform data_process_proxy_restart using <lk_intfms>.
              when '1'.
                append <lk_intfms> to li_intfms.
            endcase.
          endif.
        endloop.

        if li_intfms[] is not initial.
          perform data_process_idoc_restart using li_intfms[].
        endif.

        message s020(xms_moni) with lv_table_count 'messages copied & processed'.
      endif.

    when 'REFRSH'.
      clear gi_intfms[].
      perform data_retrieve.
      go_main_alv_table->refresh( ).

    when 'TREE_SEARCH'.
      if go_proxy_tree is not initial.
        perform data_display_tree_search changing go_proxy_tree.
      endif.
      if go_idoc_tree is not initial.
        perform data_display_tree_search changing go_idoc_tree.
      endif.

    when 'TREE_SEARCH_NEXT'.
      if go_proxy_tree is not initial.
        perform data_display_tree_search_next changing go_proxy_tree.
      endif.
      if go_idoc_tree is not initial.
        perform data_display_tree_search_next changing go_idoc_tree.
      endif.

    when 'TREE_HIDE_EMPTY_FIELD'.
      if go_idoc_tree is not initial.
        perform data_display_tree_search_next changing go_idoc_tree.
      endif.
    when 'TREE_SHOW_EMPTY_FIELD'.
      if go_idoc_tree is not initial.
        perform data_display_tree_search_next changing go_idoc_tree.
      endif.

    when 'PROXY_XML'.
      if go_proxy_tree is not initial.
        perform data_display_proxy_tree2xml using go_proxy_tree changing gv_xml_document.
      endif.
      perform data_display_proxy_xml using gk_intfms.

    when 'PROXY_TREE'.
      if go_proxy_xml is not initial.
        gv_xml_document = go_proxy_xml->get_xstring( ).
      endif.
      perform data_display_proxy_tree using gk_intfms.


      "XML Commands
    when 'PROXY_XML_TOGGLE'.
      if go_proxy_xml is not initial.
        if go_proxy_xml->get_change_mode( ) = abap_true.
          go_proxy_xml->set_change_mode( abap_false ).
        else.
          go_proxy_xml->pretty_print( ).
          go_proxy_xml->set_change_mode( abap_true ).
        endif.
      endif.

    when 'PROXY_XML_STORE'.
      if go_proxy_xml is not initial.
        data : lv_payload type xstring.
        lv_payload = go_proxy_xml->get_xstring( ).
        try.
            cl_proxy_adapter_test=>download_payload( lv_payload ).
          catch cx_proxy_gen_error.                     "#EC NO_HANDLER
        endtry.
      endif.

    when 'PROXY_XML_PRETTY'.
      if go_proxy_xml is not initial.
        go_proxy_xml->pretty_print( ).
      endif.

    when 'EDITPROCESS'.
      clear lv_popup_answer.
      call function 'POPUP_TO_CONFIRM'
        exporting
          text_question         = 'Are you sure you want to process this message ?'(f05)
          text_button_1         = 'Yes'(f02)
          icon_button_1         = '@01@'
          text_button_2         = 'No'(f03)
          icon_button_2         = '@02@'
          default_button        = '1'
          display_cancel_button = ' '
        importing
          answer                = lv_popup_answer.
      if lv_popup_answer = '1'.
        case gk_intfms-interface_type.
          when '0'.
            if go_proxy_xml is not initial.
              gv_xml_document = go_proxy_xml->get_xstring( ).
            endif.
            if go_proxy_tree is not initial.
              perform data_display_proxy_tree2xml using go_proxy_tree changing gv_xml_document.
            endif.
            perform data_process_proxy_editproc using gk_intfms.
            message s020(xms_moni) with 'XML message saved & processed'.
          when '1'.
            if go_idoc_tree is not initial.
              perform data_process_idoc_edittree.
              message s020(xms_moni) with 'IDoc message saved & processed'.
            endif.
        endcase.
      endif.

      "Tree Commands
    when 'NODEEXPAND'.
      if go_idoc_tree is not initial.
        go_idoc_tree->get_selected_node( importing ed_node_id = lv_tree_node_id ).
        go_idoc_tree->expand_node( id_node_id = lv_tree_node_id id_levels = /pstech/cl_bc_gui_edit_tree=>cd_entire_subtree ).
      endif.
      if go_proxy_tree is not initial.
        go_proxy_tree->get_selected_node( importing ed_node_id = lv_tree_node_id ).
        go_proxy_tree->expand_node( id_node_id = lv_tree_node_id id_levels = /pstech/cl_bc_gui_edit_tree=>cd_entire_subtree ).
      endif.

    when 'NODECOLLAPSE'.
      if go_idoc_tree is not initial.
        go_idoc_tree->get_selected_node( importing ed_node_id = lv_tree_node_id ).
        if lv_tree_node_id is initial.
          li_tree_nodes = go_idoc_tree->get_nodes( ).
          loop at li_tree_nodes assigning <lk_tree_nodes> where parent_node is initial.
            go_idoc_tree->collapse_node( id_node_id = <lk_tree_nodes>-node_id ).
          endloop.
        else.
          go_idoc_tree->collapse_node( id_node_id = lv_tree_node_id ).
        endif.
      endif.
      if go_proxy_tree is not initial.
        go_proxy_tree->get_selected_node( importing ed_node_id = lv_tree_node_id ).
        if lv_tree_node_id is initial.
          li_tree_nodes = go_proxy_tree->get_nodes( ).
          loop at li_tree_nodes assigning <lk_tree_nodes> where parent_node is initial.
            go_proxy_tree->collapse_node( id_node_id = <lk_tree_nodes>-node_id ).
          endloop.
        else.
          go_proxy_tree->collapse_node( id_node_id = lv_tree_node_id ).
        endif.
      endif.

  endcase.
endform.                    " DATA_DISPLAY_COMMANDS
*&---------------------------------------------------------------------*
*&      Form  data_display_tree_commands
*&---------------------------------------------------------------------*
form data_display_tree_commands using lo_tree            type ref to /pstech/cl_bc_gui_edit_tree
                                      lv_tree_nodeid_sel type uac_node_id
                                      lv_okcode          type uac_menu_function_code.
  types: begin of ltk_idoc_segments,
           segtyp type edilsegtyp,
         end of ltk_idoc_segments.
  data: lv_tree_nodeid_new      type uac_node_id,
        lv_tree_nodeid_new_elem type uac_node_id,
        lv_tree_nodeid_new_attr type uac_node_id.
  data: lk_tree_node_new        type uac_s_node_long,
        lk_tree_node_sel        type uac_s_node_long,
        lk_tree_node_parent     type uac_s_node_long,
        lk_tree_node_last       type uac_s_node_long.

  data: li_idoc_segments        type standard table of ltk_idoc_segments.
  data: lv_idoc_parseg          type idocsyn-parseg.
  data: lk_idoc_control         type edidc.
  data: li_idoc_segment_sel     type rsdm_f4_return_values.
  data: li_tree_nodes           type uac_t_node_long.
  data: lv_idoc_node_curr       type uac_node_id,
        lv_idoc_node_last       type uac_node_id.
  data: lo_idoc_segment         type ref to data.
  data: lo_data_typedescr       type ref to cl_abap_typedescr.
  data: li_data_fields          type dd_x031l_table.
  data: lv_idoc_last_num        type n length 6.
  field-symbols <lk_data_fields>      type x031l.
  field-symbols:<lk_idoc_segment>     type any,
                <lk_idoc_field>       type any.
  field-symbols <lk_idoc_segment_sel> type ddshretval.

  case lv_okcode.
    when 'XML_DUPNODE' or 'IDOC_DUPNODE'.
      lv_tree_nodeid_new = lo_tree->copy_subtree( id_source_node_id = lv_tree_nodeid_sel id_target_node_id = lv_tree_nodeid_sel id_with_cells = '1' ).
      lo_tree->change_node( id_node_id = lv_tree_nodeid_new id_editable = '1' id_u_editable = '1' id_style_id = lk_tree_node_new-style_id ).
      lo_tree->expand_node( id_node_id = lv_tree_nodeid_new ).
      lo_tree->set_selected_node( id_node_id = lv_tree_nodeid_new ).
    when 'XML_DELNODE' or 'IDOC_DELNODE'.
      lo_tree->remove_subtree( id_node_id = lv_tree_nodeid_sel ).
    when 'XML_ADDELEM'.
      lv_tree_nodeid_new_elem = lo_tree->add_node( id_relat_node = lv_tree_nodeid_sel id_relationship = /pstech/cl_bc_gui_edit_tree=>cd_following_sibling id_value = ''  id_is_leaf = '1'  id_editable = '1' id_style_id = 'XML_ELEM' ).
      lo_tree->add_cell( id_node_id = lv_tree_nodeid_new_elem id_column_id = '1' id_editable = '1' id_value = '' ).
      lo_tree->set_selected_node( id_node_id = lv_tree_nodeid_new_elem ).
    when 'XML_ADDELEM2'.
      lo_tree->get_node( exporting id_node_id = lv_tree_nodeid_sel importing es_node = lk_tree_node_sel ).
      if lk_tree_node_sel-style_id = 'XML_ELEM'.
        lo_tree->change_node( id_node_id = lv_tree_nodeid_sel id_editable = '0' id_u_editable = '0' id_style_id = 'XML_STRUC' ).
        lo_tree->change_cell( id_node_id = lv_tree_nodeid_sel id_column_id = '1' id_value = '' id_u_value = '1' id_editable = '0' id_u_editable = '1' ).
      endif.
      if lk_tree_node_sel-is_leaf = '1'.
        lo_tree->change_node_leaf( id_node_id = lv_tree_nodeid_sel id_is_leaf = '0' ).
        lv_tree_nodeid_new = lo_tree->copy_subtree( id_source_node_id = lv_tree_nodeid_sel id_target_node_id = lv_tree_nodeid_sel id_with_cells = '1' ).
        lv_tree_nodeid_new_elem = lo_tree->add_node( id_relat_node = lv_tree_nodeid_new id_relationship = /pstech/cl_bc_gui_edit_tree=>cd_last_child id_value = ''  id_is_leaf = '1'  id_editable = '1' id_style_id = 'XML_ELEM' ).
        lo_tree->add_cell( id_node_id = lv_tree_nodeid_new_elem id_column_id = '1' id_editable = '1' id_value = '' ).
        lo_tree->expand_node( id_node_id = lv_tree_nodeid_new ).
        lo_tree->remove_subtree( id_node_id = lv_tree_nodeid_sel ).
        lo_tree->set_selected_node( id_node_id = lv_tree_nodeid_new_elem ).
      else.
        lv_tree_nodeid_new_elem = lo_tree->add_node( id_relat_node = lv_tree_nodeid_sel id_relationship = /pstech/cl_bc_gui_edit_tree=>cd_last_child id_value = ''  id_is_leaf = '1'  id_editable = '1' id_style_id = 'XML_ELEM' ).
        lo_tree->add_cell( id_node_id = lv_tree_nodeid_new_elem id_column_id = '1' id_editable = '1' id_value = '' ).
        lo_tree->expand_node( id_node_id = lv_tree_nodeid_new ).
        lo_tree->set_selected_node( id_node_id = lv_tree_nodeid_new_elem ).
      endif.
    when 'XML_ADDATTR'.
      lo_tree->get_node( exporting id_node_id = lv_tree_nodeid_sel importing es_node = lk_tree_node_sel ).
      if lk_tree_node_sel-is_leaf = '1'.
        lo_tree->change_node_leaf( id_node_id = lv_tree_nodeid_sel id_is_leaf = '0' ).
        lv_tree_nodeid_new = lo_tree->copy_subtree( id_source_node_id = lv_tree_nodeid_sel id_target_node_id = lv_tree_nodeid_sel id_with_cells = '1' ).
        lv_tree_nodeid_new_attr = lo_tree->add_node( id_relat_node = lv_tree_nodeid_new id_relationship = /pstech/cl_bc_gui_edit_tree=>cd_last_child id_value = ''  id_is_leaf = '1'  id_editable = '1' id_style_id = 'XML_ATTR' ).
        lo_tree->add_cell( id_node_id = lv_tree_nodeid_new_attr id_column_id = '1' id_editable = '1' id_value = '' ).
        lo_tree->expand_node( id_node_id = lv_tree_nodeid_new ).
        lo_tree->remove_subtree( id_node_id = lv_tree_nodeid_sel ).
        lo_tree->set_selected_node( id_node_id = lv_tree_nodeid_new_attr ).
      else.
        lv_tree_nodeid_new_attr = lo_tree->add_node( id_relat_node = lv_tree_nodeid_sel id_relationship = /pstech/cl_bc_gui_edit_tree=>cd_last_child id_value = ''  id_is_leaf = '1'  id_editable = '1' id_style_id = 'XML_ATTR' ).
        lo_tree->add_cell( id_node_id = lv_tree_nodeid_new_attr id_column_id = '1' id_editable = '1' id_value = '' ).
        lo_tree->expand_node( id_node_id = lv_tree_nodeid_new ).
        lo_tree->set_selected_node( id_node_id = lv_tree_nodeid_new_attr ).
      endif.
    when 'IDOC_ADDSTR1' or 'IDOC_ADDSTR2'.
      lo_tree->get_node( exporting id_node_id = lv_tree_nodeid_sel importing es_node = lk_tree_node_sel ).
      if lk_tree_node_sel-style_id eq 'IDOC_NODE'.
        lo_tree->get_node( exporting id_node_id = lk_tree_node_sel-parent_node  importing es_node = lk_tree_node_sel ).
      endif.

      if lv_okcode = 'IDOC_ADDSTR1'.
        lo_tree->get_node( exporting id_node_id = lk_tree_node_sel-parent_node  importing es_node = lk_tree_node_parent ).
      endif.

      if lk_tree_node_sel-value ne 'Control Record' or lk_tree_node_sel-value ne 'Status Records'.
        select single * into lk_idoc_control from edidc where docnum = gk_intfms-idoc_docnum.

        if lv_okcode = 'IDOC_ADDSTR1'.
          if lk_tree_node_parent-value eq 'Data Records'.
            select segtyp into table li_idoc_segments from idocsyn where idoctyp = lk_idoc_control-idoctp and nr = 1.
          else.
            if lk_tree_node_parent-value is not initial.
              lv_idoc_parseg = lk_tree_node_parent-value.
              select segtyp into table li_idoc_segments from idocsyn where idoctyp = lk_idoc_control-idoctp and parseg = lv_idoc_parseg.
            endif.
          endif.
        else.
          if lk_tree_node_sel-value = 'Data Records'.
            select segtyp into table li_idoc_segments from idocsyn where idoctyp = lk_idoc_control-idoctp and nr = 1.
          else.
            if lk_tree_node_sel-value is not initial.
              lv_idoc_parseg = lk_tree_node_sel-value.
              select segtyp into table li_idoc_segments from idocsyn where idoctyp = lk_idoc_control-idoctp and parseg = lv_idoc_parseg.
            endif.
          endif.
        endif.

        call function 'F4IF_INT_TABLE_VALUE_REQUEST'
          exporting
            retfield   = 'SEGTYP'
            value_org  = 'S'
          tables
            value_tab  = li_idoc_segments
            return_tab = li_idoc_segment_sel.
      endif.

      read table li_idoc_segment_sel assigning <lk_idoc_segment_sel> index 1.
      if sy-subrc = 0.
        create data lo_idoc_segment type (<lk_idoc_segment_sel>-fieldval).
        assign lo_idoc_segment->* to <lk_idoc_segment>.
        lo_data_typedescr = cl_abap_typedescr=>describe_by_name( <lk_idoc_segment_sel>-fieldval ).
        li_data_fields = lo_data_typedescr->get_ddic_object( ).
        li_tree_nodes = lo_tree->get_nodes( ).
        delete li_tree_nodes where node_id np 'EDIDD*'.
        sort li_tree_nodes by node_id descending.
        read table li_tree_nodes into lk_tree_node_last index 1.
        lv_idoc_last_num = lk_tree_node_last-node_id+6 + 1.
        concatenate 'EDIDD' lv_idoc_last_num into lv_idoc_node_curr.
        if lv_okcode = 'IDOC_ADDSTR1'.
          lo_tree->add_node( id_node_id = lv_idoc_node_curr id_relat_node = lk_tree_node_sel-node_id id_relationship = /pstech/cl_bc_gui_edit_tree=>cd_following_sibling id_value = <lk_idoc_segment_sel>-fieldval id_style_id = 'IDOC_STRUC' ).
        else.
          lo_tree->add_node( id_node_id = lv_idoc_node_curr id_relat_node = lk_tree_node_sel-node_id id_relationship = /pstech/cl_bc_gui_edit_tree=>cd_last_child id_value = <lk_idoc_segment_sel>-fieldval id_style_id = 'IDOC_STRUC' ).
        endif.
        loop at li_data_fields assigning <lk_data_fields>.
          assign component <lk_data_fields>-fieldname of structure <lk_idoc_segment> to <lk_idoc_field>.
          lv_idoc_node_last = lo_tree->add_node( id_value = <lk_data_fields>-fieldname id_relat_node = lv_idoc_node_curr id_relationship = /pstech/cl_bc_gui_edit_tree=>cd_last_child id_is_leaf = '1' id_style_id = 'IDOC_NODE' ).
          lo_tree->add_cell( id_node_id = lv_idoc_node_last id_column_id = '1' id_editable = '1' id_value = '' id_style_id = 'CELL1' ).
        endloop.
        lo_tree->expand_node( id_node_id = lv_idoc_node_curr ).
      endif.
  endcase.
*  lo_tree->update_frontend( ).
endform.                    "data_display_tree_commands
*&---------------------------------------------------------------------*
*&      Form  data_display_idoc_tree
*&---------------------------------------------------------------------*
form data_display_idoc_tree using lk_intfms type gtk_intfms.
  data: li_idoc_toolbar_event   type cntl_simple_events,
        lk_idoc_toolbar_event   type cntl_simple_event.
  data: lo_idoc_toolbar_handle   type ref to lcl_tool_events_handle.
  data: li_idoc_data            type standard table of edid4.
  data: li_idoc_status          type standard table of edids.
  data: lk_idoc_control         type edidc.
  data: lo_idoc_segment         type ref to data.
  data: lo_data_typedescr       type ref to cl_abap_typedescr.
  data: li_data_fields          type dd_x031l_table.
  data: lv_idoc_node_curr       type uac_node_id,
        lv_idoc_node_main       type uac_node_id,
        lv_idoc_node_last       type uac_node_id.
  data: lv_idoc_status_statva   type edi_statva.
  data: lv_idoc_status_stalight type edi_slight.
  data: lv_idoc_status_node     type string,
        lv_idoc_status_nodetext type string,
        lv_idoc_status_nodedate type string.
  data: lv_idoc_status_nodetime type c length 8.
  data: lv_tree_editable        type uac_flag.
  field-symbols: <lk_idoc_data>        type edid4.
  field-symbols: <lk_idoc_status>      type edids.
  field-symbols: <lk_data_fields>      type x031l.
  field-symbols: <lk_idoc_field>       type any,
                 <lk_idoc_segment>     type any.

  authority-check object 'ZBC_INTF'
         id '/PSTECH/II' field lk_intfms-interface_id
         id 'ACTVT' field '02'.
  if sy-subrc = 0.
    lv_tree_editable = '1'.
  else.
    lv_tree_editable = '0'.
  endif.


  clear: gi_tree_search_result, gv_tree_search_index.
  if go_log_table is not initial or go_proxy_xml is not initial or go_proxy_tree is not initial.
    go_main_split_container->remove_control( row = 1 column = 2 ).
    free: go_main_view_container, go_log_table, go_proxy_xml, go_proxy_tree, go_idoc_tree.
  endif.


  if go_main_view_container is initial.
    go_main_view_container = go_main_split_container->get_container( row = 1 column = 2 ).
    clear: go_view_split_container, go_view_tool_container, go_view_obj_container, go_view_toolbar.
  endif.

  if go_view_split_container is initial.
    create object go_view_split_container
      exporting
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

    create object go_view_toolbar
      exporting
        parent = go_view_tool_container.
    go_view_toolbar->add_button_group( gi_idoc_tree_button ).
    lk_idoc_toolbar_event-eventid = cl_gui_toolbar=>m_id_function_selected.  lk_idoc_toolbar_event-appl_event = ' '.
    append lk_idoc_toolbar_event to li_idoc_toolbar_event.
    go_view_toolbar->set_registered_events( li_idoc_toolbar_event ).
    create object lo_idoc_toolbar_handle.
    set handler lo_idoc_toolbar_handle->command     for go_view_toolbar.
  endif.

  if go_idoc_tree is not initial.
    go_idoc_tree->free( ).
    clear go_idoc_tree.
  endif.

  if go_idoc_tree is initial.
    create object go_idoc_tree
      exporting
        id_parent    = go_view_obj_container
        id_tree_text = 'IDoc'.
    perform data_display_idoc_tree_config using lv_tree_editable.
  endif.

  gk_intfms = lk_intfms.

  "Create Main IDoc Node
  go_idoc_tree->add_node( id_node_id = 'IDOC' id_relationship = /pstech/cl_bc_gui_edit_tree=>cd_last_child id_value = 'IDoc' id_style_id = 'IDOC' ).
  go_idoc_tree->add_cell( id_node_id = 'IDOC' id_column_id = '1' id_editable = '0' id_value = lk_intfms-idoc_docnum ).

  "Create IDoc Control Record
  select single * into lk_idoc_control    from edidc where docnum = lk_intfms-idoc_docnum.
  lo_data_typedescr = cl_abap_typedescr=>describe_by_name( 'EDIDC' ).
  li_data_fields = lo_data_typedescr->get_ddic_object( ).
  go_idoc_tree->add_node( id_node_id = 'EDIDC' id_relat_node = 'IDOC' id_relationship = /pstech/cl_bc_gui_edit_tree=>cd_last_child id_value = 'Control Record' id_style_id = 'IDOC_STRUC' ).
  loop at li_data_fields assigning <lk_data_fields> from 2.
    assign component <lk_data_fields>-fieldname of structure lk_idoc_control to <lk_idoc_field>.
    lv_idoc_node_last = go_idoc_tree->add_node( id_value = <lk_data_fields>-fieldname id_relat_node = 'EDIDC' id_relationship = /pstech/cl_bc_gui_edit_tree=>cd_last_child id_is_leaf = '1' id_style_id = 'IDOC_NODE' ).
    go_idoc_tree->add_cell( id_node_id = lv_idoc_node_last id_column_id = '1' id_editable = lv_tree_editable id_value = <lk_idoc_field> ).
  endloop.

  "Create IDoc Data Records
  select * into table li_idoc_data from edid4 where docnum = lk_intfms-idoc_docnum.
  go_idoc_tree->add_node( id_node_id = 'EDIDD000000' id_relat_node = 'IDOC' id_relationship = /pstech/cl_bc_gui_edit_tree=>cd_last_child id_value = 'Data Records' id_style_id = 'IDOC_STRUC' ).
  loop at li_idoc_data assigning <lk_idoc_data>.
    clear lo_idoc_segment.

    create data lo_idoc_segment type (<lk_idoc_data>-segnam).
    assign lo_idoc_segment->* to <lk_idoc_segment>.
    <lk_idoc_segment> = <lk_idoc_data>-sdata.

    lo_data_typedescr = cl_abap_typedescr=>describe_by_name( <lk_idoc_data>-segnam ).
    li_data_fields = lo_data_typedescr->get_ddic_object( ).

    concatenate 'EDIDD' <lk_idoc_data>-psgnum into lv_idoc_node_last.
    concatenate 'EDIDD' <lk_idoc_data>-segnum into lv_idoc_node_curr.

    go_idoc_tree->add_node( id_node_id = lv_idoc_node_curr id_relat_node = lv_idoc_node_last id_relationship = /pstech/cl_bc_gui_edit_tree=>cd_last_child id_value = <lk_idoc_data>-segnam id_style_id = 'IDOC_STRUC' ).

    loop at li_data_fields assigning <lk_data_fields>.
      assign component <lk_data_fields>-fieldname of structure <lk_idoc_segment> to <lk_idoc_field>.
      lv_idoc_node_last = go_idoc_tree->add_node( id_value = <lk_data_fields>-fieldname id_relat_node = lv_idoc_node_curr id_relationship = /pstech/cl_bc_gui_edit_tree=>cd_last_child id_is_leaf = '1' id_style_id = 'IDOC_NODE' ).
      go_idoc_tree->add_cell( id_node_id = lv_idoc_node_last id_column_id = '1' id_editable = lv_tree_editable id_value = <lk_idoc_field> id_style_id = 'CELL1' ).
    endloop.
  endloop.

  "IDoc Statuses
  select * into table li_idoc_status    from edids where docnum = lk_intfms-idoc_docnum.
  sort li_idoc_status by countr descending.
  go_idoc_tree->add_node( id_node_id = 'EDIDS' id_relat_node = 'IDOC' id_relationship = /pstech/cl_bc_gui_edit_tree=>cd_last_child id_value = 'Status Records' id_style_id = 'IDOC_STRUC' ).
  loop at li_idoc_status assigning <lk_idoc_status>.
    clear: lv_idoc_status_statva, lv_idoc_status_stalight, lv_idoc_status_nodedate, lv_idoc_status_nodetime, lv_idoc_status_nodetext.
    select single statva   into lv_idoc_status_statva   from teds3    where status = <lk_idoc_status>-status.
    select single stalight into lv_idoc_status_stalight from stalight where statva = lv_idoc_status_statva.

    call function 'CONVERSION_EXIT_MODAT_OUTPUT'
      exporting
        input  = <lk_idoc_status>-logdat
      importing
        output = lv_idoc_status_nodedate.
    .
    call function 'CONVERSION_EXIT_TIMLO_OUTPUT'
      exporting
        input  = <lk_idoc_status>-logtim
      importing
        output = lv_idoc_status_nodetime.

    concatenate <lk_idoc_status>-status lv_idoc_status_nodedate lv_idoc_status_nodetime into lv_idoc_status_node separated by space.

    case lv_idoc_status_stalight.
      when '1'.
        lv_idoc_node_last = go_idoc_tree->add_node( id_value = lv_idoc_status_node id_relat_node = 'EDIDS' id_relationship = /pstech/cl_bc_gui_edit_tree=>cd_last_child id_is_leaf = '1' id_style_id = 'EDIDS_W' ).
      when '2'.
        lv_idoc_node_last = go_idoc_tree->add_node( id_value = lv_idoc_status_node id_relat_node = 'EDIDS' id_relationship = /pstech/cl_bc_gui_edit_tree=>cd_last_child id_is_leaf = '1' id_style_id = 'EDIDS_S' ).
      when '3'.
        lv_idoc_node_last = go_idoc_tree->add_node( id_value = lv_idoc_status_node id_relat_node = 'EDIDS' id_relationship = /pstech/cl_bc_gui_edit_tree=>cd_last_child id_is_leaf = '1' id_style_id = 'EDIDS_E' ).
    endcase.

    message id <lk_idoc_status>-stamid type 'I' number <lk_idoc_status>-stamno with <lk_idoc_status>-stapa1 <lk_idoc_status>-stapa2 <lk_idoc_status>-stapa3 <lk_idoc_status>-stapa4 into lv_idoc_status_nodetext.
    go_idoc_tree->add_cell( id_node_id = lv_idoc_node_last id_column_id = '1' id_editable = '0' id_value = lv_idoc_status_nodetext ).
  endloop.

  go_idoc_tree->expand_node( id_node_id = 'EDIDD000000' id_levels  = 2 ).
  go_idoc_tree->expand_node( id_node_id = 'EDIDS' id_levels  = 1 ).
  go_idoc_tree->set_selected_node( id_node_id = 'IDOC' ).



  perform data_display_tree_update changing go_idoc_tree.
endform.                    "data_display_idoc
*&---------------------------------------------------------------------*
*&      Form  data_display_idoc_tree_config
*&---------------------------------------------------------------------*
form data_display_idoc_tree_config using lv_edit type uac_flag.
  data: lk_node_style       type uac_s_node_style.
  data: lk_column_style     type uac_s_column_style.
  data: lk_cell_style       type uac_s_cell_style.
  data: lo_idoc_tree_handle type ref to lcl_tree_events_handle.

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

  create object lo_idoc_tree_handle.
  if lv_edit = '1'.
    set handler lo_idoc_tree_handle->node_menu_request     for go_idoc_tree.
    set handler lo_idoc_tree_handle->node_menu_select      for go_idoc_tree.
  endif.
  set handler lo_idoc_tree_handle->node_open             for go_idoc_tree.
endform.                    "data_display_idoc_settings
*---------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*---------------------------------------------------------------------*
class lcl_alv_events_handle implementation.
  method command.
    perform data_display_commands using e_salv_function.
  endmethod.                    "on_user_command
  method linkclick.
    field-symbols: <lk_intfms> type gtk_intfms.
    case column.
      when 'INTF_MSGID_ICO'.
        clear gv_xml_document.
        read table gi_intfms assigning <lk_intfms> index row.
        if sy-subrc = 0.
          case <lk_intfms>-interface_type.
            when '0'.
              perform data_display_proxy_tree using <lk_intfms>.
            when '1'.
              perform data_display_idoc_tree using <lk_intfms>.
          endcase.
        endif.

      when 'INTF_ALOG_ICO'.
        if go_log_table is not initial.
          clear : gi_log_table.
          go_log_table->refresh( ).
        endif.
        read table gi_intfms assigning <lk_intfms> index row.
        if sy-subrc = 0.
          perform data_display_log_alv using <lk_intfms>.
        endif.

    endcase.
  endmethod.                    "doubleclick
  method doubleclick.
    field-symbols: <lk_intfms> type gtk_intfms.
    case column.
      when 'INTERFACE_MSGID'.
        read table gi_intfms assigning <lk_intfms> index row.
        if sy-subrc = 0.
          perform data_display_proxy_xml using <lk_intfms>.
        endif.
      when 'LOG_MSGTY' or 'LOG_MSGID' or 'LOG_MSGNO' or 'LOG_MSGTXT'.
        read table gi_intfms assigning <lk_intfms> index row.
        if sy-subrc = 0.
          perform data_display_log_alv using <lk_intfms>.
        endif.
      when 'INTERFACE_KEY1' or 'INTERFACE_KEY2' or 'INTERFACE_KEY3' or 'INTERFACE_KEY4'.
    endcase.
  endmethod.                    "doubleclick
endclass.                    "lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
*       CLASS lcl_tool_events_handle IMPLEMENTATION
*----------------------------------------------------------------------*
class lcl_tool_events_handle implementation.
  method command.
    perform data_display_commands using fcode.
  endmethod.                    "on_user_command
endclass.               "lcl_tool_events_handle
*----------------------------------------------------------------------*
*       CLASS lcl_tree_events_handle IMPLEMENTATION
*----------------------------------------------------------------------*
class lcl_tree_events_handle implementation.
  method node_open.
    if go_proxy_tree is not initial.
      go_proxy_tree->column_optimize( id_column_id = '&Hierarchy' ).
      go_proxy_tree->column_optimize( id_column_id = '1' ).
    endif.
    if go_idoc_tree is not initial.
      go_idoc_tree->column_optimize( id_column_id = '&Hierarchy' ).
      go_idoc_tree->column_optimize( id_column_id = '1' ).
    endif.
  endmethod.                    "node_open
  method node_menu_request.
    if go_proxy_tree is not initial.
      go_proxy_tree->display_context_menu( gi_proxy_tree_context ).
    endif.
    if go_idoc_tree is not initial.
      go_idoc_tree->display_context_menu( gi_idoc_tree_context ).
    endif.
  endmethod.                    "node_menu_request
  method node_menu_select.
    if go_proxy_tree is not initial.
      perform data_display_tree_commands using go_proxy_tree node_id fcode.
    endif.
    if go_idoc_tree is not initial.
      perform data_display_tree_commands using go_idoc_tree node_id fcode.
    endif.
  endmethod.                    "node_menu_select
endclass.                    "lcl_tree_events_handle IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS_IDOC_COPYPROC
*&---------------------------------------------------------------------*
form data_process_idoc_copyproc  using lk_intfms type gtk_intfms.
  data: lk_idoc_control          type edidc.
  data: li_idoc_control          type standard table of edidc.
  data: li_idoc_edid4            type standard table of edid4.
  data: li_idoc_data             type standard table of edidd.
  data: lk_idoc_process_data_in  type tede2.
  data: lk_intfms_new            type gtk_intfms.
  data: lv_idoc_subrc            type sysubrc.
  field-symbols: <lk_idoc_edid4> type edid4.
  field-symbols: <lk_idoc_data>  type edidd.

  "Create IDoc Control Record
  select single * into lk_idoc_control  from edidc where docnum = lk_intfms-idoc_docnum.
  clear lk_idoc_control-docnum.

  "Create IDoc Data Records
  select * into table li_idoc_edid4 from edid4 where docnum = lk_intfms-idoc_docnum..
  loop at li_idoc_edid4 assigning <lk_idoc_edid4>.
    append initial line to li_idoc_data assigning <lk_idoc_data>.
    <lk_idoc_data>-segnum = <lk_idoc_edid4>-segnum.
    <lk_idoc_data>-segnam = <lk_idoc_edid4>-segnam.
    <lk_idoc_data>-psgnum = <lk_idoc_edid4>-psgnum.
    <lk_idoc_data>-hlevel = <lk_idoc_edid4>-hlevel.
    <lk_idoc_data>-dtint2 = <lk_idoc_edid4>-dtint2.
    <lk_idoc_data>-sdata  = <lk_idoc_edid4>-sdata.
  endloop.

  case lk_idoc_control-direct.
    when '1'.
      call function 'IDOC_OUTBOUND_WRITE_TO_DB'
        tables
          int_edidd      = li_idoc_data
        changing
          int_edidc      = lk_idoc_control
        exceptions
          idoc_not_saved = 1
          others         = 2.

      append lk_idoc_control to li_idoc_control.
      clear li_idoc_data[].

      call function 'EDI_OUTPUT_NEW'
        exporting
          onl_option = 'B'
          error_flag = ' '
        tables
          i_edidc    = li_idoc_control
          i_edidd    = li_idoc_data
        exceptions
          others     = 0.
    when '2'.
      call function 'IDOC_INBOUND_WRITE_TO_DB'
        exporting
          pi_return_data_flag     = ' '
        importing
          pe_idoc_number          = lk_idoc_control-docnum
          pe_state_of_processing  = lv_idoc_subrc
          pe_inbound_process_data = lk_idoc_process_data_in
        tables
          t_data_records          = li_idoc_data
        changing
          pc_control_record       = lk_idoc_control
        exceptions
          idoc_not_saved          = 1
          others                  = 2.
      if lv_idoc_subrc = 0.
        append lk_idoc_control to li_idoc_control.
        call function 'IDOC_START_INBOUND'
          exporting
            pi_inbound_process_data = lk_idoc_process_data_in
            pi_called_online        = 'X'
            succ_show_flag          = 'X'
          tables
            t_control_records       = li_idoc_control
          exceptions
            others                  = 1.
      endif.
  endcase.

  if lk_intfms-interface_dir = 'O'.
    lk_intfms_new = lk_intfms.
    lk_intfms_new-interface_msgid = lk_idoc_control-docnum.
    lk_intfms_new-execute_date = sy-datum.
    lk_intfms_new-execute_time = sy-uzeit.
    modify /pstech/bcintfms from lk_intfms_new.
  endif.
endform.                    " DATA_PROCESS_IDOC_COPYPROC
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS_IDOC_EDITTREE
*&---------------------------------------------------------------------*
form data_process_idoc_edittree.
  data li_idoc_tree_nodes      type uac_t_node_long.
  data lk_idoc_control         type edidc.
  data li_idoc_control         type standard table of edidc.
  data li_idoc_data            type standard table of edidd.
  data lk_idoc_process_data_in type tede2.
  data lv_idoc_subrc           type sysubrc.
  data lk_idoc_data            type edidd.
  data lv_idoc_tree_value      type string.
  data lk_intfms_new           type gtk_intfms.
  data lo_data_struc           type ref to data.
  field-symbols: <lk_idoc_tree_nodes> type uac_s_node_long.
  field-symbols: <lv_data_field>      type any,
                 <lv_data_struc>      type any,
                 <lv_tree_value>      type any.

  li_idoc_tree_nodes = go_idoc_tree->get_nodes( ).

  loop at li_idoc_tree_nodes assigning <lk_idoc_tree_nodes>.
    clear: lv_idoc_tree_value.
    case <lk_idoc_tree_nodes>-parent_node(5).
      when 'EDIDC'.
        assign component <lk_idoc_tree_nodes>-value of structure lk_idoc_control to <lv_data_field>.
        go_idoc_tree->get_cell( exporting id_node_id = <lk_idoc_tree_nodes>-node_id id_column_id = '1' importing ed_value = lv_idoc_tree_value ).
        <lv_data_field> = lv_idoc_tree_value.
      when 'EDIDD'.
        if <lk_idoc_tree_nodes>-node_id(5) = 'EDIDD'.
          if lo_data_struc is not initial.
            lk_idoc_data-sdata = <lv_data_struc>.
            append lk_idoc_data to li_idoc_data.
            clear: lo_data_struc.
            unassign: <lv_data_struc>.
          endif.
          lk_idoc_data-segnam = <lk_idoc_tree_nodes>-value.
          create data lo_data_struc type (<lk_idoc_tree_nodes>-value).
          assign lo_data_struc->* to  <lv_data_struc>.
        else.
          assign component <lk_idoc_tree_nodes>-value of structure <lv_data_struc> to <lv_data_field>.
          go_idoc_tree->get_cell( exporting id_node_id = <lk_idoc_tree_nodes>-node_id id_column_id = '1' importing ed_value = lv_idoc_tree_value ).
          <lv_data_field> = lv_idoc_tree_value.
        endif.
    endcase.
  endloop.

  "Append last node
  lk_idoc_data-sdata = <lv_data_struc>.
  append lk_idoc_data to li_idoc_data.
  clear: lo_data_struc.
  unassign: <lv_data_struc>.

  "Remove Previous DocNum
  clear lk_idoc_control-docnum.

  case lk_idoc_control-direct.
    when '1'.
      call function 'IDOC_OUTBOUND_WRITE_TO_DB'
        tables
          int_edidd      = li_idoc_data
        changing
          int_edidc      = lk_idoc_control
        exceptions
          idoc_not_saved = 1
          others         = 2.
      append lk_idoc_control to li_idoc_control.
      clear li_idoc_data[].

      call function 'EDI_OUTPUT_NEW'
        exporting
          onl_option = 'B'
          error_flag = ' '
        tables
          i_edidc    = li_idoc_control
          i_edidd    = li_idoc_data
        exceptions
          others     = 0.
    when '2'.
      call function 'IDOC_INBOUND_WRITE_TO_DB'
        exporting
          pi_return_data_flag     = ' '
        importing
          pe_idoc_number          = lk_idoc_control-docnum
          pe_state_of_processing  = lv_idoc_subrc
          pe_inbound_process_data = lk_idoc_process_data_in
        tables
          t_data_records          = li_idoc_data
        changing
          pc_control_record       = lk_idoc_control
        exceptions
          idoc_not_saved          = 1
          others                  = 2.
      if lv_idoc_subrc = 0.
        append lk_idoc_control to li_idoc_control.
        call function 'IDOC_START_INBOUND'
          exporting
            pi_inbound_process_data = lk_idoc_process_data_in
            pi_called_online        = 'X'
            succ_show_flag          = 'X'
          tables
            t_control_records       = li_idoc_control
          exceptions
            others                  = 1.
      endif.
  endcase.

  if gk_intfms-interface_dir = 'O'.
    lk_intfms_new = gk_intfms.
    lk_intfms_new-interface_msgid = lk_idoc_control-docnum.
    lk_intfms_new-execute_date = sy-datum.
    lk_intfms_new-execute_time = sy-uzeit.
    modify /pstech/bcintfms from lk_intfms_new.
  endif.

endform.                    " DATA_PROCESS_IDOC_EDITPROC
*&---------------------------------------------------------------------*
*&      Form  DATA_PROCESS_IDOC_RESTART
*&---------------------------------------------------------------------*
form data_process_idoc_restart using li_intfms type gti_intfms.
  data li_idoc_docnum  type bdrg_doc_tab.
  data li_idoc_control type standard table of edidc.
  field-symbols: <lk_idoc_control> type edidc.
  field-symbols: <lk_idoc_docnum>  type bdrg_doc.

  if li_intfms[] is not initial.
    select * into table li_idoc_control from edidc for all entries in li_intfms where docnum = li_intfms-idoc_docnum.
  endif.

  "Status 30
  clear: li_idoc_docnum[].
  loop at li_idoc_control assigning <lk_idoc_control> where status = '30'.
    append initial line to li_idoc_docnum assigning <lk_idoc_docnum>.
    <lk_idoc_docnum>-sign = 'I'.
    <lk_idoc_docnum>-option = 'EQ'.
    <lk_idoc_docnum>-low = <lk_idoc_control>-docnum.
  endloop.
  if li_idoc_docnum[] is not initial.
    submit rseout00 with docnum in li_idoc_docnum and return.
  endif.

  " Status 02 04 05 25 29
  clear: li_idoc_docnum[].
  loop at li_idoc_control assigning <lk_idoc_control> where status = '02' or status = '04' or status = '05' or status = '25' or status = '29'.
    append initial line to li_idoc_docnum assigning <lk_idoc_docnum>.
    <lk_idoc_docnum>-sign = 'I'.
    <lk_idoc_docnum>-option = 'EQ'.
    <lk_idoc_docnum>-low = <lk_idoc_control>-docnum.
  endloop.
  if li_idoc_docnum[] is not initial.
    submit rbdagain with so_docnu in li_idoc_docnum with p_output = ' ' and return.
  endif.

  " Status 26
  clear: li_idoc_docnum[].
  loop at li_idoc_control assigning <lk_idoc_control>  where status = '26'.
    append initial line to li_idoc_docnum assigning <lk_idoc_docnum>.
    <lk_idoc_docnum>-sign = 'I'.
    <lk_idoc_docnum>-option = 'EQ'.
    <lk_idoc_docnum>-low = <lk_idoc_control>-docnum.
  endloop.
  if li_idoc_docnum[] is not initial.
    submit rbdsyneo with so_docnu in li_idoc_docnum with p_output = ' '  and return.
  endif.

  " Status 32
  clear: li_idoc_docnum[].
  loop at li_idoc_control assigning <lk_idoc_control> where status = '32'.
    append initial line to li_idoc_docnum assigning <lk_idoc_docnum>.
    <lk_idoc_docnum>-sign = 'I'.
    <lk_idoc_docnum>-option = 'EQ'.
    <lk_idoc_docnum>-low = <lk_idoc_control>-docnum.
  endloop.
  if li_idoc_docnum[] is not initial.
    submit rbdagaie with p_idoc in li_idoc_docnum with p_direct = '1' with p_output = ' ' and return.
  endif.

  " Status 51
  clear: li_idoc_docnum[].
  loop at li_idoc_control assigning <lk_idoc_control>  where status = '51'.
    append initial line to li_idoc_docnum assigning <lk_idoc_docnum>.
    <lk_idoc_docnum>-sign = 'I'.
    <lk_idoc_docnum>-option = 'EQ'.
    <lk_idoc_docnum>-low = <lk_idoc_control>-docnum.
  endloop.
  if li_idoc_docnum[] is not initial.
    submit rbdmani2 with so_docnu in li_idoc_docnum with p_output = ' ' and return.
  endif.

  " Status 56 61 63 65
  clear: li_idoc_docnum[].
  loop at li_idoc_control assigning <lk_idoc_control> where status = '56' or status = '61' or status = '63' or status = '65'.
    append initial line to li_idoc_docnum assigning <lk_idoc_docnum>.
    <lk_idoc_docnum>-sign = 'I'.
    <lk_idoc_docnum>-option = 'EQ'.
    <lk_idoc_docnum>-low = <lk_idoc_control>-docnum.
  endloop.
  if li_idoc_docnum[] is not initial.
    submit rbdagai2 with so_docnu in li_idoc_docnum with p_output = ' ' and return.
  endif.

  " Status 60
  clear: li_idoc_docnum[].
  loop at li_idoc_control assigning <lk_idoc_control> where status = '60'.
    append initial line to li_idoc_docnum assigning <lk_idoc_docnum>.
    <lk_idoc_docnum>-sign = 'I'.
    <lk_idoc_docnum>-option = 'EQ'.
    <lk_idoc_docnum>-low = <lk_idoc_control>-docnum.
  endloop.
  if li_idoc_docnum[] is not initial.
    submit rbdsynei with so_docnu in li_idoc_docnum with p_output = ' ' and return.
  endif.

  " Status 64 66
  clear: li_idoc_docnum[].
  loop at li_idoc_control assigning <lk_idoc_control> where status = '64' or status = '66'.
    append initial line to li_idoc_docnum assigning <lk_idoc_docnum>.
    <lk_idoc_docnum>-sign = 'I'.
    <lk_idoc_docnum>-option = 'EQ'.
    <lk_idoc_docnum>-low = <lk_idoc_control>-docnum.
  endloop.
  if li_idoc_docnum[] is not initial.
    submit rbdapp01 with docnum in li_idoc_docnum with p_output = ' ' and return.
  endif.

  " Status 69
  clear: li_idoc_docnum[].
  loop at li_idoc_control assigning <lk_idoc_control> where status = '69'.
    append initial line to li_idoc_docnum assigning <lk_idoc_docnum>.
    <lk_idoc_docnum>-sign = 'I'.
    <lk_idoc_docnum>-option = 'EQ'.
    <lk_idoc_docnum>-low = <lk_idoc_control>-docnum.
  endloop.
  if li_idoc_docnum[] is not initial.
    submit rbdagaie  with p_idoc in li_idoc_docnum with p_direct = '2' with p_output = ' ' and return.
  endif.
endform.                    "DATA_PROCESS_IDOC_RESTART
