**********************************************************************
* Description   : BC - Table Date Change Audit
* Developer     : Jason Mok
******************** VERSION CONTROL *********************************
* Date     | User ID  | Transport# | Request#
**********************************************************************
* 04/07/11 | 00438745 | DE2K900172 |
* - New development
**********************************************************************
* This is an include to set the change audit data for AEDAT AENAM ERDAT ERNAM
*&---------------------------------------------------------------------*
*&      Form  set_changeaudit
*&---------------------------------------------------------------------*
form set_changeauditdate2.
  data lv_objname type string.
  data li_total_comp   type cl_abap_structdescr=>component_table.
  data li_flag_comp    type cl_abap_structdescr=>component_table.
  data lo_total_struct type ref to cl_abap_structdescr.
  data lo_total_dref   type ref to data.
  data lo_typ_descr    type ref to cl_abap_typedescr.
  data lo_str_descr    type ref to cl_abap_structdescr.
  field-symbols <lk_total_struct> type any.
  field-symbols <lk_total>        type c.
  field-symbols <lv_ernam>        type ernam.
  field-symbols <lv_erdat>        type erdat.
  field-symbols <lv_erzet>        type erzet.
  field-symbols <lv_aenam>        type aenam.
  field-symbols <lv_aedat>        type aedat.
  field-symbols <lv_aezet>        type aezet.
  field-symbols <lv_vim_action>   type xfeld.

* Retrieve table structures
  concatenate '\TYPE=' view_name into lv_objname.
  call method cl_abap_typedescr=>describe_by_name
    exporting
      p_name      = lv_objname
    receiving
      p_descr_ref = lo_typ_descr.
  lo_str_descr ?= lo_typ_descr.
  li_total_comp = lo_str_descr->get_components( ).

* Retrieve flag structures
  call method cl_abap_typedescr=>describe_by_name
    exporting
      p_name      = '\TYPE=VIMTBFLAGS'
    receiving
      p_descr_ref = lo_typ_descr.
  lo_str_descr ?= lo_typ_descr.
  li_flag_comp = lo_str_descr->get_components( ).

* Combine both structural components
  append lines of li_flag_comp to li_total_comp.

  lo_total_struct = cl_abap_structdescr=>create( li_total_comp ).

* Create the combined structure and assign created by, changed by, change date, create date.
  create data lo_total_dref type handle lo_total_struct.
  assign lo_total_dref->* to <lk_total_struct>.

* update created by, changed by, change date, create date whenever it's new or changed.
  loop at total assigning <lk_total> casting.
    assign <lk_total> to <lk_total_struct> casting type handle lo_total_struct.
      assign component 'ERNAM' of structure <lk_total_struct> to <lv_ernam>.
      assign component 'ERDAT' of structure <lk_total_struct> to <lv_erdat>.
      assign component 'ERZET' of structure <lk_total_struct> to <lv_erzet>.
      assign component 'AENAM' of structure <lk_total_struct> to <lv_aenam>.
      assign component 'AEDAT' of structure <lk_total_struct> to <lv_aedat>.
      assign component 'AEZET' of structure <lk_total_struct> to <lv_aezet>.
      assign component 'VIM_ACTION' of structure <lk_total_struct> to <lv_vim_action>.
    case <lv_vim_action>.
      when neuer_eintrag.    "New (N)
        if <lv_ernam> is assigned.
        <lv_ernam> = cl_abap_syst=>get_user_name( ).
        endif.

        if <lv_aenam> is assigned.
        <lv_aenam> = cl_abap_syst=>get_user_name( ).
        endif.

        if <lv_erdat> is assigned.
        <lv_erdat> = sy-datum.
        endif.

        if  <lv_aedat> is assigned.
        <lv_aedat> = sy-datum.
        endif.

        if <lv_erzet> is assigned.
        <lv_erzet> = sy-uzeit.
        endif.

        if <lv_aezet> is assigned.
        <lv_aezet> = sy-uzeit.
        endif.

        assign <lk_total_struct> to <lk_total> casting.
      when aendern.          "Updated (U)
        if <lv_aenam> is assigned.
        <lv_aenam> = cl_abap_syst=>get_user_name( ).
        endif.

        if  <lv_aedat> is assigned.
        <lv_aedat> = sy-datum.
        endif.

        if <lv_aezet> is assigned.
        <lv_aezet> = sy-uzeit.
        endif.

        assign <lk_total_struct> to <lk_total> casting.
    endcase.

    unassign : <lv_ernam>, <lv_erdat>, <lv_erzet>, <lv_aenam>, <lv_aedat>, <lv_aezet>, <lv_vim_action>.
  endloop.

endform.                    "set_changeauditdate
