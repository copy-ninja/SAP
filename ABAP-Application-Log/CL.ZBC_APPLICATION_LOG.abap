*----------------------------------------------------------------------*
*       CLASS ZBC_APPLICATION_LOG DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ZBC_APPLICATION_LOG definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !OBJECT type BALOBJ_D
      !SUBOBJECT type BALSUBOBJ
      !EXTERNAL_ID type ANY optional
      !LOG_NUMBER type BALOGNR optional
      !LOG_DATE type BALDATE default SY-DATUM
      !LOG_TIME type BALTIME default SY-UZEIT
      !LOG_USER type BALUSER default SY-UNAME
      !LOG_DAYS type I default 30
    exceptions
      OBJECT_NOT_FOUND .
  class-methods GET_INSTANCE
    importing
      !OBJECT type BALOBJ_D
      !SUBOBJECT type BALSUBOBJ
      !EXTERNAL_ID type ANY optional
      !LOG_NUMBER type BALOGNR optional
      !LOG_DATE type BALDATE default SY-DATUM
      !LOG_TIME type BALTIME default SY-UZEIT
      !LOG_USER type BALUSER default SY-UNAME
      !LOG_DAYS type I default 30
    returning
      value(APPLICATION_LOG) type ref to ZBC_APPLICATION_LOG
    exceptions
      OBJECT_NOT_FOUND .
  methods ADD_MESSAGE
    importing
      !MSGTY type SYMSGTY default 'E'
      !MSGID type SYMSGID
      !MSGNO type SYMSGNO optional
      !MSGV1 type ANY optional
      !MSGV2 type ANY optional
      !MSGV3 type ANY optional
      !MSGV4 type ANY optional
      !PROBCLASS type BALPROBCL optional .
  methods ADD_MESSAGE_TEXT
    importing
      !MSGTY type SYMSGTY default 'E'
      !MSG type CLIKE
      !PROBCLASS type BALPROBCL optional .
  methods ADD_MESSAGE_STRUC
    importing
      !MSGTY type SYMSGTY default 'E'
      !STRUC type ANY
      !PROBCLASS type BALPROBCL optional .
  methods ADD_EXCEPTION
    importing
      !EXCEPTION type ref to CX_ROOT .
  methods SAVE
    importing
      !SAVE_EMPTY_LOG type BOOLE_D default SPACE
    returning
      value(LOG_NUMBER) type BALOGNR .
  methods DISPLAY
    importing
      !LOG_NUMBERS type BAL_T_LOGN optional .
  PROTECTED SECTION.
private section.

  types:
    BAL_T_EXC TYPE STANDARD TABLE OF bal_s_exc .

  data LOG_HEADER type BAL_S_LOG .
  data LOG_MESSAGES type BAL_T_MSG .
  data LOG_EXCEPTIONS type BAL_T_EXC .
  data LOG_HANDLE type BALLOGHNDL .
ENDCLASS.



CLASS ZBC_APPLICATION_LOG IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBC_APPLICATION_LOG->ADD_EXCEPTION
* +-------------------------------------------------------------------------------------------------+
* | [--->] EXCEPTION                      TYPE REF TO CX_ROOT
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD add_exception.
  DATA: lk_exception TYPE bal_s_exc.
  DATA: lk_message   TYPE bal_s_msg.
  DATA: lo_exception TYPE REF TO cx_root.
  DATA: lv_prog_name TYPE syrepid,
        lv_prog_incl TYPE syrepid.
  DATA: lv_prog_line TYPE i.
  DATA: lv_message   TYPE string.

  lo_exception = exception.
  WHILE lo_exception IS NOT INITIAL.
    CLEAR: lk_exception, lk_message, lv_message.
    lk_exception-msgty = 'E'.
    lk_exception-exception = lo_exception.
    lk_exception-detlevel  = '1'.
    lk_exception-probclass = '1'.
    GET TIME STAMP FIELD lk_exception-time_stmp.
    APPEND lk_exception TO log_exceptions.

    "Add message of where exception occured
    lo_exception->get_source_position( IMPORTING program_name = lv_prog_name include_name = lv_prog_incl source_line  = lv_prog_line ).
    lv_message = |Exception occurred in program | && lv_prog_name && |, | && |include | && lv_prog_incl && |, | && |line | && lv_prog_line.
    add_message_text( msg = lv_message ).

    "Add message of exception
    CLEAR lv_message.
    lv_message = lo_exception->get_text( ).
    IF lv_message IS NOT INITIAL.
      add_message_text( msg = lv_message ).
    ENDIF.

    lo_exception = lo_exception->previous.
  ENDWHILE.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBC_APPLICATION_LOG->ADD_MESSAGE
* +-------------------------------------------------------------------------------------------------+
* | [--->] MSGTY                          TYPE        SYMSGTY (default ='E')
* | [--->] MSGID                          TYPE        SYMSGID
* | [--->] MSGNO                          TYPE        SYMSGNO(optional)
* | [--->] MSGV1                          TYPE        ANY(optional)
* | [--->] MSGV2                          TYPE        ANY(optional)
* | [--->] MSGV3                          TYPE        ANY(optional)
* | [--->] MSGV4                          TYPE        ANY(optional)
* | [--->] PROBCLASS                      TYPE        BALPROBCL(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD add_message.
  DATA:  lk_message         TYPE bal_s_msg.
  lk_message-msgty     = msgty.
  lk_message-msgno     = msgno.
  lk_message-msgid     = msgid.
  lk_message-msgv1     = msgv1.
  lk_message-msgv2     = msgv2.
  lk_message-msgv3     = msgv3.
  lk_message-msgv4     = msgv4.

  IF probclass IS SUPPLIED.
    lk_message-probclass = probclass.
  ELSE.
    CASE msgty.
      WHEN 'A' OR 'E'.
        lk_message-probclass = '1'.
      WHEN 'W'.
        lk_message-probclass = '3'.
      WHEN OTHERS.
        lk_message-probclass = '4'.
    ENDCASE.
  ENDIF.

  APPEND lk_message TO log_messages.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBC_APPLICATION_LOG->ADD_MESSAGE_STRUC
* +-------------------------------------------------------------------------------------------------+
* | [--->] MSGTY                          TYPE        SYMSGTY (default ='E')
* | [--->] STRUC                          TYPE        ANY
* | [--->] PROBCLASS                      TYPE        BALPROBCL(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD add_message_struc.
  DATA: lk_message     TYPE bal_s_msg.
  DATA: lo_data_type   TYPE REF TO cl_abap_typedescr.
  DATA: lo_data        TYPE REF TO data.
  DATA: lk_data_header TYPE x030l.
  DATA: li_data_fields TYPE dd_x031l_table.
  FIELD-SYMBOLS : <lk_data_fields>    TYPE x031l.
  FIELD-SYMBOLS : <lk_data>           TYPE any,
                  <lk_data_component> TYPE any.

  lo_data_type   = cl_abap_datadescr=>describe_by_data( struc ).
  lk_data_header = lo_data_type->get_ddic_header( ).
  li_data_fields = lo_data_type->get_ddic_object( ).
  CREATE DATA lo_data TYPE (lk_data_header-tabname).
  ASSIGN lo_data->* TO <lk_data>.
  <lk_data> = struc.

  "Move whatever that is similar in name
  MOVE-CORRESPONDING struc TO lk_message.
  "for BAPI returns (Badly Associated Progamming Interface) Freaking BAPIs...
  LOOP AT li_data_fields ASSIGNING <lk_data_fields>.
    CASE <lk_data_fields>-rollname.
      WHEN 'SYMSGID' OR 'ARBGB' OR 'MSGID' OR 'BAPI_MSGID' OR 'BALMSGID' OR 'BDC_MID'.
        ASSIGN COMPONENT <lk_data_fields>-fieldname OF STRUCTURE <lk_data> TO <lk_data_component>.
        IF sy-subrc = 0.  lk_message-msgid = <lk_data_component>. ENDIF.
      WHEN 'SYMSGTY' OR 'BAPI_MTYPE' OR 'ERRORTYP' OR 'MSGTY_CO' OR 'BALMSGTY' OR 'MSGTY' OR 'EDI_SYMSTY' OR 'BDC_MART'.
        ASSIGN COMPONENT <lk_data_fields>-fieldname OF STRUCTURE <lk_data> TO <lk_data_component>.
        IF sy-subrc = 0.  lk_message-msgty = <lk_data_component>. ENDIF.
      WHEN 'SYMSGNO' OR 'MSGNR' OR 'MSGNO' OR 'SEVERITY' OR 'BALMSGNO' OR 'BDC_MNR' .
        ASSIGN COMPONENT <lk_data_fields>-fieldname OF STRUCTURE <lk_data> TO <lk_data_component>.
        IF sy-subrc = 0.  lk_message-msgno = <lk_data_component>. ENDIF.
      WHEN 'SYMSGV' OR 'BDC_VTEXT1'.
        ASSIGN COMPONENT <lk_data_fields>-fieldname OF STRUCTURE <lk_data> TO <lk_data_component>.
        IF sy-subrc = 0.
          IF <lk_data_fields>-fieldname CA '1'. lk_message-msgv1 = <lk_data_component>. ENDIF.
          IF <lk_data_fields>-fieldname CA '2'. lk_message-msgv2 = <lk_data_component>. ENDIF.
          IF <lk_data_fields>-fieldname CA '3'. lk_message-msgv3 = <lk_data_component>. ENDIF.
          IF <lk_data_fields>-fieldname CA '4'. lk_message-msgv4 = <lk_data_component>. ENDIF.
        ENDIF.
      WHEN 'MSGV1' OR 'BALMSGV1' OR 'EDI_STAPA1'.
        ASSIGN COMPONENT <lk_data_fields>-fieldname OF STRUCTURE <lk_data> TO <lk_data_component>.
        IF sy-subrc = 0.  lk_message-msgv1 = <lk_data_component>. ENDIF.
      WHEN 'MSGV2' OR 'BALMSGV2' OR 'EDI_STAPA2'.
        ASSIGN COMPONENT <lk_data_fields>-fieldname OF STRUCTURE <lk_data> TO <lk_data_component>.
        IF sy-subrc = 0.  lk_message-msgv2 = <lk_data_component>. ENDIF.
      WHEN 'MSGV3' OR 'BALMSGV3' OR 'EDI_STAPA3'.
        ASSIGN COMPONENT <lk_data_fields>-fieldname OF STRUCTURE <lk_data> TO <lk_data_component>.
        IF sy-subrc = 0.  lk_message-msgv3 = <lk_data_component>. ENDIF.
      WHEN 'MSGV4' OR 'BALMSGV4' OR 'EDI_STAPA4'.
        ASSIGN COMPONENT <lk_data_fields>-fieldname OF STRUCTURE <lk_data> TO <lk_data_component>.
        IF sy-subrc = 0.  lk_message-msgv4 = <lk_data_component>. ENDIF.
    ENDCASE.
  ENDLOOP.

  IF probclass IS SUPPLIED.
    lk_message-probclass = probclass.
  ELSE.
    CASE msgty.
      WHEN 'A' OR 'E'.
        lk_message-probclass = '1'.
      WHEN 'W'.
        lk_message-probclass = '3'.
      WHEN OTHERS.
        lk_message-probclass = '4'.
    ENDCASE.
  ENDIF.

  APPEND lk_message TO log_messages.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBC_APPLICATION_LOG->ADD_MESSAGE_TEXT
* +-------------------------------------------------------------------------------------------------+
* | [--->] MSGTY                          TYPE        SYMSGTY (default ='E')
* | [--->] MSG                            TYPE        CLIKE
* | [--->] PROBCLASS                      TYPE        BALPROBCL(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD add_message_text.
  TYPES: BEGIN OF ltk_string,
           part1   TYPE symsgv,
           part2   TYPE symsgv,
           part3   TYPE symsgv,
           part4   TYPE symsgv,
         END OF ltk_string.
  DATA: lk_message  TYPE bal_s_msg.
  DATA: lk_string  TYPE ltk_string.

  lk_string = msg.
  lk_message-msgty     = msgty.
  lk_message-msgno     = 001.
  lk_message-msgid     = 'BL'.
  lk_message-msgv1     = lk_string-part1.
  lk_message-msgv2     = lk_string-part2.
  lk_message-msgv3     = lk_string-part3.
  lk_message-msgv4     = lk_string-part4.

  IF probclass IS SUPPLIED.
    lk_message-probclass = probclass.
  ELSE.
    CASE msgty.
      WHEN 'A' OR 'E'.
        lk_message-probclass = '1'.
      WHEN 'W'.
        lk_message-probclass = '3'.
      WHEN OTHERS.
        lk_message-probclass = '4'.
    ENDCASE.
  ENDIF.

  APPEND lk_message TO log_messages.
ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBC_APPLICATION_LOG->CONSTRUCTOR
* +-------------------------------------------------------------------------------------------------+
* | [--->] OBJECT                         TYPE        BALOBJ_D
* | [--->] SUBOBJECT                      TYPE        BALSUBOBJ
* | [--->] EXTERNAL_ID                    TYPE        ANY(optional)
* | [--->] LOG_NUMBER                     TYPE        BALOGNR(optional)
* | [--->] LOG_DATE                       TYPE        BALDATE (default =SY-DATUM)
* | [--->] LOG_TIME                       TYPE        BALTIME (default =SY-UZEIT)
* | [--->] LOG_USER                       TYPE        BALUSER (default =SY-UNAME)
* | [--->] LOG_DAYS                       TYPE        I (default =30)
* | [EXC!] OBJECT_NOT_FOUND
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD constructor.
    DATA: lk_log_obj TYPE balobj.
    DATA: lk_log_sub TYPE balsub.
    DATA: lk_log_dbheader              TYPE balhdr.

    "Look for Object
    SELECT SINGLE * FROM balobj INTO lk_log_obj WHERE object EQ object.
    IF sy-subrc NE 0.
      MESSAGE e010(bl) WITH object RAISING object_not_found.
    ENDIF.

    "Look for Sub-object
    SELECT SINGLE * FROM balsub INTO lk_log_sub WHERE object EQ object AND subobject  EQ subobject.
    IF sy-subrc <> 0.
      MESSAGE e011(bl) WITH object subobject RAISING object_not_found.
    ENDIF.

    IF log_number IS NOT INITIAL.
      "Retrieve log handle from log number
      SELECT SINGLE * INTO lk_log_dbheader FROM balhdr WHERE lognumber EQ log_number AND object EQ object AND subobject EQ subobject.
      IF sy-subrc = 0.
        log_handle = lk_log_dbheader-log_handle.
      ENDIF.
    ENDIF.

    "If no previous log handle, then create a new one.
    IF log_handle IS INITIAL.
      log_header-extnumber       = external_id.
      log_header-object          = object.
      log_header-subobject       = subobject.
      log_header-alprog          = sy-cprog.
      log_header-aldate          = log_date.
      log_header-altime          = log_time.
      log_header-aluser          = log_user.
      IF log_days > 0.
        log_header-aldate_del = sy-datum + log_days.
      ELSE.
        log_header-aldate_del = '99991231'.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "constructor


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBC_APPLICATION_LOG->DISPLAY
* +-------------------------------------------------------------------------------------------------+
* | [--->] LOG_NUMBERS                    TYPE        BAL_T_LOGN(optional)
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD display.
  DATA: lk_log_filter TYPE bal_s_lfil.
  DATA: lk_log_lognum TYPE bal_s_logn.
  DATA: lk_log_object TYPE bal_s_obj.
  DATA: lk_log_subobject TYPE bal_s_sub.
  FIELD-SYMBOLS: <lk_log_number> TYPE balognr.

  lk_log_object-sign = lk_log_subobject-sign = 'I'.
  lk_log_object-option = lk_log_subobject-option = 'EQ'.
  lk_log_object-low = log_header-object.
  lk_log_subobject-low = log_header-subobject.
  APPEND lk_log_object TO lk_log_filter-object.
  APPEND lk_log_subobject TO lk_log_filter-subobject.

  LOOP AT log_numbers ASSIGNING <lk_log_number>.
    lk_log_lognum-sign = 'I'.
    lk_log_lognum-option = 'EQ'.
    lk_log_lognum-low = <lk_log_number>.
    APPEND lk_log_lognum TO lk_log_filter-lognumber.
    CLEAR lk_log_lognum.
  ENDLOOP.

  CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
    EXPORTING
      i_s_log_filter       = lk_log_filter
    EXCEPTIONS
      profile_inconsistent = 1
      internal_error       = 2
      no_data_available    = 3
      no_authority         = 4
      OTHERS               = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Static Public Method ZBC_APPLICATION_LOG=>GET_INSTANCE
* +-------------------------------------------------------------------------------------------------+
* | [--->] OBJECT                         TYPE        BALOBJ_D
* | [--->] SUBOBJECT                      TYPE        BALSUBOBJ
* | [--->] EXTERNAL_ID                    TYPE        ANY(optional)
* | [--->] LOG_NUMBER                     TYPE        BALOGNR(optional)
* | [--->] LOG_DATE                       TYPE        BALDATE (default =SY-DATUM)
* | [--->] LOG_TIME                       TYPE        BALTIME (default =SY-UZEIT)
* | [--->] LOG_USER                       TYPE        BALUSER (default =SY-UNAME)
* | [--->] LOG_DAYS                       TYPE        I (default =30)
* | [<-()] APPLICATION_LOG                TYPE REF TO ZBC_APPLICATION_LOG
* | [EXC!] OBJECT_NOT_FOUND
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_instance.
    CREATE OBJECT application_log
      EXPORTING
        object           = object
        subobject        = subobject
        external_id      = external_id
        log_number       = log_number
        log_date         = log_date
        log_time         = log_time
        log_user         = log_user
        log_days         = log_days
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.
    CASE sy-subrc .
      WHEN 1.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING object_not_found.
      WHEN 2.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDCASE.
  ENDMETHOD.                    "GET_INSTANCE


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZBC_APPLICATION_LOG->SAVE
* +-------------------------------------------------------------------------------------------------+
* | [--->] SAVE_EMPTY_LOG                 TYPE        BOOLE_D (default =SPACE)
* | [<-()] LOG_NUMBER                     TYPE        BALOGNR
* +--------------------------------------------------------------------------------------</SIGNATURE>
METHOD save.
  DATA: li_log_handle                TYPE bal_t_logh.
  DATA: li_log_number                TYPE bal_t_lgnm.
  DATA: lk_log_dbheader              TYPE balhdr.
  DATA: li_log_dbheader              TYPE STANDARD TABLE OF balhdr.
  FIELD-SYMBOLS <lk_log_number>      TYPE bal_s_lgnm.
  FIELD-SYMBOLS <lk_msg> TYPE bal_s_msg.
  FIELD-SYMBOLS <lk_exc> TYPE bal_s_exc.

  "Create/retrieve log
  IF log_handle IS INITIAL.
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = log_header
      IMPORTING
        e_log_handle            = log_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ELSE.
    CALL FUNCTION 'BAL_LOG_EXIST'
      EXPORTING
        i_log_handle  = log_handle
      EXCEPTIONS
        log_not_found = 1.
    IF sy-subrc NE 0.
      "Load up the log.
      APPEND lk_log_dbheader TO li_log_dbheader.
      CALL FUNCTION 'BAL_DB_LOAD'
        EXPORTING
          i_t_log_header = li_log_dbheader
        EXCEPTIONS
          OTHERS         = 0.
    ENDIF.

    "Retrieve log header
    CALL FUNCTION 'BAL_LOG_HDR_READ'
      EXPORTING
        i_log_handle  = lk_log_dbheader-log_handle
      IMPORTING
        e_s_log       = log_header
      EXCEPTIONS
        log_not_found = 1
        OTHERS        = 2.
  ENDIF.

  "Add exceptions to log.
  LOOP AT log_exceptions ASSIGNING <lk_exc>.
    CALL FUNCTION 'BAL_LOG_EXCEPTION_ADD'
      EXPORTING
        i_log_handle     = log_handle
        i_s_exc          = <lk_exc>
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDLOOP.

  "Add messages to log.
  LOOP AT log_messages ASSIGNING <lk_msg>.
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle     = log_handle
        i_s_msg          = <lk_msg>
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDLOOP.

  "Check if to proceed with empty log
  IF log_messages[] IS INITIAL.
    CHECK save_empty_log IS NOT INITIAL.
  ENDIF.

  "Save log
  APPEND log_handle TO li_log_handle.
  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_t_log_handle   = li_log_handle
    IMPORTING
      e_new_lognumbers = li_log_number
    EXCEPTIONS
      log_not_found    = 1
      save_not_allowed = 2
      numbering_error  = 3
      OTHERS           = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    READ TABLE li_log_number ASSIGNING <lk_log_number> INDEX 1.
    IF sy-subrc = 0.
      log_number = <lk_log_number>-lognumber.
    ENDIF.
  ENDIF.


ENDMETHOD.
ENDCLASS.
