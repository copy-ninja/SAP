PROGRAM.
TABLES sscrfields.

CONSTANTS: c_form_name TYPE string VALUE 'JASON_MOK_ROCKS'.
CONSTANTS: BEGIN OF ck_data_name,
            f01     TYPE c LENGTH 30 VALUE 'DATA_FIELD01',
            jobname TYPE c LENGTH 30 VALUE 'DATA_JOBNAME',
            jobtype TYPE c LENGTH 30 VALUE 'DATA_JOBTYPE',
            sortkey TYPE c LENGTH 30 VALUE 'DATA_SORTKEY',
           END OF ck_data_name.
CONSTANTS: BEGIN OF ck_data_type,
            jobname TYPE string VALUE 'BTCJOB',
            jobtype TYPE string VALUE 'WPTYP',
            sortkey TYPE string VALUE 'CHAR20',
           END OF ck_data_type.
CONSTANTS: BEGIN OF ck_job_type,
            foreground TYPE string VALUE 'DIA',
            background TYPE string VALUE 'BGD',
           END OF ck_job_type.
TYPES: BEGIN OF gtk_data,
         jobname       TYPE btcjob,
         jobtype       TYPE wptyp,
         progname      TYPE rsvar-report,
         print_param   TYPE pri_params,
         archive_param TYPE arc_params,
         variant_param TYPE rsparams_tt,
       END OF gtk_data.
TYPES: gti_data TYPE STANDARD TABLE OF gtk_data.

"Screen for Program, Field and Variant
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE s_b01.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (31) s_b01_01 FOR FIELD s_prgrpt.
PARAMETERS s_prgrpt TYPE rsvar-report MODIF ID xmt OBLIGATORY ##SEL_WRONG.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (31) s_b01_02 FOR FIELD s_prgf01.
PARAMETERS s_prgf01 TYPE rsscr_name OBLIGATORY  ##SEL_WRONG.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (31) s_b01_03 FOR FIELD s_prgvar.
PARAMETERS s_prgvar TYPE rsvar-variant MODIF ID xmt ##SEL_WRONG.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b01.

"Selection Logic Screen
SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE s_b02.
PARAMETERS s_select TYPE string LOWER CASE NO-DISPLAY ##SEL_WRONG.
SELECTION-SCREEN PUSHBUTTON 2(20) s_b02_01 USER-COMMAND sel1.
SELECTION-SCREEN PUSHBUTTON 24(22) s_b02_02 USER-COMMAND sel2.
SELECTION-SCREEN END OF BLOCK b02.

"Screen for Distribution options
SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE s_b03.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: s_dist01 RADIOBUTTON GROUP b03 DEFAULT 'X' ##SEL_WRONG.
SELECTION-SCREEN COMMENT (31) s_b03_01 FOR FIELD s_dist01.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: s_dist02 RADIOBUTTON GROUP b03 ##SEL_WRONG.
SELECTION-SCREEN COMMENT (31) s_b03_02 FOR FIELD s_dist02 .
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: s_dist03 RADIOBUTTON GROUP b03 ##SEL_WRONG.
SELECTION-SCREEN COMMENT (31) s_b03_03 FOR FIELD s_dist03.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b03.

"Screen for Processing options
SELECTION-SCREEN BEGIN OF BLOCK b04 WITH FRAME TITLE s_b04.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS s_jobty1 RADIOBUTTON GROUP b04 ##SEL_WRONG.
SELECTION-SCREEN COMMENT (29) s_b04_06 FOR FIELD s_procfg.
PARAMETERS s_procfg TYPE n LENGTH 4 DEFAULT 10 ##SEL_WRONG.
SELECTION-SCREEN COMMENT 38(3) s_b04_11.
PARAMETERS s_procbg TYPE n LENGTH 4 DEFAULT 10 ##SEL_WRONG.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS s_jobty2 RADIOBUTTON GROUP b04 ##SEL_WRONG.
SELECTION-SCREEN COMMENT (29) s_b04_02 FOR FIELD s_jobcn2.
PARAMETERS s_jobcn2 TYPE i DEFAULT 5000 ##SEL_WRONG.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN PUSHBUTTON 2(20) s_b04_12 USER-COMMAND sel4.
SELECTION-SCREEN BEGIN OF BLOCK b04_01 WITH FRAME TITLE s_b04_01.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (31) s_b04_07 FOR FIELD s_jobnam.
PARAMETERS s_jobnam TYPE c LENGTH 28 LOWER CASE ##SEL_WRONG.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: s_bgerr  AS CHECKBOX ##SEL_WRONG.
SELECTION-SCREEN COMMENT (50) s_b04_04 FOR FIELD s_bgerr.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: s_bgop   AS CHECKBOX DEFAULT 'X' ##SEL_WRONG.
SELECTION-SCREEN COMMENT (50) s_b04_03 FOR FIELD s_bgop.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (31) s_b04_05 FOR FIELD s_bgtime.
PARAMETERS: s_bgtime TYPE uzeit DEFAULT '030000' ##SEL_WRONG.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b04_01.
SELECTION-SCREEN BEGIN OF BLOCK b04_08 WITH FRAME TITLE s_b04_08.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (31) s_b04_09 FOR FIELD s_jobrfc.
PARAMETERS s_jobrfc TYPE spta_rfcgr MEMORY ID spta_rfcgr ##SEL_WRONG.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (31) s_b04_10 FOR FIELD s_jobmax.
PARAMETERS s_jobmax TYPE sy-index ##SEL_WRONG.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b04_08.
SELECTION-SCREEN END OF BLOCK b04.

"Screen for displaying data
SELECTION-SCREEN BEGIN OF BLOCK b05 WITH FRAME TITLE s_b05.
SELECTION-SCREEN PUSHBUTTON 2(20) s_b05_01 USER-COMMAND sel3.
SELECTION-SCREEN END OF BLOCK b05.

"Screen for Spool option
SELECTION-SCREEN BEGIN OF BLOCK b06 WITH FRAME TITLE s_b06.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT (31) s_b06_01 FOR FIELD s_plist.
PARAMETERS: s_plist TYPE syplist ##SEL_WRONG.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: s_sspl  AS CHECKBOX DEFAULT ' ' ##SEL_WRONG.
SELECTION-SCREEN COMMENT (31) s_b06_02 FOR FIELD s_sspl.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b06.

"Test key
SELECTION-SCREEN FUNCTION KEY: 1.

AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'SEL1'.
      PERFORM screen_select_editor CHANGING s_select.
    WHEN 'SEL2'.
      PERFORM screen_select_disp.
    WHEN 'SEL3' OR 'FC01'.
      PERFORM screen_distro_disp.
    WHEN 'SEL4'.
      PERFORM screen_server_info.
  ENDCASE.
  PERFORM screen_handle.

AT SELECTION-SCREEN OUTPUT.
  PERFORM screen_output.
  PERFORM screen_handle.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_prgrpt.
  PERFORM screen_prgrpt_help CHANGING s_prgrpt.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_prgvar.
  PERFORM screen_prgvar_help CHANGING s_prgvar.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR s_prgf01.
  PERFORM screen_prgfxx_help CHANGING s_prgf01.
  PERFORM screen_handle.

START-OF-SELECTION.
  PERFORM screen_check. "Do a final check before processing the data
  PERFORM process_data.

*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
FORM process_data.
  DATA: li_job_name TYPE STANDARD TABLE OF bapixmjobs.
  DATA: lo_data_tab TYPE REF TO data.
  DATA: lo_data_job TYPE REF TO data.
  DATA: li_prog_source TYPE rswsourcet.
  DATA: li_data TYPE gti_data.

  "Split program source from string
  SPLIT s_select AT cl_abap_char_utilities=>cr_lf INTO TABLE li_prog_source.

  "Get the data
  PERFORM screen_select_exec  USING li_prog_source CHANGING lo_data_tab.

  "Build the distribution
  PERFORM screen_distro_build USING lo_data_tab    CHANGING lo_data_job.

  "Compile all values in a complex structure
  PERFORM process_data_compile USING lo_data_job   CHANGING li_data.

  "Background processing
  PERFORM process_data_sendbg USING li_data        CHANGING li_job_name.

  "Foreground processing
  PERFORM process_data_sendfg USING li_data.

  "Keep track of all the background jobs
  PERFORM process_data_tracebg USING li_job_name.

ENDFORM.                    "process_data
*&---------------------------------------------------------------------*
*&      Form  process_data_compile
*&---------------------------------------------------------------------*
FORM process_data_compile USING lo_data_job TYPE REF TO data
                       CHANGING li_data TYPE gti_data.
  DATA: lo_job_list TYPE REF TO data.
  DATA: lo_job_handle TYPE REF TO cl_abap_tabledescr.
  DATA: lv_job_idx  TYPE i.
  DATA: lv_prgper_fieldtype TYPE grp3_____4.
  DATA: li_prog_var_params TYPE STANDARD TABLE OF rsparams.
  DATA: lk_prog_pri_params TYPE pri_params.
  DATA: lk_prog_arc_params TYPE arc_params.
  DATA: lv_spool_options TYPE c.
  FIELD-SYMBOLS: <lk_data>     TYPE gtk_data.
  FIELD-SYMBOLS: <li_job_list> TYPE STANDARD TABLE.
  FIELD-SYMBOLS: <lk_job_list> TYPE any.
  FIELD-SYMBOLS: <lv_job_name> TYPE btcjob.
  FIELD-SYMBOLS: <lv_job_type> TYPE wptyp.
  FIELD-SYMBOLS: <li_data_job> TYPE STANDARD TABLE.
  FIELD-SYMBOLS: <lk_data_job> TYPE any.
  FIELD-SYMBOLS: <lv_data_jobname> TYPE btcjob.
  FIELD-SYMBOLS: <lv_data_content> TYPE any.
  FIELD-SYMBOLS: <lk_prog_var_data> TYPE rsparams.

  "Get the field type.
  PERFORM screen_prgfxx_fieldtype USING s_prgf01 CHANGING lv_prgper_fieldtype.

  "Retrieve program variant
  CALL FUNCTION 'RS_VARIANT_CONTENTS'
    EXPORTING
      report               = s_prgrpt
      variant              = s_prgvar
      move_or_write        = 'M'
    TABLES
      valutab              = li_prog_var_params
    EXCEPTIONS
      variant_non_existent = 1
      variant_obsolete     = 2
      OTHERS               = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  "Clear off values that might be saved in the variant
  DELETE li_prog_var_params WHERE selname = s_prgf01.

  "Combine Spool into single
  IF s_sspl IS NOT INITIAL.
    lv_spool_options = ' '.
  ELSE.
    lv_spool_options = 'X'.
  ENDIF.

  "Print Parameters
  CALL FUNCTION 'GET_PRINT_PARAMETERS'
    EXPORTING
      no_dialog              = 'X'
      mode                   = 'CURRENT'
      new_list_id            = lv_spool_options
      list_name              = s_plist
      immediately            = lv_spool_options
    IMPORTING
      out_parameters         = lk_prog_pri_params
      out_archive_parameters = lk_prog_arc_params.

  "Make sure previous spool is closed
  IF s_sspl IS NOT INITIAL.
    UPDATE tsp01 SET rqfinal = 'C'
      WHERE rqclient EQ sy-mandt AND rq0name EQ lk_prog_pri_params-prdsn AND rq1name EQ lk_prog_pri_params-pdest AND rq2name EQ lk_prog_pri_params-plist AND rqowner EQ lk_prog_pri_params-prrec AND rqfinal NE 'C'.
  ENDIF.

  "Copy data/job table
  lo_job_handle ?= cl_abap_tabledescr=>describe_by_data_ref( lo_data_job ).
  CREATE DATA lo_job_list TYPE HANDLE lo_job_handle.
  ASSIGN lo_job_list->* TO <li_job_list>.
  ASSIGN lo_data_job->* TO <li_data_job>.
  <li_job_list> = <li_data_job>.

  "Get unique list of jobs
  SORT <li_job_list> BY (ck_data_name-jobname).
  SORT <li_data_job> BY (ck_data_name-jobname).
  DELETE ADJACENT DUPLICATES FROM <li_job_list> COMPARING (ck_data_name-jobname).

  "Compile content into data table
  LOOP AT <li_job_list> ASSIGNING <lk_job_list>.
    APPEND INITIAL LINE TO li_data ASSIGNING <lk_data>.

    "Program Name
    <lk_data>-progname = s_prgrpt.

    "Get Job Type
    ASSIGN COMPONENT ck_data_name-jobtype OF STRUCTURE <lk_job_list> TO <lv_job_type>.
    <lk_data>-jobtype = <lv_job_type>.

    "Get Job Name
    ASSIGN COMPONENT ck_data_name-jobname OF STRUCTURE <lk_job_list> TO <lv_job_name>.
    <lk_data>-jobname = <lv_job_name>.

    "Printing parameters
    <lk_data>-print_param = lk_prog_pri_params.

    "Archiving parameters
    <lk_data>-archive_param = lk_prog_arc_params.

    "Build variant parameters
    READ TABLE <li_data_job> TRANSPORTING NO FIELDS WITH KEY (ck_data_name-jobname) = <lk_data>-jobname.
    lv_job_idx = sy-tabix.
    LOOP AT <li_data_job> ASSIGNING <lk_data_job> FROM lv_job_idx.
      ASSIGN COMPONENT ck_data_name-jobname OF STRUCTURE <lk_data_job> TO <lv_data_jobname>.
      IF <lv_data_jobname> NE <lk_data>-jobname. EXIT. ENDIF.
      ASSIGN COMPONENT ck_data_name-f01 OF STRUCTURE <lk_data_job> TO <lv_data_content>.

      APPEND INITIAL LINE TO <lk_data>-variant_param ASSIGNING <lk_prog_var_data>.
      <lk_prog_var_data>-selname = s_prgf01.
      CASE lv_prgper_fieldtype.
        WHEN 'LOW'.
          <lk_prog_var_data>-kind = 'S'.
          <lk_prog_var_data>-sign = 'I'.
          <lk_prog_var_data>-option = 'EQ'.
          <lk_prog_var_data>-low = <lv_data_content>.
        WHEN 'PAR'.
          <lk_prog_var_data>-kind = 'P'.
          <lk_prog_var_data>-low = <lv_data_content>.
      ENDCASE.
    ENDLOOP.

    "Put in the rest of variant values
    APPEND LINES OF li_prog_var_params TO <lk_data>-variant_param.
  ENDLOOP.
ENDFORM.                    "process_data_compile
*&---------------------------------------------------------------------*
*&      Form  process_data_sendbg
*&---------------------------------------------------------------------*
FORM process_data_sendbg USING li_data TYPE gti_data
                      CHANGING li_job_name TYPE STANDARD TABLE.
  DATA: li_data_ref TYPE gti_data.
  DATA: lo_job      TYPE REF TO cl_bp_abap_job.
  FIELD-SYMBOLS: <lk_job_name> TYPE bapixmjobs.
  FIELD-SYMBOLS: <lk_data> TYPE gtk_data.

  "Process only dialogs
  li_data_ref[] = li_data[].
  DELETE li_data_ref WHERE jobtype NE ck_job_type-background.

  LOOP AT li_data_ref ASSIGNING <lk_data>.
    CLEAR lo_job.

    "Create Job
    CREATE OBJECT lo_job.
    lo_job->name = <lk_data>-jobname.
    lo_job->if_bp_job_engine~generate_job_count( EXCEPTIONS OTHERS = 1 ).
    CHECK sy-subrc = 0.

    "Call program in the job
    SUBMIT (<lk_data>-progname) TO SAP-SPOOL SPOOL PARAMETERS <lk_data>-print_param ARCHIVE PARAMETERS <lk_data>-archive_param WITHOUT SPOOL DYNPRO
      WITH SELECTION-TABLE <lk_data>-variant_param
      VIA JOB lo_job->name NUMBER lo_job->jobcount AND RETURN.
    CHECK sy-subrc = 0.

    "Start Job
    lo_job->if_bp_job_engine~start_immediately( EXCEPTIONS OTHERS = 1 ).
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    "Put this into job name list
    APPEND INITIAL LINE TO li_job_name ASSIGNING <lk_job_name>.
    <lk_job_name>-jobname = lo_job->name.
    <lk_job_name>-jobcount = lo_job->jobcount.
  ENDLOOP.
ENDFORM.                    "process_data_sendbg
*&---------------------------------------------------------------------*
*&      Form  process_data_tracebg
*&---------------------------------------------------------------------*
FORM process_data_tracebg USING li_jobs TYPE STANDARD TABLE.
  CONSTANTS: lc_second TYPE i VALUE 60.
  DATA: lv_timestamp     TYPE timestamp.
  DATA: lv_stop_seconds  TYPE i.
  DATA: lv_stop_date     TYPE d.
  DATA: lv_stop_time     TYPE t.
  DATA: lv_count_active  TYPE p.
  DATA: li_job_name TYPE STANDARD TABLE OF bapixmjobs.

  "Only keep track of this if the main program is running in background mode
  IF sy-batch IS NOT INITIAL and li_jobs[] is not INITIAL.
    li_job_name = li_jobs.
    lv_stop_seconds = ( s_bgtime+0(2) * lc_second * lc_second ) + ( s_bgtime+2(2) * lc_second ) + s_bgtime+4(2).
    CONVERT DATE sy-datum TIME sy-uzeit INTO TIME STAMP lv_timestamp TIME ZONE sy-zonlo.
    lv_timestamp = cl_abap_tstmp=>add( tstmp = lv_timestamp secs = lv_stop_seconds ).
    CONVERT TIME STAMP lv_timestamp TIME ZONE sy-zonlo INTO DATE lv_stop_date TIME lv_stop_time.

    WHILE ( sy-uzeit LE lv_stop_time AND sy-datum EQ lv_stop_date ) OR    "job started and end on the same day
          ( sy-uzeit GE lv_stop_time AND sy-datum LE lv_stop_date ).      "job started but end on the next day
      CLEAR: lv_count_active.

      SELECT COUNT(*) INTO lv_count_active FROM v_op FOR ALL ENTRIES IN li_job_name WHERE jobname EQ li_job_name-jobname AND jobcount EQ li_job_name-jobcount AND progname EQ s_prgrpt AND status  IN ('Y', 'R').  "ready,active
      IF lv_count_active GT 0.
        WAIT UP TO 10 SECONDS.
      ELSE.
        EXIT.
      ENDIF.
    ENDWHILE.
  ENDIF.
ENDFORM.                    "process_data_tracebg

*&---------------------------------------------------------------------*
*&      Form  process_data_sendfg
*&---------------------------------------------------------------------*
FORM process_data_sendfg USING li_data TYPE gti_data.
  DATA: li_data_ref TYPE gti_data.

  "Process only dialogs
  li_data_ref[] = li_data[].
  DELETE li_data_ref WHERE jobtype NE ck_job_type-foreground.

  "Call SPTA parallel processing
  CALL FUNCTION 'SPTA_PARA_PROCESS_START_2'
    EXPORTING
      server_group             = s_jobrfc
      max_no_of_tasks          = s_jobmax
      before_rfc_callback_form = 'PROCESS_DATA_SENDFG01'
      in_rfc_callback_form     = 'PROCESS_DATA_SENDFG02'
      after_rfc_callback_form  = 'PROCESS_DATA_SENDFG03'
      callback_prog            = sy-repid
    CHANGING
      user_param               = li_data_ref
    EXCEPTIONS
      invalid_server_group     = 1
      no_resources_available   = 2
      OTHERS                   = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    "process_data_sendfg

*&---------------------------------------------------------------------*
*&      Form  screen_prgfxx_fieldinfo
*&---------------------------------------------------------------------*
FORM screen_prgfxx_fieldinfo USING lv_field      TYPE rsscr_name
                          CHANGING lv_field_type TYPE c
                                   lv_field_len  TYPE i
                                   lv_field_dec  TYPE i.
  TYPES: BEGIN OF ltk_dynproname,
         prog type d020s-prog,
         dnum type d020s-dnum,
       END OF ltk_dynproname.
  DATA: lv_field_name TYPE      rs38m-stexti.
  DATA: lv_field_byte TYPE      x LENGTH 8.
  DATA: lk_dynproname TYPE ltk_dynproname.
  DATA: li_dynproname TYPE STANDARD TABLE OF ltk_dynproname.
  DATA: lk_dynpheader TYPE d020s                                ##NEEDED.
  DATA: li_dynpfields TYPE STANDARD TABLE OF d021s.
  DATA: li_dynpfields_temp TYPE STANDARD TABLE OF d021s.
  DATA: li_dynplogic  TYPE STANDARD TABLE OF d022s              ##NEEDED.
  DATA: li_dynpmatchc TYPE STANDARD TABLE OF d023s              ##NEEDED.
  DATA: lv_dummy      TYPE c LENGTH 30                          ##NEEDED.
  FIELD-SYMBOLS: <lk_dynpfields> TYPE d021s.

  SELECT prog dnum FROM d020s INTO TABLE li_dynproname WHERE prog EQ s_prgrpt.

  LOOP AT li_dynproname INTO  lk_dynproname.
    CLEAR: lk_dynpheader, li_dynpfields_temp[], li_dynplogic[], li_dynpmatchc[].
    IMPORT DYNPRO lk_dynpheader li_dynpfields_temp li_dynplogic li_dynpmatchc ID lk_dynproname.

    DELETE li_dynpfields_temp WHERE fmb1 NE '00'.
    DELETE li_dynpfields_temp WHERE flg3 EQ '00'.
    DELETE li_dynpfields_temp WHERE grp3 EQ 'HGH'.
    DELETE li_dynpfields_temp WHERE fill EQ 'C' OR fill EQ 'A'.
    APPEND LINES OF li_dynpfields_temp TO li_dynpfields.
  ENDLOOP.

  LOOP AT li_dynpfields ASSIGNING <lk_dynpfields>.
    SPLIT <lk_dynpfields>-fnam AT '-' INTO lv_field_name lv_dummy.
    IF sy-subrc NE 0.
      lv_field_name = <lk_dynpfields>-fnam.
    ENDIF.

    IF lv_field_name = lv_field.
      "Type
      lv_field_type = <lk_dynpfields>-ityp.
      "Length
      lv_field_byte = <lk_dynpfields>-leng.
      cl_abap_conv_in_ce=>create( EXPORTING encoding = 'UTF-8' input = lv_field_byte )->read( IMPORTING data = lv_field_len ).
      "Decimals
      lv_field_byte = <lk_dynpfields>-adez.
      cl_abap_conv_in_ce=>create( EXPORTING encoding = 'UTF-8' input = lv_field_byte )->read( IMPORTING data = lv_field_dec ).
    ENDIF.
  ENDLOOP.
ENDFORM.                    "screen_prgfxx_fieldinfo
*&---------------------------------------------------------------------*
*&      Form  screen_prgfxx_fieldtype
*&---------------------------------------------------------------------*
FORM screen_prgfxx_fieldtype USING lv_field      TYPE rsscr_name
                          CHANGING lv_field_type TYPE grp3_____4.
  TYPES: BEGIN OF ltk_dynproname,
         prog type d020s-prog,
         dnum type d020s-dnum,
       END OF ltk_dynproname.
  DATA: lv_field_name TYPE      rs38m-stexti.
  DATA: lk_dynproname TYPE ltk_dynproname.
  DATA: li_dynproname TYPE STANDARD TABLE OF ltk_dynproname.
  DATA: lk_dynpheader TYPE d020s                                ##NEEDED.
  DATA: li_dynpfields TYPE STANDARD TABLE OF d021s.
  DATA: li_dynpfields_temp TYPE STANDARD TABLE OF d021s.
  DATA: li_dynplogic  TYPE STANDARD TABLE OF d022s              ##NEEDED.
  DATA: li_dynpmatchc TYPE STANDARD TABLE OF d023s              ##NEEDED.
  DATA: lv_dummy      TYPE c LENGTH 30                          ##NEEDED.
  FIELD-SYMBOLS: <lk_dynpfields> TYPE d021s.

  SELECT prog dnum FROM d020s INTO TABLE li_dynproname WHERE prog EQ s_prgrpt.

  LOOP AT li_dynproname INTO  lk_dynproname.
    CLEAR: lk_dynpheader, li_dynpfields_temp[], li_dynplogic[], li_dynpmatchc[].
    IMPORT DYNPRO lk_dynpheader li_dynpfields_temp li_dynplogic li_dynpmatchc ID lk_dynproname.

    DELETE li_dynpfields_temp WHERE fmb1 NE '00'.
    DELETE li_dynpfields_temp WHERE flg3 EQ '00'.
    DELETE li_dynpfields_temp WHERE grp3 EQ 'HGH'.
    DELETE li_dynpfields_temp WHERE fill EQ 'C' OR fill EQ 'A'.
    APPEND LINES OF li_dynpfields_temp TO li_dynpfields.
  ENDLOOP.

  LOOP AT li_dynpfields ASSIGNING <lk_dynpfields>.
    SPLIT <lk_dynpfields>-fnam AT '-' INTO lv_field_name lv_dummy.
    IF sy-subrc NE 0.
      lv_field_name = <lk_dynpfields>-fnam.
    ENDIF.

    IF lv_field_name = lv_field.
      "Type
      lv_field_type = <lk_dynpfields>-grp3.
    ENDIF.
  ENDLOOP.
ENDFORM.                    "screen_prgfxx_fieldtype


*&---------------------------------------------------------------------*
*&      Form  screen_select_genprog
*&---------------------------------------------------------------------*
FORM screen_select_genprog  USING li_prog_source TYPE rswsourcet
                         CHANGING lv_subrc       TYPE sysubrc
                                  lv_prog_name   TYPE string ##NEEDED
                                  lv_prog_msg    TYPE string ##NEEDED
                                  lv_prog_sid    TYPE string ##NEEDED.
  DATA: li_program TYPE rswsourcet.
  DATA: li_program_text TYPE rswsourcet.
  DATA: lv_program TYPE string.
  DATA: lv_field_typ TYPE c.
  DATA: lv_field_len TYPE i.
  DATA: lv_field_dec TYPE i.
  DATA: lv_program_line TYPE  i.



  lv_program = |PROGRAM.|.                                                    APPEND lv_program TO li_program.
  lv_program = |FORM | && c_form_name && | CHANGING data TYPE REF TO data.|. APPEND lv_program TO li_program.
  lv_program = |  DATA lo_data TYPE REF TO data.|.                            APPEND lv_program TO li_program.
  lv_program = |  FIELD-SYMBOLS <data> TYPE any.|.                            APPEND lv_program TO li_program.
  lv_program = |  FIELD-SYMBOLS <data_tab> TYPE STANDARD TABLE.|.             APPEND lv_program TO li_program.

  "Get the field information and create dynamic program based on the types and length
  PERFORM screen_prgfxx_fieldinfo USING s_prgf01 CHANGING lv_field_typ lv_field_len lv_field_dec.
  CASE lv_field_typ.
    WHEN 'D' OR 'F' OR 'I'.
      lv_program = |  CREATE DATA lo_data TYPE | && lv_field_typ && |.|.      APPEND lv_program TO li_program.
    WHEN 'C' OR 'N' OR 'X'.
      lv_program = |  CREATE DATA lo_data TYPE | && lv_field_typ && | LENGTH | && lv_field_len && |.|. APPEND lv_program TO li_program.
    WHEN 'P'.
      lv_program = |  CREATE DATA lo_data TYPE p LENGTH | && lv_field_len && | DECIMALS | && lv_field_dec && |.|. APPEND lv_program TO li_program.
  ENDCASE.

  lv_program = |  ASSIGN lo_data->* TO <data>.|.                              APPEND lv_program TO li_program.
  lv_program = |  CREATE DATA data LIKE STANDARD TABLE OF <data>.|.           APPEND lv_program TO li_program.
  lv_program = |  ASSIGN data->* TO <data_tab>.|.                             APPEND lv_program TO li_program.

  "Pretty Print it first to prevent whitespaces in FORM usage
  li_program_text[] = li_prog_source[].
  PERFORM screen_select_pretty CHANGING li_program_text.

  "Look for FORM
  FIND FIRST OCCURRENCE OF REGEX '^\bform\b'
    IN TABLE li_program_text IGNORING CASE MATCH LINE   lv_program_line.
  IF sy-subrc = 0.
    APPEND LINES OF li_program_text FROM 1 TO ( lv_program_line - 1 ) TO li_program .
    lv_program = |ENDFORM.|.                                                 APPEND lv_program TO li_program.
    APPEND LINES OF li_program_text FROM lv_program_line TO li_program.
  ELSE.
    "Retrieve string value and put it into the method
    APPEND LINES OF li_program_text TO li_program.
    lv_program = |ENDFORM.|.                                                 APPEND lv_program TO li_program.
  ENDIF.

  "Generate program
  GENERATE SUBROUTINE POOL li_program NAME lv_prog_name MESSAGE lv_prog_msg SHORTDUMP-ID lv_prog_sid.
  lv_subrc = sy-subrc.
ENDFORM.                    "screen_select_genprog
*&---------------------------------------------------------------------*
*&      Form  screen_select_editor
*&---------------------------------------------------------------------*
FORM screen_select_editor CHANGING lv_selection_prog TYPE string.
  DATA: li_program TYPE rswsourcet.
  FIELD-SYMBOLS: <lk_program> LIKE LINE OF li_program.

  IF lv_selection_prog IS INITIAL.
    lv_selection_prog =
      `* Populate internal table <data_tab> with values of your field specified in the selection screen` && cl_abap_char_utilities=>cr_lf &&
      `* Example : SELECT carrid FROM alv_t_t2 INTO TABLE <data_tab> WHERE carrid EQ 'AA'.`.
  ENDIF.

  SPLIT lv_selection_prog AT cl_abap_char_utilities=>cr_lf INTO TABLE li_program.
  CALL FUNCTION 'RS_NAVIGATION_BREAK'.
  CALL FUNCTION 'EDITOR_APPLICATION'
    EXPORTING
      application        = 'TT'
      callback_program   = sy-repid
      callback_usercom   = 'SCREEN_SELECT_UCOMM'
      callback_set_pfkey = 'SCREEN_SELECT_GUI'
      title_text         = 'Selection Logic' ##NO_TEXT
    TABLES
      content            = li_program
    EXCEPTIONS
      line               = 1
      linenumbers        = 2
      offset             = 3
      OTHERS             = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  CLEAR lv_selection_prog.
  LOOP AT li_program ASSIGNING <lk_program>.
    lv_selection_prog = lv_selection_prog && <lk_program> && cl_abap_char_utilities=>cr_lf.
  ENDLOOP.
ENDFORM.                    "screen_select_editor

*&---------------------------------------------------------------------*
*&      Form  screen_prgvar_help
*&---------------------------------------------------------------------*
FORM screen_prgvar_help CHANGING lk_prgvari TYPE variant.
  DATA: lv_prgname TYPE progname.
  DATA: lv_title TYPE c LENGTH 32.
  DATA: li_dynpfields TYPE STANDARD TABLE OF dynpread.
  FIELD-SYMBOLS: <lk_dynpfields> TYPE dynpread.

  APPEND INITIAL LINE TO li_dynpfields ASSIGNING <lk_dynpfields>.
  <lk_dynpfields>-fieldname = 'S_PRGRPT'.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname             = sy-repid
      dynumb             = '1000'
      translate_to_upper = 'X'
    TABLES
      dynpfields         = li_dynpfields.

  READ TABLE li_dynpfields ASSIGNING <lk_dynpfields> WITH KEY fieldname = 'S_PRGRPT'.
  IF sy-subrc = 0.
    lv_prgname = <lk_dynpfields>-fieldvalue.
  ENDIF.

  lv_title = |Variants for program | && lv_prgname.

  CALL FUNCTION 'RS_VARIANT_CATALOG'
    EXPORTING
      report              = lv_prgname
      new_title           = lv_title
    IMPORTING
      sel_variant         = lk_prgvari
    EXCEPTIONS
      no_report           = 01
      report_not_existent = 02
      report_not_supplied = 03.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " screen_prgvar_help
*&---------------------------------------------------------------------*
*&      Form  screen_prgrpt_help
*&---------------------------------------------------------------------*
FORM screen_prgrpt_help  CHANGING lv_prgrpt TYPE rsvar-report.
  CALL FUNCTION 'REPOSITORY_INFO_SYSTEM_F4'
    EXPORTING
      object_type          = 'PROG'
      object_name          = lv_prgrpt
      suppress_selection   = 'X'
    IMPORTING
      object_name_selected = lv_prgrpt
    EXCEPTIONS
      cancel               = 01
      OTHERS               = 02.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.                    " screen_prgrpt_help
*&---------------------------------------------------------------------*
*&      Form  screen_prgfxx_help
*&---------------------------------------------------------------------*
FORM screen_prgfxx_help  CHANGING lv_prgper TYPE rsscr_name.
  TYPES: BEGIN OF ltk_dynproname,
         prog type d020s-prog,
         dnum type d020s-dnum,
       END OF ltk_dynproname.
  TYPES: BEGIN OF ltk_progtext,
         stexti type rs38m-stexti,
         stextt type rs38m-stextt,
       END OF ltk_progtext.
  DATA: lk_dynproname TYPE ltk_dynproname.
  DATA: li_dynproname TYPE STANDARD TABLE OF ltk_dynproname.
  DATA: li_dynpfield TYPE STANDARD TABLE OF dynpread.
  DATA: li_dynpfields TYPE STANDARD TABLE OF d021s.
  DATA: li_dynpfields_temp TYPE STANDARD TABLE OF d021s.
  DATA: li_return     TYPE STANDARD TABLE OF ddshretval.
  DATA: li_progtext   TYPE STANDARD TABLE OF ltk_progtext.
  DATA: li_dynpftext  TYPE STANDARD TABLE OF textpool.
  DATA: lk_dynpheader TYPE d020s                                ##NEEDED.
  DATA: li_dynplogic  TYPE STANDARD TABLE OF d022s              ##NEEDED.
  DATA: li_dynpmatchc TYPE STANDARD TABLE OF d023s              ##NEEDED.
  DATA: lv_dummy      TYPE c LENGTH 30                          ##NEEDED.

  FIELD-SYMBOLS: <lk_dynpfield>  TYPE dynpread.
  FIELD-SYMBOLS: <lk_dynpfields> TYPE d021s.
  FIELD-SYMBOLS: <lk_progtext>   TYPE ltk_progtext.
  FIELD-SYMBOLS: <lk_dynpftext>  TYPE textpool.
  FIELD-SYMBOLS: <lk_return>     TYPE ddshretval.

  "Retrieve values for Report name
  lk_dynproname-prog = s_prgrpt.
  IF s_prgrpt IS INITIAL.
    APPEND INITIAL LINE TO li_dynpfield ASSIGNING <lk_dynpfield>.
    <lk_dynpfield>-fieldname = 'S_PRGRPT'.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname             = sy-repid
        dynumb             = '1000'
        translate_to_upper = 'X'
      TABLES
        dynpfields         = li_dynpfield.

    READ TABLE li_dynpfield ASSIGNING <lk_dynpfield> WITH KEY fieldname = 'S_PRGRPT'.
    IF sy-subrc = 0.
      lk_dynproname-prog = <lk_dynpfield>-fieldvalue.
    ENDIF.
  ENDIF.

  IF lk_dynproname-prog IS NOT INITIAL.

    SELECT prog dnum FROM d020s INTO TABLE li_dynproname WHERE prog EQ lk_dynproname-prog.

    CALL FUNCTION 'RS_TEXTPOOL_READ'
      EXPORTING
        objectname           = lk_dynproname-prog
        action               = 'SHOW'
      TABLES
        tpool                = li_dynpftext
      EXCEPTIONS
        object_not_found     = 1
        permission_failure   = 2
        invalid_program_type = 3
        error_occured        = 4
        action_cancelled     = 5
        OTHERS               = 6.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    LOOP AT li_dynproname INTO lk_dynproname.
      CLEAR: lk_dynpheader, li_dynpfields_temp[], li_dynplogic[], li_dynpmatchc[].
      IMPORT DYNPRO lk_dynpheader li_dynpfields_temp li_dynplogic li_dynpmatchc ID lk_dynproname.

      DELETE li_dynpfields_temp WHERE fmb1 NE '00'.
      DELETE li_dynpfields_temp WHERE flg3 EQ '00'.
      DELETE li_dynpfields_temp WHERE grp3 EQ 'HGH'.
      DELETE li_dynpfields_temp WHERE fill EQ 'C' OR fill EQ 'A'.
      APPEND LINES OF li_dynpfields_temp TO li_dynpfields.
    ENDLOOP.

    LOOP AT li_dynpfields ASSIGNING <lk_dynpfields>.
      APPEND INITIAL LINE TO li_progtext ASSIGNING <lk_progtext>.
      SPLIT <lk_dynpfields>-fnam AT '-' INTO <lk_progtext>-stexti lv_dummy.
      IF sy-subrc NE 0.
        <lk_progtext>-stexti = <lk_dynpfields>-fnam.
      ENDIF.
      READ TABLE li_dynpftext ASSIGNING <lk_dynpftext> WITH KEY id = 'S' key = <lk_progtext>-stexti.
      IF sy-subrc = 0.
        <lk_progtext>-stextt = <lk_dynpftext>-entry+8(30).
      ENDIF.
    ENDLOOP.


    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'STEXTI'
        value_org       = 'S'
      TABLES
        value_tab       = li_progtext
        return_tab      = li_return
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    READ TABLE li_return ASSIGNING <lk_return> INDEX 1.
    IF sy-subrc = 0.
      lv_prgper = <lk_return>-fieldval.
    ENDIF.
  ENDIF.
ENDFORM.                    " screen_prgfxx_help

*&---------------------------------------------------------------------*
*&      Form  SCREEN_SELECT_GUI
*&---------------------------------------------------------------------*
FORM screen_select_gui ##CALLED .
  DATA li_exclude TYPE STANDARD TABLE OF syucomm.
  APPEND 'WB_BACK_TB' TO li_exclude.
  APPEND 'WB_FORWARD' TO li_exclude.
  APPEND 'WB_DISP_EDIT_TOGGLE' TO li_exclude.
  APPEND 'WB_ACT_INACT_TOGGLE' TO li_exclude.
  APPEND 'WB_OTHER_OBJECT' TO li_exclude.
  APPEND 'WB_EDIT_ENH' TO li_exclude.
  APPEND 'WB_ACTIVATE' TO li_exclude.
  APPEND 'WB_CHECK' TO li_exclude.
  APPEND 'WB_CHECK_MAINPROG' TO li_exclude.
  APPEND 'WB_GENERATE' TO li_exclude.
  APPEND 'WB_ATC_CHECK_DEFAULT' TO li_exclude.
  APPEND 'WB_ATC_CHECK' TO li_exclude.
  APPEND 'WB_CODE_INSPECTOR' TO li_exclude.
  APPEND 'WB_CHECK_GENLIMITS' TO li_exclude.
  APPEND 'WB_CHECK_EXTENDED' TO li_exclude.
  APPEND 'WB_PACKAGE_CHECK' TO li_exclude.
  APPEND 'ED_PUSH_TO_LMEM' TO li_exclude.
  APPEND 'ED_POP_FROM_LMEM' TO li_exclude.
  APPEND 'WB_WHERE_USED_LIST' TO li_exclude.
  APPEND 'WB_EXEC_DEBUG' TO li_exclude.
  APPEND 'ED_MODULTEST' TO li_exclude.
  APPEND 'WB_MODULE_TEST_COV' TO li_exclude.
  APPEND 'WB_OBJECT_LIST' TO li_exclude.
  APPEND 'ED_PROG_ATTR' TO li_exclude.
  APPEND 'ED_LIST_TOP' TO li_exclude.
  APPEND 'ED_SEL_TEXT' TO li_exclude.
  APPEND 'ED_NUM_TEXT' TO li_exclude.
  APPEND 'ED_SEARCH_REPL' TO li_exclude.
  APPEND 'ED_SEARCH_NEXT' TO li_exclude.
  APPEND 'ED_REPLACE_NEXT' TO li_exclude.
  APPEND 'ED_COMMAND_RESET' TO li_exclude.
  APPEND 'ED_RESOLVE_INCL' TO li_exclude.
  APPEND 'ED_COMPRIM_INCL' TO li_exclude.
  APPEND 'ED_SAVE_INCL' TO li_exclude.
  APPEND 'ED_COMPRESS' TO li_exclude.
  APPEND 'ED_DECOMPRESS' TO li_exclude.
  APPEND 'ED_COMPRESS_ALL' TO li_exclude.
  APPEND 'ED_DECOMPRESS_ALL' TO li_exclude.
  APPEND 'ED_BLOCK_DEFINE' TO li_exclude.
  APPEND 'ED_SWF_ENHANCEMENT' TO li_exclude.
  APPEND 'ED_SWF_RESET' TO li_exclude.
  APPEND 'ED_SWF_SPOTS' TO li_exclude.
  APPEND 'ED_SWF_IMPLICIT' TO li_exclude.
  APPEND 'ED_SWF_IMPLICIT_OFF' TO li_exclude.
  APPEND 'WB_MESSAGES' TO li_exclude.
  APPEND 'ED_PROG_VARI' TO li_exclude.
  APPEND 'WB_TADIR_EDIT' TO li_exclude.
  APPEND 'WB_DOCUMENTATION' TO li_exclude.
  APPEND 'WB_TRANSLATION' TO li_exclude.
  APPEND 'WB_WL_ADD_OBJECT' TO li_exclude.
  APPEND 'WB_WL_SHOW' TO li_exclude.
  APPEND 'ED_INDEX_ACT' TO li_exclude.
  APPEND 'ED_SHOW_BREAK' TO li_exclude.
  APPEND 'ED_SET_BREAK' TO li_exclude.
  APPEND 'ED_DELETE_BREAK' TO li_exclude.
  APPEND 'ED_SHOW_EXT_BREAK' TO li_exclude.
  APPEND 'ED_SET_EXT_BREAK' TO li_exclude.
  APPEND 'ED_ACTIVATE_BREAK' TO li_exclude.
  APPEND 'ED_DELETE_EXT_BREAK' TO li_exclude.
  APPEND 'WB_VERSIONS' TO li_exclude.
  APPEND 'WB_VERSION_CREATE' TO li_exclude.
  APPEND 'WB_ACTIVE_VERSION' TO li_exclude.
  APPEND 'ED_PATTERN_CREATE' TO li_exclude.
  APPEND 'ED_PATTERN_EDIT' TO li_exclude.
  APPEND 'ED_PATTERN_DELETE' TO li_exclude.
  APPEND 'ED_PATTERN_SHOW' TO li_exclude.
  APPEND '+PGE' TO li_exclude.
  SET PF-STATUS 'WB_WITH_TOOL_PC' OF PROGRAM 'SAPLS38E' EXCLUDING li_exclude.
ENDFORM.             "SCREEN_SELECT_GUI

*&---------------------------------------------------------------------*
*&      Form  SCREEN_SELECT_UCOMM
*&---------------------------------------------------------------------*
FORM screen_select_ucomm ##CALLED. "This is called from FM EDITOR_APPLICATION
  DATA: lo_data_tab TYPE REF TO data.
  DATA: lo_source TYPE REF TO cl_wb_source.
  DATA: li_source TYPE rswsourcet.
  FIELD-SYMBOLS: <lo_editor> TYPE REF TO cl_wb_editor.

  CASE sy-ucomm.
    WHEN 'ED_PRETTY_PRINT'.
      "Retrieve source
      ASSIGN ('(SAPLS38E)ABAP_EDITOR') TO <lo_editor>.
      <lo_editor>->get_source_instance( IMPORTING source_object = lo_source ).
      lo_source->get_source_tab( EXPORTING use_control = 'X' IMPORTING source = li_source[] ).
      "Pretty Print it
      PERFORM screen_select_pretty CHANGING li_source.
      "Set the pretty source
      lo_source->set_source_tab( EXPORTING source = li_source[] not_actualize_control = 'X' ).
      <lo_editor>->visualize_source( ).

      "Call DUMY to prevent error message
      CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
        EXPORTING
          functioncode = 'DUMY'.
    WHEN 'WB_EXEC'.
      "Retrieve Source
      ASSIGN ('(SAPLS38E)ABAP_EDITOR') TO <lo_editor>.
      <lo_editor>->get_source_instance( IMPORTING source_object = lo_source ).
      lo_source->get_source_tab( EXPORTING use_control = 'X' IMPORTING source = li_source[] ).
      "Execute ALV
      PERFORM screen_select_exec USING li_source CHANGING lo_data_tab.
      "Display ALV
      PERFORM screen_alv USING lo_data_tab.
  ENDCASE.
ENDFORM.                    "screen_select_ucomm
*&---------------------------------------------------------------------*
*&      Form  screen_select_disp
*&---------------------------------------------------------------------*
FORM screen_select_disp.
  DATA: lo_data_tab TYPE REF TO data.
  DATA: li_prog_source TYPE rswsourcet.
  "Split program source from string
  SPLIT s_select AT cl_abap_char_utilities=>cr_lf INTO TABLE li_prog_source.

  "Generate program and call the method
  PERFORM screen_select_exec USING  li_prog_source CHANGING lo_data_tab.

  "Display ALV
  PERFORM screen_alv USING lo_data_tab.
ENDFORM.                    "screen_select_disp
*&---------------------------------------------------------------------*
*&      Form  screen_select_exec
*&---------------------------------------------------------------------*
FORM screen_select_exec USING li_prog_source TYPE rswsourcet
                     CHANGING lo_result_tab TYPE REF TO data.
  DATA: lo_data_tab TYPE REF TO data.
  DATA: lv_subrc TYPE sysubrc.
  DATA: lv_prog_name TYPE string,
        lv_prog_mess TYPE string,
        lv_prog_sid  TYPE string.
  DATA: li_data_strcmp TYPE cl_abap_structdescr=>component_table.
  FIELD-SYMBOLS: <li_data> TYPE STANDARD TABLE.
  FIELD-SYMBOLS: <li_result> TYPE STANDARD TABLE.

  "Generate program and call the method
  PERFORM screen_select_genprog USING li_prog_source CHANGING lv_subrc lv_prog_name lv_prog_mess lv_prog_sid.
  CASE lv_subrc.
    WHEN 0.
      "Retrieve data
      TRY .
          PERFORM (c_form_name) IN PROGRAM (lv_prog_name) CHANGING lo_data_tab IF FOUND.
        CATCH cx_root ##CATCH_ALL.
          MESSAGE 'Error on the selection logic' TYPE 'E' ##NO_TEXT.
      ENDTRY.
    WHEN 4. MESSAGE lv_prog_mess TYPE 'E'.
    WHEN 8. MESSAGE lv_prog_sid TYPE 'E'.
  ENDCASE.

  "Build a proper structured table and copy the information over. (ALV will fail if there's no structure)
  PERFORM screen_struct_build USING li_data_strcmp CHANGING lo_result_tab.
  ASSIGN lo_result_tab->* TO <li_result>.
  ASSIGN lo_data_tab->* TO <li_data>.
  <li_result> = <li_data>.

ENDFORM.                    "screen_select_exec

*&---------------------------------------------------------------------*
*&      Form  screen_select_pretty
*&---------------------------------------------------------------------*
FORM screen_select_pretty CHANGING li_program TYPE rswsourcet.
  DATA: lo_pretty_printer TYPE REF TO cl_sedi_pretty_printer.
  DATA: lx_pretty_printer TYPE REF TO cx_sedi_pretty_printer.

  TRY.
      CREATE OBJECT lo_pretty_printer. "I feel pretty, oh so pretty.
      lo_pretty_printer->format_source( CHANGING c_source = li_program[] ). "I feel pretty and witty and bright!
    CATCH cx_sedi_pretty_printer INTO lx_pretty_printer.
      MESSAGE lx_pretty_printer TYPE 'E'. "And I pity anybody who isn't me tonight
  ENDTRY.
ENDFORM.                    "screen_select_pretty
*&---------------------------------------------------------------------*
*&      Form  screen_distro_disp
*&---------------------------------------------------------------------*
FORM screen_distro_disp.
  DATA: lo_data_tab TYPE REF TO data.
  DATA: lo_result_tab TYPE REF TO data.
  DATA: li_prog_source TYPE rswsourcet.

  "Split program source from selection screen
  SPLIT s_select AT cl_abap_char_utilities=>cr_lf INTO TABLE li_prog_source.

  "Get the data
  PERFORM screen_select_exec USING  li_prog_source CHANGING lo_data_tab.

  "Compile the data into distribution table
  PERFORM screen_distro_build USING  lo_data_tab CHANGING lo_result_tab.

  "Display Distribution results
  PERFORM screen_alv USING lo_result_tab.

ENDFORM.                    "screen_distro_disp

*&---------------------------------------------------------------------*
*&      Form  screen_alv
*&---------------------------------------------------------------------*
FORM screen_alv USING lo_data_tab TYPE REF TO data.
  DATA: lo_alv           TYPE REF TO cl_salv_table.
  DATA: lo_alv_column    TYPE REF TO cl_salv_column.
  DATA: lx_alv_error     TYPE REF TO cx_root.
  FIELD-SYMBOLS: <li_data_alv> TYPE STANDARD TABLE.
  ASSIGN lo_data_tab->* TO <li_data_alv>.

  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = lo_alv CHANGING t_table = <li_data_alv> ).
      lo_alv->get_functions( )->set_all( 'X' ).
      lo_alv->get_columns( )->set_optimize( 'X' ).
      lo_alv_column = lo_alv->get_columns( )->get_column( ck_data_name-f01 ).
      lo_alv_column->set_short_text( 'Value' ) ##NO_TEXT.
      lo_alv->display( ).
    CATCH cx_salv_msg cx_salv_not_found INTO lx_alv_error.
      MESSAGE lx_alv_error TYPE 'E'.
  ENDTRY.
ENDFORM.                    "screen_alv
*&---------------------------------------------------------------------*
*&      Form  screen_distro_build
*&---------------------------------------------------------------------*
FORM screen_distro_build USING lo_data_tab   TYPE REF TO data
                      CHANGING lo_result_tab TYPE REF TO data.
  DATA: lv_data_total TYPE i,
        lv_data_index TYPE i,
        lv_data_index_max TYPE i.
  DATA: lv_job_index  TYPE i.
  DATA: lv_job_total  TYPE i.
  DATA: lv_job_total_foreground  TYPE i.
  DATA: lv_job_maxrow TYPE i.
  DATA: lv_job_num_background TYPE n LENGTH 3.
  DATA: lv_job_num_foreground TYPE n LENGTH 3.
  DATA: lv_job_name TYPE btcjob.
  DATA: lv_job_type TYPE wptyp.
  DATA: li_data_strcmp TYPE cl_abap_structdescr=>component_table.
  DATA: lk_data_strcmp TYPE cl_abap_structdescr=>component.
  FIELD-SYMBOLS: <li_result> TYPE STANDARD TABLE.
  FIELD-SYMBOLS: <lk_result> TYPE any.
  FIELD-SYMBOLS: <lv_result_jobname> TYPE any.
  FIELD-SYMBOLS: <lv_result_jobtype> TYPE any.
  FIELD-SYMBOLS: <li_data> TYPE STANDARD TABLE.

  "Build result structure
  " DATA
  " Process TYPE
  " JOB NUMBER
  lk_data_strcmp-name = ck_data_name-jobtype. lk_data_strcmp-type ?= cl_abap_typedescr=>describe_by_name( ck_data_type-jobtype ).  APPEND lk_data_strcmp TO li_data_strcmp.
  lk_data_strcmp-name = ck_data_name-jobname. lk_data_strcmp-type ?= cl_abap_typedescr=>describe_by_name( ck_data_type-jobname ).  APPEND lk_data_strcmp TO li_data_strcmp.
  PERFORM screen_struct_build USING li_data_strcmp CHANGING lo_result_tab.

  ASSIGN lo_data_tab->* TO <li_data>.
  ASSIGN lo_result_tab->* TO <li_result>.
  <li_result> = <li_data>.

  "Data Total Rows
  DESCRIBE TABLE <li_result> LINES lv_data_total.

  "Calculate total number of job, maximum rows per job and total foreground jobs.
  PERFORM screen_distro_calc USING lv_data_total CHANGING lv_job_total lv_job_maxrow lv_job_total_foreground.

  "Randomize
  IF s_dist03 IS NOT INITIAL.
    DATA: lo_random_int TYPE REF TO cl_abap_random_int.
    DATA: lv_random_seed TYPE i.
    DATA: lo_sort_tab    TYPE REF TO data.
    FIELD-SYMBOLS: <li_sort> TYPE STANDARD TABLE.
    FIELD-SYMBOLS: <lk_sort> TYPE any.
    FIELD-SYMBOLS: <lv_sort> TYPE any.

    "Build sort structure
    "CLEAR li_data_strcmp[].
    lk_data_strcmp-name = ck_data_name-sortkey. lk_data_strcmp-type ?= cl_abap_typedescr=>describe_by_name( ck_data_type-sortkey ). APPEND lk_data_strcmp TO li_data_strcmp.
    PERFORM screen_struct_build USING li_data_strcmp CHANGING lo_sort_tab.

    ASSIGN lo_sort_tab->* TO <li_sort>.

    <li_sort> = <li_data>.

    lv_random_seed = sy-timlo.
    lo_random_int = cl_abap_random_int=>create( min = '1' max = lv_data_total seed = lv_random_seed ).
    LOOP AT <li_sort> ASSIGNING <lk_sort>.
      ASSIGN COMPONENT ck_data_name-sortkey OF STRUCTURE <lk_sort> TO <lv_sort>.
      <lv_sort> = lo_random_int->get_next( ).
    ENDLOOP.
    SORT <li_sort> BY (ck_data_name-sortkey).

    "Move sorted data
    <li_result> = <li_sort>. "Omit last sorting key.
  ENDIF.

  "Default Job Name to program name if it's empty
  IF s_jobnam IS INITIAL. s_jobnam = s_prgrpt. ENDIF.


  DO lv_job_total TIMES.
    CLEAR: lv_job_name, lv_job_type.

    "Determine if it should go foreground or background
    lv_job_index = sy-index.
    IF lv_job_index LE lv_job_total_foreground.
      lv_job_num_foreground = lv_job_num_foreground + 1.
      lv_job_name = |Dialog Process | && lv_job_num_foreground.
      lv_job_type = ck_job_type-foreground.
    ELSE.
      lv_job_num_background = lv_job_num_background + 1.
      lv_job_name = s_jobnam && | | && lv_job_num_background.
      lv_job_type = ck_job_type-background.
    ENDIF.

    IF s_dist01 IS NOT INITIAL.
      lv_data_index = sy-index.
      WHILE lv_data_index LE lv_data_total.
        READ TABLE <li_result> ASSIGNING <lk_result> INDEX lv_data_index.
        CHECK sy-subrc = 0.
        lv_data_index = lv_data_index + lv_job_total.
        ASSIGN COMPONENT ck_data_name-jobname OF STRUCTURE <lk_result> TO <lv_result_jobname>.
        <lv_result_jobname> = lv_job_name.
        ASSIGN COMPONENT ck_data_name-jobtype OF STRUCTURE <lk_result> TO <lv_result_jobtype>.
        <lv_result_jobtype> = lv_job_type.
      ENDWHILE.
    ELSE.
      IF lv_data_index IS INITIAL. lv_data_index = sy-index. ENDIF.
      lv_data_index_max = lv_data_index + lv_job_maxrow - 1.
      IF lv_data_index_max GT lv_data_total. lv_data_index_max = lv_data_total. ENDIF.
      IF lv_data_index GT lv_data_total. EXIT. ENDIF.

      WHILE lv_data_index LE lv_data_index_max.
        READ TABLE <li_result> ASSIGNING <lk_result> INDEX lv_data_index.
        CHECK sy-subrc = 0.
        lv_data_index = lv_data_index + 1.
        ASSIGN COMPONENT ck_data_name-jobname OF STRUCTURE <lk_result> TO <lv_result_jobname>.
        <lv_result_jobname> = lv_job_name.
        ASSIGN COMPONENT ck_data_name-jobtype OF STRUCTURE <lk_result> TO <lv_result_jobtype>.
        <lv_result_jobtype> = lv_job_type.
      ENDWHILE.
    ENDIF.
  ENDDO.

  SORT <li_result> BY (ck_data_name-jobtype).
ENDFORM.                    "screen_distro_build
*&---------------------------------------------------------------------*
*&      Form  screen_struct_build
*&---------------------------------------------------------------------*
FORM screen_struct_build USING li_result_strcmp TYPE cl_abap_structdescr=>component_table
                      CHANGING lo_result_tab TYPE REF TO data.
  DATA: lo_data_fld    TYPE REF TO data.
  DATA: lv_field_typ   TYPE c.
  DATA: lv_field_len   TYPE i.
  DATA: lv_field_dec   TYPE i.
  DATA: lo_data_strtyp TYPE REF TO cl_abap_structdescr.
  DATA: lo_data_tabtyp TYPE REF TO cl_abap_tabledescr.
  DATA: li_data_strcmp TYPE cl_abap_structdescr=>component_table.
  DATA: lk_data_strcmp TYPE cl_abap_structdescr=>component.

  "Get the field information and create dynamic program based on the types and length
  PERFORM screen_prgfxx_fieldinfo USING s_prgf01 CHANGING lv_field_typ lv_field_len lv_field_dec.
  CASE lv_field_typ.
    WHEN 'D' OR 'F' OR 'I'.
      CREATE DATA lo_data_fld TYPE (lv_field_typ).
    WHEN 'C' OR 'N' OR 'X'.
      CREATE DATA lo_data_fld TYPE (lv_field_typ) LENGTH lv_field_len.
    WHEN 'P'.
      CREATE DATA lo_data_fld TYPE p LENGTH lv_field_len DECIMALS lv_field_dec.
  ENDCASE.

  "Build local structure
  lk_data_strcmp-name = ck_data_name-f01.
  lk_data_strcmp-type ?= cl_abap_typedescr=>describe_by_data_ref( lo_data_fld ).
  APPEND lk_data_strcmp TO li_data_strcmp.
  APPEND LINES OF li_result_strcmp TO li_data_strcmp.
  lo_data_strtyp = cl_abap_structdescr=>create( p_components = li_data_strcmp p_strict = space ).
  lo_data_tabtyp = cl_abap_tabledescr=>create( lo_data_strtyp ).
  CREATE DATA lo_result_tab TYPE HANDLE lo_data_tabtyp.
ENDFORM.                    "screen_struct_build

*&---------------------------------------------------------------------*
*&      Form  screen_handle
*&---------------------------------------------------------------------*
FORM screen_handle.
  DATA lk_screen TYPE screen.
  DATA lv_prgper_type TYPE grp3_____4.
  LOOP AT screen INTO lk_screen.
    CASE lk_screen-name.
      WHEN 'S_JOBCN2' OR 'S_JOBTY1'  OR 'S_JOBCNT'.
        PERFORM screen_prgfxx_fieldtype USING s_prgf01 CHANGING lv_prgper_type.
        IF lv_prgper_type = 'PAR'.
          s_jobcn2 = '1'.
          s_jobty1 = ' '.
          s_jobty2 = 'X'.
          lk_screen-input = '0'. MODIFY screen FROM lk_screen.
        ELSE.
          lk_screen-input = '1'. MODIFY screen FROM lk_screen.
        ENDIF.
    ENDCASE.
  ENDLOOP.
  PERFORM screen_check.
ENDFORM.                    "screen_handle

*&---------------------------------------------------------------------*
*&      Form  process_data_sendfg01 Before RFC
*&---------------------------------------------------------------------*
FORM process_data_sendfg01 USING lk_rfc_imp TYPE spta_t_before_rfc_imp     ##NEEDED
                        CHANGING lk_rfc_exp TYPE spta_t_before_rfc_exp     ##NEEDED
                                li_rfc_data TYPE spta_t_indxtab            ##NEEDED
                                lk_fail_obj TYPE spta_t_failed_objects     ##NEEDED
                                lk_proc_obj TYPE spta_t_objects_in_process ##NEEDED
                                li_data     TYPE gti_data                  ##CALLED.
  FIELD-SYMBOLS <lk_data> TYPE gtk_data.

  "Call RFC only when we have data to process
  IF li_data[] IS INITIAL.
    CLEAR lk_rfc_exp-start_rfc.
    RETURN.
  ELSE.
    lk_rfc_exp-start_rfc = 'X'.
  ENDIF.

  "Get first entry
  READ TABLE li_data ASSIGNING <lk_data> INDEX 1.

  "Encode it
  CALL FUNCTION 'SPTA_INDX_PACKAGE_ENCODE'
    EXPORTING
      data    = <lk_data>
    IMPORTING
      indxtab = li_rfc_data.

  "Remove it from entry
  DELETE li_data INDEX 1.
ENDFORM.                    "process_data_sendfg01

*&---------------------------------------------------------------------*
*&      Form  process_data_sendfg02 In RFC
*&---------------------------------------------------------------------*
FORM process_data_sendfg02 USING lk_rfc_imp  TYPE spta_t_in_rfc_imp ##NEEDED
                        CHANGING lk_rfc_exp  TYPE spta_t_in_rfc_exp ##NEEDED
                                 li_rfc_data TYPE spta_t_indxtab    ##CALLED.
  DATA lk_data TYPE gtk_data.
  SET UPDATE TASK LOCAL.

  "Decode the content
  CALL FUNCTION 'SPTA_INDX_PACKAGE_DECODE'
    EXPORTING
      indxtab = li_rfc_data
    IMPORTING
      data    = lk_data.

  "Call the program
  SUBMIT (lk_data-progname) TO SAP-SPOOL SPOOL PARAMETERS lk_data-print_param ARCHIVE PARAMETERS lk_data-archive_param WITHOUT SPOOL DYNPRO
    WITH SELECTION-TABLE lk_data-variant_param AND RETURN.

ENDFORM.                    "process_data_sendfg02
*&---------------------------------------------------------------------*
*&      Form  process_data_sendfg03 After RFC
*&---------------------------------------------------------------------*
FORM process_data_sendfg03  USING li_rfc_data   TYPE spta_t_indxtab            ##NEEDED
                                  lv_rfc_subrc  TYPE sy-subrc                  ##NEEDED
                                  lk_rfc_msg    TYPE spta_t_rfcmsg             ##NEEDED
                                  lk_proc_obj   TYPE spta_t_objects_in_process ##NEEDED
                                  lk_rfc_imp    TYPE spta_t_after_rfc_imp      ##NEEDED
                         CHANGING lk_rfc_exp    TYPE spta_t_after_rfc_exp      ##NEEDED
                                  lo_user_param TYPE any                       ##CALLED.
ENDFORM.                    "process_data_sendfg03
*&---------------------------------------------------------------------*
*&      Form  screen_server_info
*&---------------------------------------------------------------------*
FORM screen_server_info.
  DATA lv_message TYPE string.
  DATA lv_total_dia TYPE i.
  DATA lv_total_btc TYPE i.
  PERFORM screen_server_total CHANGING lv_total_dia lv_total_btc.
  lv_message = |Total Dialog/Background Processes: | && lv_total_dia && | / | && lv_total_btc.
  MESSAGE lv_message TYPE 'I'.
ENDFORM.                    "screen_server_info
*&---------------------------------------------------------------------*
*&      Form  screen_server_total
*&---------------------------------------------------------------------*
FORM screen_server_total CHANGING lv_total_dia TYPE i
                                 lv_total_btc TYPE i.
  DATA li_servers TYPE STANDARD TABLE OF msxxlist.
  DATA li_wpinfo TYPE STANDARD TABLE OF wpinfo.
  DATA li_wpinfo_dia TYPE STANDARD TABLE OF wpinfo.
  DATA li_wpinfo_btc TYPE STANDARD TABLE OF wpinfo.
  FIELD-SYMBOLS: <lk_server> TYPE msxxlist.

  "Get all servers
  CALL FUNCTION 'TH_SERVER_LIST'
    TABLES
      list           = li_servers
    EXCEPTIONS
      no_server_list = 1
      OTHERS         = 2.
  IF sy-subrc = 0.

    "Get all process types
    LOOP AT li_servers ASSIGNING <lk_server>.
      CLEAR: li_wpinfo[], li_wpinfo_dia[], li_wpinfo_btc[].
      CALL FUNCTION 'TH_WPINFO'
        EXPORTING
          srvname    = <lk_server>-name
        TABLES
          wplist     = li_wpinfo
        EXCEPTIONS
          send_error = 1
          OTHERS     = 2.
      CHECK sy-subrc = 0.

      "Find out total BTC
      li_wpinfo_btc[] = li_wpinfo[].
      DELETE li_wpinfo_btc WHERE wp_typ NE ck_job_type-background.
      lv_total_btc = lv_total_btc + lines( li_wpinfo_btc ).

      "Find out total DIA
      li_wpinfo_dia[] = li_wpinfo[].
      DELETE li_wpinfo_dia WHERE wp_typ NE ck_job_type-foreground.
      lv_total_dia = lv_total_dia + lines( li_wpinfo_dia ).
    ENDLOOP.
  ENDIF.
ENDFORM.                    "screen_server_total
*&---------------------------------------------------------------------*
*&      Form  screen_output
*&---------------------------------------------------------------------*
FORM screen_output.
  DATA: lk_function_code  TYPE smp_dyntxt.
  s_b01    = |Select Program, Field and Variant|.
  s_b01_01 = |Program Name|.
  s_b01_02 = |Program Field Name|.
  s_b01_03 = |Program Variant|.
  s_b02    = |Selection Logic|.
  s_b02_01 = |@9U@ Selection Logic|.
  s_b02_02 = |@XD@ Display Selection|.
  s_b03    = |Distribution Options|.
  s_b03_01 = |Equal/Round Robin Distribution|.
  s_b03_02 = |First in First Out|.
  s_b03_03 = |Randomized|.
  s_b04    = |Parallel Processing Options|.
  s_b04_01 = |Background Jobs|.
  s_b04_02 = |Max row(s) per job|.
  s_b04_03 = |Wait for split jobs to complete before ending main job|.
  s_b04_04 = |Error out on the main job if any split job errors|.
  s_b04_05 = |Max time for waiting|.
  s_b04_06 = |No of Dialog/Background Jobs|.
  s_b04_07 = |Split Job Title|.
  s_b04_08 = |Dialog/Foreground Jobs|.
  s_b04_09 = |Server Group|.
  s_b04_10 = |Maximum Concurent Tasks|.
  s_b04_11 = | / |.
  s_b04_12 = |@GA@ Server Info|.
  s_b05    = |Display Data|.
  s_b05_01 = |@12@ Display Data|.
  s_b06    = |Spool options|.
  s_b06_01 = |Spool name|.
  s_b06_02 = |Combine to single spool|.
  lk_function_code-icon_id    =  icon_test.
  lk_function_code-text       =  |Display Data|.
  lk_function_code-quickinfo  =  |Display Data|.
  sscrfields-functxt_01 = lk_function_code.
ENDFORM.                    "screen_output

*&---------------------------------------------------------------------*
*&      Form  screen_distro_calc
*&---------------------------------------------------------------------*
FORM screen_distro_calc  USING lv_data_total TYPE i
                         CHANGING lv_job_total TYPE i
                                  lv_job_maxrow TYPE i
                                  lv_job_total_foreground TYPE i.
  DATA: lv_field_type TYPE grp3_____4.
  DATA: lv_temp TYPE p DECIMALS 2.
  DATA: lv_process_total TYPE i.
  DATA: lv_process_total_foreground TYPE i.
  DATA: lv_process_total_background TYPE i.

  "Get field type to calculate total Jobs & total rows per job
  PERFORM screen_prgfxx_fieldtype USING s_prgf01 CHANGING lv_field_type.
  CASE lv_field_type.
    WHEN 'LOW'.
      "Calculate by maximum number of jobs
      IF s_jobty1 IS NOT INITIAL.
        lv_job_total_foreground = s_procfg.
        lv_job_total = s_procfg + s_procbg.
        lv_temp = lv_data_total / lv_job_total.
        lv_job_maxrow = ceil( lv_temp ).
      ENDIF.

      "Calculate by maximum number of rows per job. foreground / background ratio will use the available ratio from the server total
      IF s_jobty2 IS NOT INITIAL.
        PERFORM screen_server_total CHANGING lv_process_total_foreground lv_process_total_background.
        lv_process_total = ( lv_process_total_foreground + lv_process_total_background ).
        lv_temp = lv_data_total / s_jobcn2.
        lv_job_total = ceil( lv_temp ).
        lv_job_total_foreground = lv_job_total * lv_process_total_foreground / lv_process_total.
        lv_job_maxrow = s_jobcn2.
      ENDIF.

    WHEN 'PAR'.
      "Type parameters would be forced to use a max row of 1.
      lv_job_total = lv_data_total.
      lv_job_maxrow = 1.
      PERFORM screen_server_total CHANGING lv_process_total_foreground lv_process_total_background.
      lv_process_total = ( lv_process_total_foreground + lv_process_total_background ).
      lv_job_total_foreground = lv_job_total * lv_process_total_foreground / lv_process_total.
  ENDCASE.
ENDFORM.                    " screen_distro_bymaxproc
*&---------------------------------------------------------------------*
*&      Form  screen_check
*&---------------------------------------------------------------------*
FORM screen_check.
  DATA lk_screen TYPE screen.
  DATA lv_error_jobrfc TYPE c.
  "Server Group required if you're using foreground processing
  IF s_procfg GT 0 AND s_jobty1 EQ 'X' AND s_jobrfc IS INITIAL.
    lv_error_jobrfc = 'X'.
  ENDIF.
  IF s_jobty2 EQ 'X' AND s_jobrfc IS INITIAL.
    lv_error_jobrfc = 'X'.
  ENDIF.
  IF lv_error_jobrfc IS NOT INITIAL.
    LOOP AT screen INTO lk_screen.
      CASE lk_screen-name.
        WHEN 'S_JOBRFC'.
          lk_screen-required = '1'. MODIFY screen FROM lk_screen.
      ENDCASE.
    ENDLOOP.
    MESSAGE 'Server Group is required' TYPE 'E' ##NO_TEXT.
  ENDIF.
ENDFORM.                    "screen_check
