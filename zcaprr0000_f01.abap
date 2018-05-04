*&---------------------------------------------------------------------*
*&      Form  DISABLE_MODID_B20
*&---------------------------------------------------------------------*
*  Disable the input data for the Modify ID B20
*----------------------------------------------------------------------*
FORM disable_modid_b20.

  LOOP AT SCREEN.

    CHECK screen-group1 EQ 'B20'.

    screen-input = 0.
    MODIFY SCREEN.

  ENDLOOP.

ENDFORM.                    " DISABLE_MODID_B20

*&---------------------------------------------------------------------*
*&      Form  READ_MASTER_LANGUAGE
*&---------------------------------------------------------------------*
*  Read the object master language
*----------------------------------------------------------------------*
FORM read_master_language.

  SET LOCALE LANGUAGE 'E'.
  TRANSLATE p_obj TO UPPER CASE.
  SET LOCALE LANGUAGE sy-langu.

  CASE abap_true.
    WHEN rb_prog.

      SELECT SINGLE rload
      FROM trdir
      INTO p_langu
      WHERE name EQ p_obj.

    WHEN rb_func.

      SELECT SINGLE prog~rload
      FROM tfdir AS func
      INNER JOIN trdir AS prog
      ON func~pname EQ prog~name
      INTO p_langu
      WHERE func~funcname EQ p_obj.

    WHEN rb_dtel.

      SELECT SINGLE dtelmaster
      FROM dd04l
      INTO p_langu
      WHERE rollname EQ p_obj
        AND as4local EQ 'A'.

    WHEN rb_strc.

      SELECT SINGLE masterlang
      FROM tadir
      INTO p_langu
      WHERE pgmid    EQ 'R3TR'
        AND object   EQ 'TABL'
        AND obj_name EQ p_obj.

    WHEN rb_fugr.

      SELECT SINGLE masterlang
      FROM tadir
      INTO p_langu
      WHERE pgmid    EQ 'R3TR'
        AND object   EQ 'FUGR'
        AND obj_name EQ p_obj.

    WHEN rb_doma.

      SELECT SINGLE masterlang
      FROM tadir
      INTO p_langu
      WHERE pgmid    EQ 'R3TR'
        AND object   EQ 'DOMA'
        AND obj_name EQ p_obj.

    WHEN rb_shlp.

      SELECT SINGLE masterlang
      FROM tadir
      INTO p_langu
      WHERE pgmid    EQ 'R3TR'
        AND object   EQ 'SHLP'
        AND obj_name EQ p_obj.

    WHEN rb_enqu.

      SELECT SINGLE masterlang
      FROM tadir
      INTO p_langu
      WHERE pgmid    EQ 'R3TR'
        AND object   EQ 'ENQU'
        AND obj_name EQ p_obj.

    WHEN rb_clas.

      SELECT SINGLE masterlang
      FROM tadir
      INTO p_langu
      WHERE pgmid    EQ 'R3TR'
        AND object   EQ 'CLAS'
        AND obj_name EQ p_obj.

    WHEN rb_mess.

      SELECT SINGLE masterlang
      FROM tadir
      INTO p_langu
      WHERE pgmid    EQ 'R3TR'
        AND object   EQ 'MSAG'
        AND obj_name EQ p_obj.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.                                                    " READ_MASTER_LANGUAGE

*&---------------------------------------------------------------------*
*&      Form  RADIOBUTTON_RB11
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM radiobutton_rb11.

  DATA: wl_object TYPE ty_objects.

  CLEAR t_objects.

  CASE abap_true.
    WHEN rb_prog.
      wl_object-type    = 'RPT4'.
      wl_object-tr_type = 'PROG'.
      APPEND wl_object TO t_objects.
    WHEN rb_func.
      wl_object-type    = 'FNC1'.
      wl_object-tr_type = 'FUNC'.
      APPEND wl_object TO t_objects.
    WHEN rb_tran.
      wl_object-type    = 'TRAN'.
      wl_object-tr_type = 'TRAN'.
      APPEND wl_object TO t_objects.
    WHEN rb_dtel.
      wl_object-type    = 'DTEL'.
      wl_object-tr_type = 'DTEL'.
      APPEND wl_object TO t_objects.
    WHEN rb_strc.
      wl_object-type    = 'TABT'.
      wl_object-tr_type = 'TABL'.
      APPEND wl_object TO t_objects.
    WHEN rb_fugr.
      wl_object-type    = 'RPT1'.
      wl_object-tr_type = 'FUGR'.
      APPEND wl_object TO t_objects.
    WHEN rb_doma.
      wl_object-type    = 'DOMA'. " Descriptions
      wl_object-tr_type = 'DOMA'.
      APPEND wl_object TO t_objects.
    WHEN rb_shlp.
      wl_object-type    = 'SHLP'.
      wl_object-tr_type = 'SHLP'.
      APPEND wl_object TO t_objects.
    WHEN rb_enqu.
      wl_object-type    = 'ENQU'.
      wl_object-tr_type = 'ENQU'.
      APPEND wl_object TO t_objects.
    WHEN rb_clas.
      wl_object-type    = 'CLAS'. " Descriptions
      wl_object-tr_type = 'CLAS'.
      APPEND wl_object TO t_objects.
    WHEN rb_mess.
      wl_object-type    = 'MSAG'.
      wl_object-tr_type = 'MSAG'.
      APPEND wl_object TO t_objects.
    WHEN rb_cuad.
      wl_object-type    = 'CA4'.
      wl_object-tr_type = 'CUAD'.
      APPEND wl_object TO t_objects.
    WHEN OTHERS.
  ENDCASE.

  PERFORM set_objnam.

ENDFORM.                    " RADIOBUTTON_RB11
*&---------------------------------------------------------------------*
*&      Form  FILL_LANG_LISTBOX
*&---------------------------------------------------------------------*
* Fill the listbox with the installed languages
*----------------------------------------------------------------------*
FORM fill_lang_listbox.

  DATA: tl_installed TYPE STANDARD TABLE OF rmsaw_t002c,
        tl_list      TYPE STANDARD TABLE OF vrm_value.

  DATA: wl_installed TYPE rmsaw_t002c,
        wl_list      TYPE vrm_value.

  SELECT *
  FROM rmsaw_t002c
  INTO TABLE tl_installed
  WHERE spras EQ sy-langu.

  LOOP AT tl_installed INTO wl_installed.

    wl_list-key  = wl_installed-sprsl.
    wl_list-text = wl_installed-sptxt.
    APPEND wl_list TO tl_list.

  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'P_DEST'
      values          = tl_list
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " FILL_LANG_LISTBOX

*&---------------------------------------------------------------------*
*&      Form  READ_ISO_LANGU
*&---------------------------------------------------------------------*
FORM read_iso_langu  USING    pv_r3   TYPE langu
                     CHANGING pv_iso  TYPE lxeisolang.

  CALL FUNCTION 'LXE_T002_CHECK_LANGUAGE'
    EXPORTING
      r3_lang    = pv_r3
    IMPORTING
      o_language = pv_iso
    EXCEPTIONS
      OTHERS     = 0.

ENDFORM.                    " READ_ISO_LANGU

*&---------------------------------------------------------------------*
*&      Form  ANALYZE_TRANSLATIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM analyze_translations.

  DATA: tl_texts     TYPE ty_t_pcx_s1,
        tl_proposals TYPE ty_t_pcx_s2.

  DATA: wl_texts     TYPE LINE OF ty_t_pcx_s1,
        wl_proposals TYPE LINE OF ty_t_pcx_s2,
        wl_output    TYPE ty_output,
        wl_object    TYPE ty_objects.

  DATA: wl_text_pair_read TYPE ty_text_pair_read.

  LOOP AT t_objects INTO wl_object.

    PERFORM read_texts    USING wl_object
                       CHANGING tl_texts
                                wl_text_pair_read.

    PERFORM read_proposals USING wl_text_pair_read
                                 tl_texts
                                 wl_object
                        CHANGING tl_proposals.

    LOOP AT tl_texts INTO wl_texts.

      MOVE-CORRESPONDING wl_texts TO wl_output.

      READ TABLE tl_proposals
      INTO wl_proposals
      WITH KEY textkey = wl_output-textkey.

      IF sy-subrc EQ 0.
        wl_output-stattrn = wl_proposals-stattrn.
      ENDIF.

*** T	Translated
*** M	Modified
*** N	New
*** D	Deleted

      CASE wl_output-stattrn.
        WHEN 'T'.
          wl_output-icon = icon_led_green.
        WHEN 'M'.
          wl_output-icon = icon_led_yellow.
        WHEN OTHERS.
          wl_output-icon = icon_led_red.
      ENDCASE.

      APPEND wl_output TO t_output.
      CLEAR wl_output.

    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " ANALYZE_TRANSLATIONS

*&---------------------------------------------------------------------*
*&      Form  READ_TEXTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_TL_TEXTS  text
*----------------------------------------------------------------------*
FORM read_texts  USING pw_object         TYPE ty_objects
              CHANGING pt_texts          TYPE ty_t_pcx_s1
                       pw_text_pair_read TYPE ty_text_pair_read.

  CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_READ'
    EXPORTING
      t_lang    = p_dstlan
      s_lang    = p_scrlan
      custmnr   = '999999'
      objtype   = pw_object-type
      objname   = pw_object-objnam
    IMPORTING
      domatyp   = pw_text_pair_read-domatyp
      domanam   = pw_text_pair_read-domanam
    TABLES
      lt_pcx_s1 = pt_texts.

ENDFORM.                    " READ_TEXTS

*&---------------------------------------------------------------------*
*&      Form  READ_PROPOSALS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WL_TEXT_PAIR_READ  text
*      <--P_TL_PROPOSALS  text
*----------------------------------------------------------------------*
FORM read_proposals  USING    pw_text_pair_read TYPE ty_text_pair_read
                              pt_texts          TYPE ty_t_pcx_s1
                              pw_object         TYPE ty_objects
                     CHANGING pt_proposals      TYPE ty_t_pcx_s2.

  CALL FUNCTION 'LXE_PP1_PROPOSALS_GET_SE63'
    EXPORTING
      s_lang   = p_scrlan
      t_lang   = p_dstlan
      custmnr  = '999999'
      objtype  = pw_object-type
      domatyp  = pw_text_pair_read-domatyp
      domanam  = pw_text_pair_read-domanam
    TABLES
      t_pcx_s1 = pt_texts
      t_pcx_s2 = pt_proposals.

ENDFORM.                    " READ_PROPOSALS
*&---------------------------------------------------------------------*
*&      Form  SHOW_RESULT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM show_result.

  DATA: tl_fieldcat TYPE lvc_t_fcat.

  DATA: wl_layout   TYPE lvc_s_layo.

  PERFORM fieldcat CHANGING tl_fieldcat.

  wl_layout-zebra      = abap_true.
  wl_layout-cwidth_opt = abap_true.

  IF o_alv IS BOUND.
    o_alv->free( ).
    CLEAR o_alv.
  ENDIF.

  IF o_cc IS BOUND.
    o_cc->free( ).
    CLEAR o_cc.
  ENDIF.

  CREATE OBJECT o_cc
    EXPORTING
      container_name              = 'CC_0100'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CREATE OBJECT o_alv
    EXPORTING
      i_parent          = o_cc
    EXCEPTIONS
      error_cntl_create = 1
      error_cntl_init   = 2
      error_cntl_link   = 3
      error_dp_create   = 4
      OTHERS            = 5.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL METHOD o_alv->set_table_for_first_display
    EXPORTING
      is_layout                     = wl_layout
    CHANGING
      it_outtab                     = t_output
      it_fieldcatalog               = tl_fieldcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " SHOW_RESULT

*&---------------------------------------------------------------------*
*&      Form  FIELDCAT
*&---------------------------------------------------------------------*
FORM fieldcat  CHANGING pt_fieldcat TYPE lvc_t_fcat.

  DATA: wl_fieldcat TYPE lvc_s_fcat.

  wl_fieldcat-fieldname = 'ICON'.
  wl_fieldcat-tabname   = 'T_OUTPUT'.
  wl_fieldcat-icon      = abap_true.
  APPEND wl_fieldcat TO pt_fieldcat.
  CLEAR wl_fieldcat.

  wl_fieldcat-fieldname = 'STATTRN'.
  wl_fieldcat-tabname   = 'T_OUTPUT'.
  wl_fieldcat-ref_field = 'STATTRN'.
  wl_fieldcat-ref_table = 'LXE_PCX_S2'.
  APPEND wl_fieldcat TO pt_fieldcat.
  CLEAR wl_fieldcat.

  wl_fieldcat-fieldname = 'TEXTKEY'.
  wl_fieldcat-tabname   = 'T_OUTPUT'.
  wl_fieldcat-ref_table = 'LXE_PCX_S1'.
  APPEND wl_fieldcat TO pt_fieldcat.
  CLEAR wl_fieldcat.

  wl_fieldcat-fieldname = 'S_TEXT'.
  wl_fieldcat-tabname   = 'T_OUTPUT'.
  wl_fieldcat-coltext   = text-f01.
  APPEND wl_fieldcat TO pt_fieldcat.
  CLEAR wl_fieldcat.

  wl_fieldcat-fieldname = 'T_TEXT'.
  wl_fieldcat-tabname   = 'T_OUTPUT'.
  wl_fieldcat-coltext   = text-f02.
  APPEND wl_fieldcat TO pt_fieldcat.

ENDFORM.                    " FIELDCAT

*&---------------------------------------------------------------------*
*&      Form  SET_OBJNAM
*&---------------------------------------------------------------------*
* Set the object name for the translation system
*----------------------------------------------------------------------*
FORM set_objnam.

  TYPES: BEGIN OF tyl_function,
           funcname TYPE tfdir-funcname,
           pname    TYPE tfdir-pname,
         END OF tyl_function.

  DATA: tl_subobjects TYPE STANDARD TABLE OF ty_objects.

  DATA: wl_function TYPE tyl_function.

  FIELD-SYMBOLS: <fsl_object> TYPE ty_objects.

  LOOP AT t_objects ASSIGNING <fsl_object>.

    CASE <fsl_object>-type.
      WHEN 'FNC1'.

        SELECT SINGLE funcname pname
        FROM tfdir
        INTO wl_function
        WHERE funcname EQ p_obj.

        wl_function-pname = wl_function-pname+4.
        <fsl_object>-objnam = wl_function.

      WHEN OTHERS.
        <fsl_object>-objnam = p_obj.
    ENDCASE.

    PERFORM fill_subobjects USING <fsl_object>-type
                                  <fsl_object>-objnam
                         CHANGING tl_subobjects.

  ENDLOOP.

  APPEND LINES OF tl_subobjects TO t_objects.

ENDFORM.                    " SET_OBJNAM

*&---------------------------------------------------------------------*
*&      Form  FILL_SUBOBJECTS
*&---------------------------------------------------------------------*
* Fill the subobjects like Domain --> Fixed Values ; Message Class --> Messages
*----------------------------------------------------------------------*
*      -->PV_TYPE        Master object type
*      -->PV_OBJECT      Master object name
*      <--PT_SUBOBJECTS  Subobjects table
*----------------------------------------------------------------------*
FORM fill_subobjects  USING    pv_type       TYPE ty_objects-type
                               pv_object     TYPE ty_objects-objnam
                      CHANGING pt_subobjects TYPE ty_t_objects.

  CASE pv_type.
    WHEN 'DOMA'.
      PERFORM fill_doma_subobjects USING pv_object
                                CHANGING pt_subobjects.
    WHEN 'MSAG'.
      PERFORM fill_msag_subobjects USING pv_object
                                CHANGING pt_subobjects.
    WHEN 'CLAS'.
      PERFORM fill_class_subobjects USING pv_object
                                 CHANGING pt_subobjects.
  ENDCASE.

ENDFORM.                    " FILL_SUBOBJECTS

*&---------------------------------------------------------------------*
*&      Form  FILL_DOMA_SUBOBJECTS
*&---------------------------------------------------------------------*
*  Fill Domain subobjects
*----------------------------------------------------------------------*
FORM fill_doma_subobjects  USING    pv_domain     TYPE ty_objects-objnam
                           CHANGING pt_subobjects TYPE ty_t_objects.

  DATA: wl_object TYPE ty_objects.

  wl_object-type    = 'VALU'. " Fixed-Values
  wl_object-tr_type = 'DOMA'.
  wl_object-objnam  = pv_domain.
  APPEND wl_object TO pt_subobjects.

ENDFORM.                    " FILL_DOMA_SUBOBJECTS

*&---------------------------------------------------------------------*
*&      Form  FILL_CLASS_SUBOBJECTS
*&---------------------------------------------------------------------*
*  Fill Classes subobjects
*----------------------------------------------------------------------*
FORM fill_class_subobjects USING    pv_class      TYPE ty_objects-objnam
                           CHANGING pt_subobjects TYPE ty_t_objects.

  DATA: wl_object TYPE ty_objects.

  wl_object-type    = 'RPT8'.  " Text-pool
  wl_object-tr_type = 'CLAS'.
  wl_object-objnam  = pv_class.
  APPEND wl_object TO pt_subobjects.

ENDFORM.                    " FILL_CLASS_SUBOBJECTS

*&---------------------------------------------------------------------*
*&      Form  FILL_MSAG_SUBOBJECTS
*&---------------------------------------------------------------------*
*  Fill Messages in a message class and doku like subobjects
*----------------------------------------------------------------------*
*      -->PV_MESSAGE     Message Class
*      <--PT_SUBOBJECTS  text
*----------------------------------------------------------------------*
FORM fill_msag_subobjects  USING    pv_message     TYPE ty_objects-objnam
                           CHANGING pt_subobjects  TYPE ty_t_objects.

  TYPES: BEGIN OF tyl_t100,
           sprsl  TYPE t100-sprsl,
           arbgb  TYPE t100-arbgb,
           msgnr  TYPE t100-msgnr,
         END OF tyl_t100.

  DATA: tl_t100  TYPE STANDARD TABLE OF tyl_t100.

  DATA: wl_t100   TYPE tyl_t100,
        wl_object TYPE ty_objects.

  CHECK p_langu IS NOT INITIAL.

  SELECT sprsl
         arbgb
         msgnr
  FROM t100
  INTO TABLE tl_t100
  WHERE sprsl EQ p_langu
    AND arbgb EQ pv_message.

  LOOP AT tl_t100 INTO wl_t100.

    CLEAR: wl_object.

    wl_object-type    = 'MESS'.
    wl_object-tr_type = 'MESS'.

    CONCATENATE wl_t100-arbgb
                wl_t100-msgnr
    INTO wl_object-objnam
    RESPECTING BLANKS.

    APPEND wl_object TO pt_subobjects.

  ENDLOOP.

ENDFORM.                    " FILL_MSAG_SUBOBJECTS

*&---------------------------------------------------------------------*
*&      Form  OK_TRANSPORT
*&---------------------------------------------------------------------*
*  Add object translation to transport request
*----------------------------------------------------------------------*
FORM ok_transport.

  DATA: vl_task  TYPE e070-trkorr.

  PERFORM get_task CHANGING vl_task.
  PERFORM add_object_to_task USING vl_task.

ENDFORM.                    "OK_TRANSPORT

*&---------------------------------------------------------------------*
*&      Form  GET_TASK
*&---------------------------------------------------------------------*
*  Ask the transport request and return the first available task
*----------------------------------------------------------------------*
*      <--PV_TASK     Task ID
*----------------------------------------------------------------------*
FORM get_task CHANGING pv_task TYPE e070-trkorr.

  DATA: vl_transport TYPE e070-trkorr.

  PERFORM ask_transport_request CHANGING vl_transport.

  PERFORM default_task  USING vl_transport
                     CHANGING pv_task.

ENDFORM.                    "GET_TASK

*&---------------------------------------------------------------------*
*&      Form  ASK_TRANSPORT_REQUEST
*&---------------------------------------------------------------------*
*  POPUP to select Transport Request
*----------------------------------------------------------------------*
*      <--PV_TRANSPORT  Transport Request ID
*----------------------------------------------------------------------*
FORM ask_transport_request CHANGING pv_transport TYPE e070-trkorr.

  DATA: wl_request TYPE trwbo_request_header.

  CALL FUNCTION 'TR_REQUEST_CHOICE'
    EXPORTING
      iv_request_types     = 'K'
    IMPORTING
      es_request           = wl_request
    EXCEPTIONS
      invalid_request      = 1
      invalid_request_type = 2
      user_not_owner       = 3
      no_objects_appended  = 4
      enqueue_error        = 5
      cancelled_by_user    = 6
      recursive_call       = 7
      OTHERS               = 8.

  IF sy-subrc NE 0.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    RETURN.

  ENDIF.

  pv_transport = wl_request-trkorr.

ENDFORM.                    "ASK_TRANSPORT_REQUEST

*&---------------------------------------------------------------------*
*&      Form  DEFAULT_TASK
*&---------------------------------------------------------------------*
*  Return the first available task from the PV_TRANSPORT transport request
*----------------------------------------------------------------------*
*      -->PV_TRANSPORT  Transport Request
*      <--PV_TASK       Task
*----------------------------------------------------------------------*
FORM default_task  USING pv_transport TYPE e070-trkorr
                CHANGING pv_task      TYPE e070-trkorr.

  SELECT SINGLE trkorr
  FROM e070
  INTO pv_task
  WHERE strkorr    EQ pv_transport
    AND trfunction EQ 'S'
    AND trstatus   EQ 'D'
    AND as4user    EQ sy-uname.

ENDFORM.                    "DEFAULT_TASK

*&---------------------------------------------------------------------*
*&      Form  ADD_OBJECT_TO_TASK
*&---------------------------------------------------------------------*
*  Add the object P_OBJ translation into the task VL_TASK
*----------------------------------------------------------------------*
*      -->PV_TASK    Task
*----------------------------------------------------------------------*
FORM add_object_to_task USING pv_task TYPE e070-trkorr.

  DATA: tl_e071   TYPE STANDARD TABLE OF e071,
        tl_e071k  TYPE STANDARD TABLE OF e071k.

  DATA: wl_e071   TYPE e071,
        wl_object TYPE ty_objects.

  wl_e071-pgmid    = 'LANG'.
  wl_e071-lang     = p_dest.

  LOOP AT t_objects INTO wl_object.

    wl_e071-object   = wl_object-tr_type.
    wl_e071-obj_name = wl_object-objnam.
    APPEND wl_e071 TO tl_e071.

  ENDLOOP.

  CALL FUNCTION 'TR_APPEND_TO_COMM_OBJS_KEYS'
    EXPORTING
      wi_trkorr                      = pv_task
    TABLES
      wt_e071                        = tl_e071
      wt_e071k                       = tl_e071k
    EXCEPTIONS
      key_char_in_non_char_field     = 1
      key_check_keysyntax_error      = 2
      key_inttab_table               = 3
      key_longer_field_but_no_generc = 4
      key_missing_key_master_fields  = 5
      key_missing_key_tablekey       = 6
      key_non_char_but_no_generic    = 7
      key_no_key_fields              = 8
      key_string_longer_char_key     = 9
      key_table_has_no_fields        = 10
      key_table_not_activ            = 11
      key_unallowed_key_function     = 12
      key_unallowed_key_object       = 13
      key_unallowed_key_objname      = 14
      key_unallowed_key_pgmid        = 15
      key_without_header             = 16
      ob_check_obj_error             = 17
      ob_devclass_no_exist           = 18
      ob_empty_key                   = 19
      ob_generic_objectname          = 20
      ob_ill_delivery_transport      = 21
      ob_ill_lock                    = 22
      ob_ill_parts_transport         = 23
      ob_ill_source_system           = 24
      ob_ill_system_object           = 25
      ob_ill_target                  = 26
      ob_inttab_table                = 27
      ob_local_object                = 28
      ob_locked_by_other             = 29
      ob_modif_only_in_modif_order   = 30
      ob_name_too_long               = 31
      ob_no_append_of_corr_entry     = 32
      ob_no_append_of_c_member       = 33
      ob_no_consolidation_transport  = 34
      ob_no_original                 = 35
      ob_no_shared_repairs           = 36
      ob_no_systemname               = 37
      ob_no_systemtype               = 38
      ob_no_tadir                    = 39
      ob_no_tadir_not_lockable       = 40
      ob_privat_object               = 41
      ob_repair_only_in_repair_order = 42
      ob_reserved_name               = 43
      ob_syntax_error                = 44
      ob_table_has_no_fields         = 45
      ob_table_not_activ             = 46
      tr_enqueue_failed              = 47
      tr_errors_in_error_table       = 48
      tr_ill_korrnum                 = 49
      tr_lockmod_failed              = 50
      tr_lock_enqueue_failed         = 51
      tr_not_owner                   = 52
      tr_no_systemname               = 53
      tr_no_systemtype               = 54
      tr_order_not_exist             = 55
      tr_order_released              = 56
      tr_order_update_error          = 57
      tr_wrong_order_type            = 58
      ob_invalid_target_system       = 59
      tr_no_authorization            = 60
      ob_wrong_tabletyp              = 61
      ob_wrong_category              = 62
      ob_system_error                = 63
      ob_unlocal_objekt_in_local_ord = 64
      tr_wrong_client                = 65
      ob_wrong_client                = 66
      key_wrong_client               = 67
      OTHERS                         = 68.

  IF sy-subrc NE 0.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    RETURN.

  ENDIF.

  MESSAGE s003(admi_crit_aba) WITH pv_task.
* Object successfully added to request/task &1

ENDFORM.                    "ADD_OBJECT_TO_TASK


*Messages
*----------------------------------------------------------
*
* Message class: ADMI_CRIT_ABA
*003   Object successfully added to request/task &1

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.5.5 - E.G.Mellodew. 1998-2018. Sap Release 731
