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

    WHEN OTHERS.
  ENDCASE.

ENDFORM.                                                    " READ_MASTER_LANGUAGE

*&---------------------------------------------------------------------*
*&      Form  RADIOBUTTON_RB11
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM radiobutton_rb11.

  CASE abap_true.
    WHEN rb_prog.
      p_type = 'RPT4'.
    WHEN rb_func.
      p_type = 'FNC1'.
    WHEN rb_tran.
      p_type = 'TRAN'.
    WHEN rb_dtel.
      p_type = 'DTEL'.
    WHEN OTHERS.
  ENDCASE.

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
        wl_output    TYPE ty_output.

  DATA: wl_text_pair_read TYPE ty_text_pair_read.

  PERFORM read_texts CHANGING tl_texts
                              wl_text_pair_read.

  PERFORM read_proposals USING wl_text_pair_read
                               tl_texts
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

ENDFORM.                    " ANALYZE_TRANSLATIONS

*&---------------------------------------------------------------------*
*&      Form  READ_TEXTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_TL_TEXTS  text
*----------------------------------------------------------------------*
FORM read_texts  CHANGING pt_texts TYPE ty_t_pcx_s1
                          pw_text_pair_read TYPE ty_text_pair_read.

  CALL FUNCTION 'LXE_OBJ_TEXT_PAIR_READ'
    EXPORTING
      t_lang    = p_dstlan
      s_lang    = p_scrlan
      custmnr   = '999999'
      objtype   = p_type
      objname   = p_objnam
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
                     CHANGING pt_proposals      TYPE ty_t_pcx_s2.

  CALL FUNCTION 'LXE_PP1_PROPOSALS_GET_SE63'
    EXPORTING
      s_lang   = p_scrlan
      t_lang   = p_dstlan
      custmnr  = '999999'
      objtype  = p_type
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
*&      Form  SET_P_OBJNAM
*&---------------------------------------------------------------------*
* Set the object name for the translation system
*----------------------------------------------------------------------*
FORM set_p_objnam.

  TYPES: BEGIN OF tyl_function,
           funcname TYPE tfdir-funcname,
           pname    TYPE tfdir-pname,
         END OF tyl_function.

  DATA: wl_function TYPE tyl_function.

  CASE abap_true.
    WHEN rb_prog OR rb_tran OR rb_dtel.

      p_objnam = p_obj.

    WHEN rb_func.

      SELECT SINGLE funcname pname
      FROM tfdir
      INTO wl_function
      WHERE funcname EQ p_obj.

      wl_function-pname = wl_function-pname+4.
      p_objnam = wl_function.

    WHEN OTHERS.
  ENDCASE.


ENDFORM.                    " SET_P_OBJNAM

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.5.5 - E.G.Mellodew. 1998-2016. Sap Release 700
