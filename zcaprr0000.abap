*----------------------------------------------------------------------*
* Developer:    Sebastián Soto
* Date:         27.09.2016
* Description:  Creation
*----------------------------------------------------------------------*
*------  NOMENCLATURE  ------------------------------------------------*
* V_    --> Global Variable
* W_    --> Global Work Area
* T_    --> Global Internal Table
* O_    --> Global Object Instance
* <FS_  --> Global Field Symbol
* VL_   --> Local Variable
* WL_   --> Local Work Area
* TL_   --> Local Internal Table
* OL_   --> Local Object Instance
* <FSL_ --> Local Field Symbol
* P_    --> Parameter
* S_    --> Select-Option
* RB_   --> Radio Buttom
* CK_   --> Checkbox
*----------------------------------------------------------------------*

REPORT  zcaprr0000.

INCLUDE zcaprr0000_top.
INCLUDE zcaprr0000_sel.
INCLUDE zcaprr0000_f01.
INCLUDE zcaprr0000_o01.
INCLUDE zcaprr0000_i01.

INITIALIZATION.

  PERFORM fill_lang_listbox.

AT SELECTION-SCREEN OUTPUT.

  PERFORM disable_modid_b20.
  PERFORM radiobutton_rb11.

  PERFORM read_iso_langu USING p_langu
                      CHANGING p_scrlan.

  PERFORM read_iso_langu USING p_dest
                      CHANGING p_dstlan.

AT SELECTION-SCREEN ON p_obj.

  PERFORM read_master_language.

AT SELECTION-SCREEN ON RADIOBUTTON GROUP rb11.

  PERFORM radiobutton_rb11.

START-OF-SELECTION.

  PERFORM analyze_translations.

END-OF-SELECTION.

  CALL SCREEN 0100.

*GUI Texts
*----------------------------------------------------------
* TT0100 --> Check Translations
* TT0100 --> Verifiar traducciones

*Text elements
*----------------------------------------------------------
* B10 Select Object
* B11 Object Type
* B20 Object master data
* F01 Source Text
* F02 Translated Text


*Selection texts
*----------------------------------------------------------
* P_DEST         Translated to...
* P_DSTLAN         Destination Language
* P_LANGU         Master Language
* P_OBJ D       .
* P_SCRLAN         Source Language
* RB_CLAS         Class
* RB_CUAD         Status & Title GUI
* RB_DOMA         Domain
* RB_DTEL         Data element
* RB_ENQU         Lock object
* RB_FUGR         Function Group
* RB_FUNC         Function Module
* RB_MESS         Message Class
* RB_PROG         Program
* RB_SHLP         Search help
* RB_STRC         Structure or Table
* RB_TRAN         Transaction Code

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.5.5 - E.G.Mellodew. 1998-2016. Sap Release 700
