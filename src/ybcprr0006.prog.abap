*----------------------------------------------------------------------*
* Developer:    Sebastián Soto
* Date:         27.09.2016
* Description:  Creation
*----------------------------------------------------------------------*
* Developer:    Sebastián Soto
* Date:         27.08.2020
* Description:  Change Read Proposals function module
*----------------------------------------------------------------------*
*------ NAME CONVENTION -----------------------------------------------*
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

REPORT  ybcprr0006.

INCLUDE ybcprr0006_top.
INCLUDE ybcprr0006_sel.
INCLUDE ybcprr0006_f01.
INCLUDE ybcprr0006_o01.
INCLUDE ybcprr0006_i01.

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

AT SELECTION-SCREEN.

  IF  sy-ucomm EQ 'ONLI'
  AND p_dstlan EQ p_scrlan.

    MESSAGE e003(skta).
*   Source language and target language cannot be the same

  ENDIF.

START-OF-SELECTION.



  PERFORM analyze_translations.

END-OF-SELECTION.

  CALL SCREEN 0100.
