
TYPE-POOLS: abap, vrm, icon.

TYPES: BEGIN OF ty_text_pair_read,
         domatyp  TYPE  lxedomatyp,
         domanam  TYPE  lxedomanam,
       END OF ty_text_pair_read,
       BEGIN OF ty_output,
         icon    TYPE icon-name,
         stattrn TYPE lxe_pcx_s2-stattrn,
         textkey TYPE lxe_pcx_s1-textkey,
         s_text  TYPE lxe_pcx_s1-s_text,
         t_text  TYPE lxe_pcx_s1-t_text,
       END OF ty_output.

TYPES: ty_t_pcx_s1 TYPE STANDARD TABLE OF lxe_pcx_s1,
       ty_t_pcx_s2 TYPE STANDARD TABLE OF lxe_pcx_s2.

DATA: o_alv    TYPE REF TO cl_gui_alv_grid,
      o_cc     TYPE REF TO cl_gui_custom_container.

DATA: t_output TYPE STANDARD TABLE OF ty_output.

----------------------------------------------------------------------------------
Extracted by Mass Download version 1.5.5 - E.G.Mellodew. 1998-2016. Sap Release 700
