*&---------------------------------------------------------------------*
*& Include          ZWMR_PICKING_SEM_OT_TOP
*&---------------------------------------------------------------------*

REPORT zwmr_picking_sem_ot.

************************************************************************
* TYPES                                                                *
************************************************************************
TYPES:
  BEGIN OF ty_pistolagem,
    imei TYPE char18,
  END OF ty_pistolagem .

TYPES:
  BEGIN OF ty_9000,
    ref    TYPE c LENGTH 10,
    transp TYPE lfa1-lifnr,
    lety   TYPE mlgn-lety1,
  END OF ty_9000,

  BEGIN OF ty_lety,
    lety1(15)," TYPE mlgn-lety1,
    lety2(15),"TYPE mlgn-lety2,
    lety3(15),"TYPE mlgn-lety3,
  END OF ty_lety,


  BEGIN OF ty_9100,
    descricao TYPE makt-maktg,  "Descrição
    matnr_v   TYPE string, "Cod validação
    matnr     TYPE mara-matnr,  "Cod
    lote      TYPE lqua-charg,  "Lote
    qtd_v     TYPE i,           "Qtd
    serial    TYPE char20,      "Serial
  END OF ty_9100,

  BEGIN OF ty_9200,
    id    TYPE c LENGTH 2,
    rbnum TYPE lvs_rbnum,
  END OF ty_9200,

  BEGIN OF ty_9100_total,
    vol_de    TYPE c LENGTH 4,
    vol_ate   TYPE c LENGTH 4,
    qtd       TYPE i,
    qtd_lidos TYPE i,
  END OF ty_9100_total,

  BEGIN OF ty_qtd_lidos,
    matnr     TYPE lqua-matnr,
    serial    TYPE char18,
    qtd       TYPE lqua-verme,
    qtd_lidos TYPE lqua-verme,
  END OF ty_qtd_lidos,

  BEGIN OF ty_mat_serial_lote,
    matnr  TYPE matnr,
    serial TYPE char18,
    lote   TYPE char10,
  END OF ty_mat_serial_lote,

  BEGIN OF ty_remessa,
    id    TYPE c LENGTH 2,
    rbnum TYPE lvs_rbnum,
  END OF ty_remessa,

  BEGIN OF ty_lrf_wkqu,
    lgnum TYPE lrf_wkqu-lgnum,
  END OF ty_lrf_wkqu,

  BEGIN OF ty_lips,
    vbeln TYPE lips-vbeln,
    posnr TYPE lips-posnr,
    matnr TYPE lips-matnr,
    lgort TYPE lips-lgort,
    lfimg TYPE lips-lfimg,
  END OF ty_lips,

  BEGIN OF ty_t311a,
    rbnum TYPE t311a-rbnum,
  END OF ty_t311a,

  BEGIN OF ty_ekko,
    ebeln TYPE ekko-ebeln,
  END OF  ty_ekko .

************************************************************************
* TABELAS/WORKAREAS                                                    *
************************************************************************
DATA:
  gs_9000            TYPE ty_9000,
  gs_lety            TYPE ty_lety,
  gs_9100            TYPE ty_9100,
  gs_9100_tot        TYPE ty_9100_total,
  gs_9200            TYPE ty_9200,
  gs_lrf_wkqu        TYPE ty_lrf_wkqu,
  tg_pistolagem      TYPE TABLE OF ty_pistolagem,
  gs_ekko            TYPE ty_ekko,
  gt_t311a           TYPE TABLE OF ty_t311a,
  gt_lqua            TYPE TABLE OF lqua,
  gs_lqua            TYPE lqua,
  gt_qtd_lidos       TYPE TABLE OF ty_qtd_lidos,
  gt_qtd_lidos_2     TYPE TABLE OF ty_qtd_lidos,
  gs_qtd_lidos       TYPE ty_qtd_lidos,
  gt_item_tmp        TYPE TABLE OF ztwm_item_tmp,
  gs_item_tmp        TYPE ztwm_item_tmp,
  gt_conf_tmp        TYPE TABLE OF ztwm_conf_tmp,
  gs_conf_tmp        TYPE ztwm_conf_tmp,
  gt_conf_hist       TYPE TABLE OF ztwm_conf_hist,
  gs_conf_hist       TYPE ztwm_conf_hist,
  gt_ltap_creat      TYPE TABLE OF ltap_creat,
  gt_mat_serial_lote TYPE TABLE OF ty_mat_serial_lote,
  gt_remessa         TYPE TABLE OF ty_remessa,
  gs_remessa         TYPE ty_remessa,
  gt_lips            TYPE TABLE OF ty_lips,
  tg_ztwm_sepa_imei  TYPE TABLE OF ztwm_sepa_imei_p,
  tg_ztwm_conf_venda TYPE TABLE OF ztwm_conf_venda,
  lt_calc_volume     TYPE TABLE OF ztwm_calc_volume,
  w_tela             TYPE ty_remessa.

************************************************************************
* VARIÁVEIS                                                            *
************************************************************************
DATA:
  gv_erro       TYPE c,
  gv_answer     TYPE c,
  gv_msgv1      TYPE sprot_u-var1,
  gv_msgv2      TYPE sprot_u-var2,
  gv_msgv3      TYPE sprot_u-var3,
  gv_msgv4      TYPE sprot_u-var4,
  gv_material   TYPE matnr,
  gv_bloq       TYPE c,
  gv_volume     TYPE n LENGTH 3,
  gv_active_qtd TYPE c,
  gv_msgid      TYPE t100-arbgb,
  gv_msgno      TYPE t100-msgnr,
  vg_pallet     TYPE mlgn-lety1,
  vg_ref        TYPE c LENGTH 10,
  vg_peso       TYPE brgew,
  vl_continue   TYPE abap_bool,
  vg_parcial    TYPE abap_bool,
  one_f         TYPE char01,
  two_f         TYPE char01,
  three_f       TYPE char01.

DATA: gv_qtd_remessas TYPE i,
      gv_idx          TYPE i,
      gv_line         TYPE i,
      gv_lines        TYPE i,
      gv_limit        TYPE i,
      gv_lgtyp        TYPE lgtyp,
      gv_lgort        TYPE lips-lgort,
      lidos_remessa   TYPE i,
      vg_processo(10) TYPE c.

*************************************************************************
** OBJETOS                                                              *
*************************************************************************


*************************************************************************
** EVENTOS                                                              *
*************************************************************************

INITIALIZATION.

  PERFORM f_seleciona_tvavr.
  PERFORM f_verificar_usuario CHANGING gv_erro.

  CHECK gv_erro IS INITIAL.

  CALL SCREEN 9000.
