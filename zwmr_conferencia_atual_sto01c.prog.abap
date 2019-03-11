*----------------------------------------------------------------------*
***INCLUDE ZWMR_CONFERENCIA_ATUAL_STATO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_9300 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_9300 OUTPUT.
  SET PF-STATUS 'ST_9300'.
* SET TITLEBAR 'xxx'.

  CLEAR: vg_pallet.

  DATA:vl_matnr TYPE char18.
  DATA(io_util) = NEW zclwm_tratamento( ).
  gs_9100-matnr = io_zcl1->get_matnr( gs_ekko ).
  IF gs_9100-matnr IS NOT INITIAL.
    vl_matnr = gs_9100-matnr(18).
    DATA(tg_lety) = io_util->get_pallet( i_matnr = vl_matnr ).
  ELSE.
    READ TABLE gt_lips INTO DATA(el_lips) WITH KEY vbeln = gs_9200-rbnum.
    vl_matnr = el_lips-matnr(18).
    tg_lety = io_util->get_pallet( i_matnr = vl_matnr ).
    gs_9100-matnr = vl_matnr.
  ENDIF.

  SELECT SINGLE *
    FROM mlgn
    INTO @DATA(ls_mlgn)
    WHERE matnr = @vl_matnr.

  LOOP AT tg_lety INTO DATA(el_lety).
    IF el_lety-lety1 IS NOT INITIAL.
      gs_lety-lety1 = el_lety-lety1 && ' : ' && ls_mlgn-lhmg1.
    ENDIF.
    IF el_lety-lety2 IS NOT INITIAL.
      gs_lety-lety2 = el_lety-lety2 && ' : ' && ls_mlgn-lhmg2.
    ENDIF.
    IF el_lety-lety3 IS NOT INITIAL.
      gs_lety-lety3 = el_lety-lety3 && ' : ' && ls_mlgn-lhmg3.
    ENDIF.
  ENDLOOP.

  IF gs_lety-lety1 IS NOT INITIAL.
    one_f = '1'.
  ELSE.
    CLEAR one_f.
  ENDIF.

  IF gs_lety-lety2 IS NOT INITIAL.
    two_f = '2'.
  ELSE.
    CLEAR two_f.
  ENDIF.
  IF gs_lety-lety3 IS NOT INITIAL.
    three_f = '3'.
  ELSE.
    CLEAR three_f.
  ENDIF.

ENDMODULE.
