class ZCL_SELECT definition
  public
  final
  create public .

public section.

  class-methods SELECT_DEFINED_TABLE_SFLIGHT
    returning
      value(RETURN) type SY-SUBRC .
  class-methods SELECT_INLINE_TABLE_SFLIGHT
    returning
      value(RETURN) type SY-SUBRC .
  class-methods SELECT_PART_ORD_SFLIGHT
    returning
      value(RETURN) type SY-SUBRC .
  class-methods SELECT_PART_INL_ORD_SFLIGHT
    returning
      value(RETURN) type SY-SUBRC .
  class-methods SELECT_PART_UNORD_SFLIGHT
    returning
      value(RETURN) type SY-SUBRC .
  class-methods SELECT_PART_INL_UNORD_SFLIGHT
    returning
      value(RETURN) type SY-SUBRC .
  class-methods SELECT_INL_TABLE_CDS_SFLIGHT
    returning
      value(RETURN) type SY-SUBRC .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF partial_flight,
             carrid    TYPE s_carr_id,
             connid    TYPE s_conn_id,
             fldate    TYPE s_date,
             price     TYPE s_price,
             currency  TYPE s_currcode,
             planetype TYPE s_planetye,
           END OF partial_flight .
ENDCLASS.



CLASS ZCL_SELECT IMPLEMENTATION.


  METHOD select_defined_table_sflight.
    DATA: fligts TYPE TABLE OF sflight.

    SELECT
      FROM sflight
      FIELDS *
      INTO TABLE @fligts.

    return = sy-subrc.
  ENDMETHOD.


  METHOD select_inline_table_sflight.
    SELECT
      FROM sflight
      FIELDS *
      INTO TABLE @DATA(flights).

    return = sy-subrc.
  ENDMETHOD.


  METHOD select_inl_table_cds_sflight.
    SELECT
      FROM fdt_test_flight_cds
      FIELDS *
      INTO TABLE @DATA(flights).

    return = sy-subrc.
  ENDMETHOD.


  METHOD select_part_inl_ord_sflight.
    SELECT
      FROM sflight
      FIELDS carrid,
             connid,
             fldate,
             price,
             currency,
             planetype
      INTO TABLE @DATA(flights).

    return = sy-subrc.
  ENDMETHOD.


  METHOD SELECT_PART_INL_UNORD_SFLIGHT.
    SELECT
      FROM sflight
      FIELDS planetype,
             carrid,
             currency,
             fldate,
             price,
             connid
      INTO TABLE @DATA(flights).

    return = sy-subrc.
  ENDMETHOD.


  METHOD select_part_ord_sflight.
    DATA: flights TYPE TABLE OF partial_flight.

    SELECT
      FROM sflight
      FIELDS carrid,
             connid,
             fldate,
             price,
             currency,
             planetype
      INTO CORRESPONDING FIELDS OF TABLE @flights.

    return = sy-subrc.
  ENDMETHOD.


  METHOD select_part_unord_sflight.
    DATA: flights TYPE TABLE OF partial_flight.

    SELECT
      FROM sflight
      FIELDS planetype,
             carrid,
             currency,
             fldate,
             price,
             connid
      INTO CORRESPONDING FIELDS OF TABLE @flights.

    return = sy-subrc.
  ENDMETHOD.
ENDCLASS.
