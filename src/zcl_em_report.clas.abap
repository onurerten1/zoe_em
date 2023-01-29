class ZCL_EM_REPORT definition
  public
  final
  create public .

public section.

  class-data:
    alv_table TYPE TABLE OF zoe_em_flight_report_alv .

  class-methods STANDARD_REPORT_SELECTION
    returning
      value(RETURN) type SY-SUBRC .
  class-methods SELECT_SINGLE_IN_LOOP
    returning
      value(RETURN) type SY-SUBRC .
  class-methods FAE_READ_TABLE
    returning
      value(RETURN) type SY-SUBRC .
  class-methods FAE_STRUCTURE_FILL
    returning
      value(RETURN) type SY-SUBRC .
  class-methods IJ_READ_TABLE
    returning
      value(RETURN) type SY-SUBRC .
  class-methods IJ_STRUCTURE_FILL
    returning
      value(RETURN) type SY-SUBRC .
  class-methods FULL_CDS_SELECTION
    returning
      value(RETURN) type SY-SUBRC .
  class-methods ALV_DISPLAY .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_EM_REPORT IMPLEMENTATION.


  METHOD alv_display.
    IF alv_table IS INITIAL.
      standard_report_selection( ).
    ENDIF.

    cl_salv_table=>factory(
      IMPORTING
        r_salv_table = DATA(salv)
      CHANGING
        t_table      = alv_table ).

    salv->display( ).
  ENDMETHOD.


  METHOD fae_read_table.
    SELECT
      FROM sflight AS s1
      FIELDS s1~carrid,
             s1~connid,
             s1~fldate,
             s1~price,
             s1~currency,
             s1~planetype,
             s1~paymentsum
      ORDER BY s1~carrid,
               s1~connid,
               s1~fldate
      INTO CORRESPONDING FIELDS OF TABLE @alv_table.

    SELECT
      FROM scarr
      FIELDS DISTINCT carrid,
                      carrname
      FOR ALL ENTRIES IN @alv_table
      WHERE carrid = @alv_table-carrid
      INTO TABLE @DATA(carriers).

    SELECT
      FROM spfli
      FIELDS DISTINCT carrid,
                      connid,
                      countryfr,
                      cityfrom,
                      airpfrom,
                      countryto,
                      cityto,
                      airpto,
                      fltime,
                      deptime,
                      arrtime,
                      distance,
                      distid
      FOR ALL ENTRIES IN @alv_table
      WHERE carrid = @alv_table-carrid
      AND   connid = @alv_table-connid
      INTO TABLE @DATA(flight_schedules).

    SELECT
      FROM t005t
      FIELDS DISTINCT land1,
                      landx
      FOR ALL ENTRIES IN @flight_schedules
      WHERE ( land1 = @flight_schedules-countryfr OR land1 = @flight_schedules-countryto )
      AND   spras = @sy-langu
      INTO TABLE @DATA(countries).

    SELECT
      FROM sairport
      FIELDS DISTINCT id,
                      name
      FOR ALL ENTRIES IN @flight_schedules
      WHERE ( id = @flight_schedules-airpfrom OR id = @flight_schedules-airpto )
      INTO TABLE @DATA(airports).

    LOOP AT alv_table ASSIGNING FIELD-SYMBOL(<alv_line>).
      READ TABLE carriers INTO DATA(carrier) WITH KEY carrid = <alv_line>-carrid.
      IF sy-subrc = 0.
        <alv_line>-carrname = carrier-carrname.
      ENDIF.

      READ TABLE flight_schedules INTO DATA(flight_schedule) WITH KEY carrid = <alv_line>-carrid
                                                                      connid = <alv_line>-connid.
      IF sy-subrc = 0.
        <alv_line>-countryfr = flight_schedule-countryfr.
        <alv_line>-cityfrom = flight_schedule-cityfrom.
        <alv_line>-airpfrom = flight_schedule-airpfrom.
        <alv_line>-countryto = flight_schedule-countryto.
        <alv_line>-cityto = flight_schedule-cityto.
        <alv_line>-airpto = flight_schedule-airpto.
        <alv_line>-fltime = flight_schedule-fltime.
        <alv_line>-deptime = flight_schedule-deptime.
        <alv_line>-arrtime = flight_schedule-arrtime.
        <alv_line>-distance = flight_schedule-distance.
        <alv_line>-distid = flight_schedule-distid.
      ENDIF.

      READ TABLE countries INTO DATA(country) WITH KEY land1 = <alv_line>-countryfr.
      IF sy-subrc = 0.
        <alv_line>-countryfr_name = country-landx.
      ENDIF.

      READ TABLE countries INTO country WITH KEY land1 = <alv_line>-countryto.
      IF sy-subrc = 0.
        <alv_line>-countryto_name = country-landx.
      ENDIF.

      READ TABLE airports INTO DATA(airport) WITH KEY id = <alv_line>-airpfrom.
      IF sy-subrc = 0.
        <alv_line>-airpfrom_name = airport-name.
      ENDIF.

      READ TABLE airports INTO airport WITH KEY id = <alv_line>-airpto.
      IF sy-subrc = 0.
        <alv_line>-airpto_name = airport-name.
      ENDIF.
    ENDLOOP.

    return = sy-subrc.
  ENDMETHOD.


  METHOD fae_structure_fill.
    SELECT
      FROM sflight AS s1
      FIELDS s1~carrid,
             s1~connid,
             s1~fldate,
             s1~price,
             s1~currency,
             s1~planetype,
             s1~paymentsum
      ORDER BY s1~carrid,
               s1~connid,
               s1~fldate
      INTO CORRESPONDING FIELDS OF TABLE @alv_table.

    SELECT
      FROM scarr
      FIELDS DISTINCT carrid,
                      carrname
      FOR ALL ENTRIES IN @alv_table
      WHERE carrid = @alv_table-carrid
      INTO TABLE @DATA(carriers).

    SELECT
      FROM spfli
      FIELDS DISTINCT carrid,
                      connid,
                      countryfr,
                      cityfrom,
                      airpfrom,
                      countryto,
                      cityto,
                      airpto,
                      fltime,
                      deptime,
                      arrtime,
                      distance,
                      distid
      FOR ALL ENTRIES IN @alv_table
      WHERE carrid = @alv_table-carrid
      AND   connid = @alv_table-connid
      INTO TABLE @DATA(flight_schedules).

    SELECT
      FROM t005t
      FIELDS DISTINCT land1,
                      landx
      FOR ALL ENTRIES IN @flight_schedules
      WHERE ( land1 = @flight_schedules-countryfr OR land1 = @flight_schedules-countryto )
      AND   spras = @sy-langu
      INTO TABLE @DATA(countries).

    SELECT
      FROM sairport
      FIELDS DISTINCT id,
                      name
      FOR ALL ENTRIES IN @flight_schedules
      WHERE ( id = @flight_schedules-airpfrom OR id = @flight_schedules-airpto )
      INTO TABLE @DATA(airports).

    LOOP AT alv_table ASSIGNING FIELD-SYMBOL(<alv_line>).
      <alv_line> = VALUE #( BASE <alv_line>
                            carrname  = VALUE #( carriers[ carrid = <alv_line>-carrid ] OPTIONAL )
                            countryfr = VALUE #( flight_schedules[ carrid = <alv_line>-carrid
                                                                   connid = <alv_line>-connid ]-countryfr OPTIONAL )
                            cityfrom  = VALUE #( flight_schedules[ carrid = <alv_line>-carrid
                                                                   connid = <alv_line>-connid ]-cityfrom OPTIONAL )
                            airpfrom  = VALUE #( flight_schedules[ carrid = <alv_line>-carrid
                                                                   connid = <alv_line>-connid ]-airpfrom OPTIONAL )
                            countryto = VALUE #( flight_schedules[ carrid = <alv_line>-carrid
                                                                   connid = <alv_line>-connid ]-countryto OPTIONAL )
                            cityto    = VALUE #( flight_schedules[ carrid = <alv_line>-carrid
                                                                   connid = <alv_line>-connid ]-cityto OPTIONAL )
                            airpto    = VALUE #( flight_schedules[ carrid = <alv_line>-carrid
                                                                   connid = <alv_line>-connid ]-airpto OPTIONAL )
                            fltime    = VALUE #( flight_schedules[ carrid = <alv_line>-carrid
                                                                   connid = <alv_line>-connid ]-fltime OPTIONAL )
                            deptime   = VALUE #( flight_schedules[ carrid = <alv_line>-carrid
                                                                   connid = <alv_line>-connid ]-deptime OPTIONAL )
                            arrtime   = VALUE #( flight_schedules[ carrid = <alv_line>-carrid
                                                                   connid = <alv_line>-connid ]-arrtime OPTIONAL )
                            distance  = VALUE #( flight_schedules[ carrid = <alv_line>-carrid
                                                                   connid = <alv_line>-connid ]-distance OPTIONAL )
                            distid    = VALUE #( flight_schedules[ carrid = <alv_line>-carrid
                                                                   connid = <alv_line>-connid ]-distid OPTIONAL ) ).
      <alv_line> = VALUE #( BASE <alv_line>
                            countryfr_name  = VALUE #( countries[ land1 = <alv_line>-countryfr ]-landx OPTIONAL )
                            countryto_name  = VALUE #( countries[ land1 = <alv_line>-countryto ]-landx OPTIONAL )
                            airpfrom_name   = VALUE #( airports[ id = <alv_line>-airpfrom ]-name OPTIONAL )
                            airpto_name   = VALUE #( airports[ id = <alv_line>-airpto ]-name OPTIONAL ) ).
    ENDLOOP.

    return = sy-subrc.
  ENDMETHOD.


  METHOD full_cds_selection.
    SELECT
      FROM zi_flightreport
      FIELDS *
      INTO CORRESPONDING FIELDS OF TABLE @alv_table.

    return = sy-subrc.
  ENDMETHOD.


  METHOD ij_read_table.
    SELECT
      FROM sflight AS s1
      FIELDS s1~carrid,
             s1~connid,
             s1~fldate,
             s1~price,
             s1~currency,
             s1~planetype,
             s1~paymentsum
      ORDER BY s1~carrid,
               s1~connid,
               s1~fldate
      INTO CORRESPONDING FIELDS OF TABLE @alv_table.

    SELECT
      FROM scarr AS s
      INNER JOIN @alv_table AS a ON a~carrid = s~carrid
      FIELDS DISTINCT s~carrid,
                      s~carrname
      INTO TABLE @DATA(carriers).

    SELECT
      FROM spfli AS s
      INNER JOIN @alv_table AS a ON a~carrid = s~carrid
      AND                           a~connid = s~connid
      FIELDS DISTINCT s~carrid,
                      s~connid,
                      s~countryfr,
                      s~cityfrom,
                      s~airpfrom,
                      s~countryto,
                      s~cityto,
                      s~airpto,
                      s~fltime,
                      s~deptime,
                      s~arrtime,
                      s~distance,
                      s~distid
      INTO TABLE @DATA(flight_schedules).

    SELECT
      FROM t005t AS t
      INNER JOIN @flight_schedules AS a ON ( a~countryfr = t~land1 OR a~countryto = t~land1 )
      FIELDS DISTINCT t~land1,
                      t~landx
      WHERE spras = @sy-langu
      INTO TABLE @DATA(countries).

    SELECT
      FROM sairport AS s
      INNER JOIN @flight_schedules AS a ON ( a~airpfrom = s~id OR a~airpto = s~id )
      FIELDS DISTINCT s~id,
                      s~name
      INTO TABLE @DATA(airports).

    LOOP AT alv_table ASSIGNING FIELD-SYMBOL(<alv_line>).
      READ TABLE carriers INTO DATA(carrier) WITH KEY carrid = <alv_line>-carrid.
      IF sy-subrc = 0.
        <alv_line>-carrname = carrier-carrname.
      ENDIF.

      READ TABLE flight_schedules INTO DATA(flight_schedule) WITH KEY carrid = <alv_line>-carrid
                                                                      connid = <alv_line>-connid.
      IF sy-subrc = 0.
        <alv_line>-countryfr = flight_schedule-countryfr.
        <alv_line>-cityfrom = flight_schedule-cityfrom.
        <alv_line>-airpfrom = flight_schedule-airpfrom.
        <alv_line>-countryto = flight_schedule-countryto.
        <alv_line>-cityto = flight_schedule-cityto.
        <alv_line>-airpto = flight_schedule-airpto.
        <alv_line>-fltime = flight_schedule-fltime.
        <alv_line>-deptime = flight_schedule-deptime.
        <alv_line>-arrtime = flight_schedule-arrtime.
        <alv_line>-distance = flight_schedule-distance.
        <alv_line>-distid = flight_schedule-distid.
      ENDIF.

      READ TABLE countries INTO DATA(country) WITH KEY land1 = <alv_line>-countryfr.
      IF sy-subrc = 0.
        <alv_line>-countryfr_name = country-landx.
      ENDIF.

      READ TABLE countries INTO country WITH KEY land1 = <alv_line>-countryto.
      IF sy-subrc = 0.
        <alv_line>-countryto_name = country-landx.
      ENDIF.

      READ TABLE airports INTO DATA(airport) WITH KEY id = <alv_line>-airpfrom.
      IF sy-subrc = 0.
        <alv_line>-airpfrom_name = airport-name.
      ENDIF.

      READ TABLE airports INTO airport WITH KEY id = <alv_line>-airpto.
      IF sy-subrc = 0.
        <alv_line>-airpto_name = airport-name.
      ENDIF.
    ENDLOOP.

    return = sy-subrc.
  ENDMETHOD.


  METHOD ij_structure_fill.
    SELECT
      FROM sflight AS s1
      FIELDS s1~carrid,
             s1~connid,
             s1~fldate,
             s1~price,
             s1~currency,
             s1~planetype,
             s1~paymentsum
      ORDER BY s1~carrid,
               s1~connid,
               s1~fldate
      INTO CORRESPONDING FIELDS OF TABLE @alv_table.

    SELECT
      FROM scarr AS s
      INNER JOIN @alv_table AS a ON a~carrid = s~carrid
      FIELDS DISTINCT s~carrid,
                      s~carrname
      INTO TABLE @DATA(carriers).

    SELECT
      FROM spfli AS s
      INNER JOIN @alv_table AS a ON a~carrid = s~carrid
      AND                           a~connid = s~connid
      FIELDS DISTINCT s~carrid,
                      s~connid,
                      s~countryfr,
                      s~cityfrom,
                      s~airpfrom,
                      s~countryto,
                      s~cityto,
                      s~airpto,
                      s~fltime,
                      s~deptime,
                      s~arrtime,
                      s~distance,
                      s~distid
      INTO TABLE @DATA(flight_schedules).

    SELECT
      FROM t005t AS t
      INNER JOIN @flight_schedules AS a ON ( a~countryfr = t~land1 OR a~countryto = t~land1 )
      FIELDS DISTINCT t~land1,
                      t~landx
      WHERE spras = @sy-langu
      INTO TABLE @DATA(countries).

    SELECT
      FROM sairport AS s
      INNER JOIN @flight_schedules AS a ON ( a~airpfrom = s~id OR a~airpto = s~id )
      FIELDS DISTINCT s~id,
                      s~name
      INTO TABLE @DATA(airports).

    LOOP AT alv_table ASSIGNING FIELD-SYMBOL(<alv_line>).
      <alv_line> = VALUE #( BASE <alv_line>
                            carrname  = VALUE #( carriers[ carrid = <alv_line>-carrid ] OPTIONAL )
                            countryfr = VALUE #( flight_schedules[ carrid = <alv_line>-carrid
                                                                   connid = <alv_line>-connid ]-countryfr OPTIONAL )
                            cityfrom  = VALUE #( flight_schedules[ carrid = <alv_line>-carrid
                                                                   connid = <alv_line>-connid ]-cityfrom OPTIONAL )
                            airpfrom  = VALUE #( flight_schedules[ carrid = <alv_line>-carrid
                                                                   connid = <alv_line>-connid ]-airpfrom OPTIONAL )
                            countryto = VALUE #( flight_schedules[ carrid = <alv_line>-carrid
                                                                   connid = <alv_line>-connid ]-countryto OPTIONAL )
                            cityto    = VALUE #( flight_schedules[ carrid = <alv_line>-carrid
                                                                   connid = <alv_line>-connid ]-cityto OPTIONAL )
                            airpto    = VALUE #( flight_schedules[ carrid = <alv_line>-carrid
                                                                   connid = <alv_line>-connid ]-airpto OPTIONAL )
                            fltime    = VALUE #( flight_schedules[ carrid = <alv_line>-carrid
                                                                   connid = <alv_line>-connid ]-fltime OPTIONAL )
                            deptime   = VALUE #( flight_schedules[ carrid = <alv_line>-carrid
                                                                   connid = <alv_line>-connid ]-deptime OPTIONAL )
                            arrtime   = VALUE #( flight_schedules[ carrid = <alv_line>-carrid
                                                                   connid = <alv_line>-connid ]-arrtime OPTIONAL )
                            distance  = VALUE #( flight_schedules[ carrid = <alv_line>-carrid
                                                                   connid = <alv_line>-connid ]-distance OPTIONAL )
                            distid    = VALUE #( flight_schedules[ carrid = <alv_line>-carrid
                                                                   connid = <alv_line>-connid ]-distid OPTIONAL ) ).
      <alv_line> = VALUE #( BASE <alv_line>
                            countryfr_name  = VALUE #( countries[ land1 = <alv_line>-countryfr ]-landx OPTIONAL )
                            countryto_name  = VALUE #( countries[ land1 = <alv_line>-countryto ]-landx OPTIONAL )
                            airpfrom_name   = VALUE #( airports[ id = <alv_line>-airpfrom ]-name OPTIONAL )
                            airpto_name   = VALUE #( airports[ id = <alv_line>-airpto ]-name OPTIONAL ) ).
    ENDLOOP.

    return = sy-subrc.
  ENDMETHOD.


  METHOD select_single_in_loop.
    SELECT
      FROM sflight AS s1
      FIELDS s1~carrid,
             s1~connid,
             s1~fldate,
             s1~price,
             s1~currency,
             s1~planetype,
             s1~paymentsum
      ORDER BY s1~carrid,
               s1~connid,
               s1~fldate
      INTO CORRESPONDING FIELDS OF TABLE @alv_table.

    LOOP AT alv_table ASSIGNING FIELD-SYMBOL(<alv_line>).
      SELECT SINGLE
        FROM scarr
        FIELDS carrname
        WHERE carrid = @<alv_line>-carrid
        INTO @<alv_line>-carrname.

      SELECT SINGLE
        FROM spfli
        FIELDS countryfr,
               cityfrom,
               airpfrom,
               countryto,
               cityto,
               airpto,
               fltime,
               deptime,
               arrtime,
               distance,
               distid
        WHERE carrid = @<alv_line>-carrid
        AND   connid = @<alv_line>-connid
        INTO CORRESPONDING FIELDS OF @<alv_line>.

      SELECT SINGLE
        FROM t005t
        FIELDS landx
        WHERE land1 = @<alv_line>-countryfr
        AND   spras = @sy-langu
        INTO @<alv_line>-countryfr_name.

      SELECT SINGLE
        FROM t005t
        FIELDS landx
        WHERE land1 = @<alv_line>-countryto
        AND   spras = @sy-langu
        INTO @<alv_line>-countryto_name.

      SELECT SINGLE
        FROM sairport
        FIELDS name
        WHERE id = @<alv_line>-airpfrom
        INTO @<alv_line>-airpfrom_name.

      SELECT SINGLE
        FROM sairport
        FIELDS name
        WHERE id = @<alv_line>-airpto
        INTO @<alv_line>-airpto_name.
    ENDLOOP.

    return = sy-subrc.
  ENDMETHOD.


  METHOD standard_report_selection.
    SELECT
      FROM sflight AS s1
      INNER JOIN      scarr     AS s2 ON s2~carrid  = s1~carrid
      LEFT OUTER JOIN spfli     AS s3 ON s3~carrid  = s1~carrid
      AND                                s3~connid  = s1~connid
      LEFT OUTER JOIN t005t     AS t1 ON t1~spras   = @sy-langu
      AND                                t1~land1   = s3~countryfr
      LEFT OUTER JOIN sairport  AS s4 ON s4~id      = s3~airpfrom
      LEFT OUTER JOIN t005t     AS t2 ON t2~spras   = @sy-langu
      AND                                t2~land1   = s3~countryto
      LEFT OUTER JOIN sairport  AS s5 ON s5~id      = s3~airpto
      FIELDS s1~carrid,
             s1~connid,
             s1~fldate,
             s1~price,
             s1~currency,
             s1~planetype,
             s1~paymentsum,
             s2~carrname,
             s3~countryfr,
             s3~cityfrom,
             s3~airpfrom,
             s3~countryto,
             s3~cityto,
             s3~airpto,
             s3~fltime,
             s3~deptime,
             s3~arrtime,
             s3~distance,
             s3~distid,
             t1~landx AS countryfr_name,
             s4~name  AS airpfrom_name,
             t2~landx AS countryto_name,
             s5~name  AS airpto_name
      ORDER BY s1~carrid,
               s1~connid,
               s1~fldate
      INTO CORRESPONDING FIELDS OF TABLE @alv_table.

    return = sy-subrc.
  ENDMETHOD.
ENDCLASS.
