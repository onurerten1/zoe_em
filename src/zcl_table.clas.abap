class ZCL_TABLE definition
  public
  final
  create public .

public section.

  class-methods STANDARD_TABLE_TEST
    returning
      value(RETURN) type SY-SUBRC .
  class-methods SORTED_TABLE_TEST
    returning
      value(RETURN) type SY-SUBRC .
  class-methods HASHED_TABLE_TEST
    returning
      value(RETURN) type SY-SUBRC .
  class-methods STANDARD_TABLE_INDEX_TEST
    returning
      value(RETURN) type SY-SUBRC .
  class-methods SORTED_TABLE_INDEX_TEST
    returning
      value(RETURN) type SY-SUBRC .
  class-methods SFLIGHT_STANDARD_TABLE
    returning
      value(RETURN) type SY-SUBRC .
  class-methods SFLIGHT_SORTED_TABLE
    returning
      value(RETURN) type SY-SUBRC .
  class-methods SFLIGHT_HASHED_TABLE
    returning
      value(RETURN) type SY-SUBRC .
  class-methods SBOOK_STANDARD_TABLE
    returning
      value(RETURN) type SY-SUBRC .
  class-methods SBOOK_SORTED_TABLE
    returning
      value(RETURN) type SY-SUBRC .
  class-methods SBOOK_HASHED_TABLE
    returning
      value(RETURN) type SY-SUBRC .
  PROTECTED SECTION.
private section.

  types:
    BEGIN OF int_tab,
        line TYPE i,
      END OF int_tab .

  class-data:
    standard_table TYPE STANDARD TABLE OF int_tab .
  class-data:
    sorted_table   TYPE SORTED TABLE OF int_tab WITH UNIQUE KEY line .
  class-data:
    hashed_table   TYPE HASHED TABLE OF int_tab WITH UNIQUE KEY line .
  class-data:
    sflight_standard TYPE STANDARD TABLE OF sflight .
  class-data:
    sflight_sorted   TYPE SORTED TABLE OF sflight WITH UNIQUE KEY carrid connid fldate .
  class-data:
    sflight_hashed   TYPE HASHED TABLE OF sflight WITH UNIQUE KEY carrid connid fldate .
  class-data:
    sbook_standard TYPE STANDARD TABLE OF sbook .
  class-data:
    sbook_sorted TYPE SORTED TABLE OF sbook WITH UNIQUE KEY carrid connid fldate bookid .
  class-data:
    sbook_hashed TYPE HASHED TABLE OF sbook WITH UNIQUE KEY carrid connid fldate bookid .
ENDCLASS.



CLASS ZCL_TABLE IMPLEMENTATION.


  METHOD hashed_table_test.
    hashed_table = VALUE #( FOR i = 0 THEN i + 1 UNTIL i = 10000
                             ( line = i ) ).

    DATA(number) = cl_abap_random_int=>create(
      seed = cl_abap_random=>seed( )
      min  = 1
      max  = 10000 ).

    READ TABLE hashed_table INTO DATA(table_line) WITH KEY line = number->get_next( ).

    return = sy-subrc.
  ENDMETHOD.


  METHOD sbook_hashed_table.
    SELECT
      FROM sbook
      FIELDS *
      INTO TABLE @sbook_hashed.

    READ TABLE sbook_hashed INTO DATA(sbook) WITH KEY carrid  = 'UA'
                                                      connid  = 3517
                                                      fldate  = '20190304'
                                                      bookid  = 00016969.

    return = sy-subrc.
  ENDMETHOD.


  METHOD sbook_sorted_table.
    SELECT
      FROM sbook
      FIELDS *
      INTO TABLE @sbook_sorted.

    READ TABLE sbook_sorted INTO DATA(sbook) WITH KEY carrid  = 'UA'
                                                      connid  = 3517
                                                      fldate  = '20190304'
                                                      bookid  = 00016969.

    return = sy-subrc.
  ENDMETHOD.


  METHOD sbook_standard_table.
    SELECT
      FROM sbook
      FIELDS *
      INTO TABLE @sbook_standard.

    READ TABLE sbook_standard INTO DATA(sbook) WITH KEY carrid  = 'UA'
                                                        connid  = 3517
                                                        fldate  = '20190304'
                                                        bookid  = 00016969.

    return = sy-subrc.
  ENDMETHOD.


  METHOD sflight_hashed_table.
    SELECT
      FROM sflight
      FIELDS *
      INTO TABLE @sflight_hashed.

    READ TABLE sflight_hashed INTO DATA(sflight_line) WITH TABLE KEY carrid = 'UA'
                                                                     connid = 3517
                                                                     fldate = '20190304'.

    return = sy-subrc.
  ENDMETHOD.


  METHOD sflight_sorted_table.
    SELECT
      FROM sflight
      FIELDS *
      INTO TABLE @sflight_sorted.

    READ TABLE sflight_sorted INTO DATA(sflight_line) WITH TABLE KEY carrid = 'UA'
                                                                     connid = 3517
                                                                     fldate = '20190304'.

    return = sy-subrc.
  ENDMETHOD.


  METHOD sflight_standard_table.
    SELECT
      FROM sflight
      FIELDS *
      INTO TABLE @sflight_standard.

    READ TABLE sflight_standard INTO DATA(sflight_line) WITH KEY carrid = 'UA'
                                                                 connid = 3517
                                                                 fldate = '20190304'.

    return = sy-subrc.
  ENDMETHOD.


  METHOD sorted_table_index_test.
    sorted_table = VALUE #( FOR i = 0 THEN i + 1 UNTIL i = 10000
                            ( line = i ) ).
    DATA(number) = cl_abap_random_int=>create(
      seed = cl_abap_random=>seed( )
      min  = 1
      max  = 10000 ).

    READ TABLE sorted_table INTO DATA(table_line) INDEX number->get_next( ).

    return = sy-subrc.
  ENDMETHOD.


  METHOD sorted_table_test.
    sorted_table = VALUE #( FOR i = 0 THEN i + 1 UNTIL i = 10000
                             ( line = i ) ).
    DATA(number) = cl_abap_random_int=>create(
      seed = cl_abap_random=>seed( )
      min  = 1
      max  = 10000 ).

    READ TABLE sorted_table INTO DATA(table_line) WITH KEY line = number->get_next( ).

    return = sy-subrc.
  ENDMETHOD.


  METHOD standard_table_index_test.
    standard_table = VALUE #( FOR i = 0 THEN i + 1 UNTIL i = 10000
                         ( line = i ) ).
    DATA(number) = cl_abap_random_int=>create(
      seed = cl_abap_random=>seed( )
      min  = 1
      max  = 10000 ).

    READ TABLE standard_table INTO DATA(table_line) INDEX number->get_next( ).

    return = sy-subrc.
  ENDMETHOD.


  METHOD standard_table_test.
    standard_table = VALUE #( FOR i = 0 THEN i + 1 UNTIL i = 10000
                             ( line = i ) ).
    DATA(number) = cl_abap_random_int=>create(
      seed = cl_abap_random=>seed( )
      min  = 1
      max  = 10000 ).

    READ TABLE standard_table INTO DATA(table_line) WITH KEY line = number->get_next( ).

    return = sy-subrc.
  ENDMETHOD.
ENDCLASS.
