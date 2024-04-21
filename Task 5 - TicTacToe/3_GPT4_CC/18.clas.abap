CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES: BEGIN OF player_enum,
             one TYPE c LENGTH 1 VALUE 'X',
             two TYPE c LENGTH 1 VALUE 'O',
           END OF player_enum.

    TYPES: BEGIN OF state_enum,
             ongoing_game TYPE string VALUE `Ongoing game`,
             draw         TYPE string VALUE `Draw`,
             win          TYPE string VALUE `Win`,
           END OF state_enum.

    TYPES board_type TYPE TABLE OF string LENGTH 3 WITH DEFAULT KEY INITIAL SIZE 3.

    METHODS get_state
      IMPORTING board        TYPE board_type
      RETURNING VALUE(state) TYPE string
      RAISING   cx_parameter_invalid.

  PRIVATE SECTION.
    METHODS validate_board
      IMPORTING board      TYPE board_type
      RAISING   cx_parameter_invalid.

    METHODS check_win
      IMPORTING board      TYPE board_type
      RETURNING VALUE(win) TYPE abap_bool.

    METHODS count_marks
      IMPORTING board      TYPE board_type
      EXPORTING x_count    TYPE i
                o_count    TYPE i.

    METHODS check_draw
      IMPORTING board      TYPE board_type
      RETURNING VALUE(draw) TYPE abap_bool.
ENDCLASS.


CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    validate_board( board ).

    IF check_win( board ).
      state = state_enum-win.
    ELSEIF check_draw( board ).
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD validate_board.
    DATA(x_count) = 0.
    DATA(o_count) = 0.
    count_marks( EXPORTING board = board IMPORTING x_count = x_count o_count = o_count ).

    IF x_count < o_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    IF check_win( board ) AND NOT ( x_count = o_count + 1 OR o_count = x_count ).
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.
  ENDMETHOD.

  METHOD check_win.
    DATA: lines TYPE board_type.

    lines = VALUE #( ( board[ 1 ] ) ( board[ 2 ] ) ( board[ 3 ] )
                    ( CONDENSE( board[ 1 ][ 1 ] && board[ 2 ][ 1 ] && board[ 3 ][ 1 ] ) )
                    ( CONDENSE( board[ 1 ][ 2 ] && board[ 2 ][ 2 ] && board[ 3 ][ 2 ] ) )
                    ( CONDENSE( board[ 1 ][ 3 ] && board[ 2 ][ 3 ] && board[ 3 ][ 3 ] ) )
                    ( CONDENSE( board[ 1 ][ 1 ] && board[ 2 ][ 2 ] && board[ 3 ][ 3 ] ) )
                    ( CONDENSE( board[ 1 ][ 3 ] && board[ 2 ][ 2 ] && board[ 3 ][ 1 ] ) ) ).

    LOOP AT lines INTO DATA(line).
      IF line CS player_enum-one AND NOT line CS player_enum-two.
        win = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    win = abap_false.
  ENDMETHOD.

  METHOD count_marks.
    LOOP AT board INTO DATA(row).
      x_count += strlen( regex_replace( val = row regex = '[^X]' with = '' global = abap_true ) ).
      o_count += strlen( regex_replace( val = row regex = '[^O]' with = '' global = abap_true ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD check_draw.
    DATA(free_slots) = lines( VALUE #( FOR row IN board FOR pos = 1 WHILE pos <= strlen( row ) ( row+pos(1) ) ) WHERE table_line CP ` ` ).
    IF free_slots = 0 AND NOT check_win( board ).
      draw = abap_true.
    ELSE.
      draw = abap_false.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
