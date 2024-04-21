CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE TABLE OF string INITIAL SIZE 3 WITH NON-UNIQUE DEFAULT KEY.

    CONSTANTS: BEGIN OF player_enum,
                 one TYPE player_type VALUE 'X',
                 two TYPE player_type VALUE 'O',
               END OF player_enum.

    CONSTANTS: BEGIN OF state_enum,
                 ongoing_game TYPE string VALUE `Ongoing game`,
                 draw         TYPE string VALUE `Draw`,
                 win          TYPE string VALUE `Win`,
               END OF state_enum.

    METHODS get_state
      IMPORTING board        TYPE board_type
      RETURNING VALUE(state) TYPE string
      RAISING   cx_parameter_invalid.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS check_validity
      IMPORTING board TYPE board_type
      RAISING   cx_parameter_invalid.

    METHODS check_win
      IMPORTING board TYPE board_type
      RETURNING VALUE(has_winner) TYPE abap_bool.

    METHODS count_marks
      IMPORTING board   TYPE board_type
                player  TYPE player_type
      RETURNING VALUE(count) TYPE i.

ENDCLASS.



CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    check_validity( board ).

    DATA: has_winner TYPE abap_bool.

    has_winner = check_win( board ).

    IF has_winner = abap_true.
      state = state_enum-win.
    ELSEIF count_marks( board, player_enum-one ) + count_marks( board, player_enum-two ) = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.

  ENDMETHOD.

  METHOD check_validity.
    DATA(count_x) = count_marks( board, player_enum-one ).
    DATA(count_o) = count_marks( board, player_enum-two ).

    IF count_x < count_o OR count_x > count_o + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING text = 'Invalid turn order.'.
    ENDIF.

    IF check_win( board ) = abap_true AND count_x + count_o < 9.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING text = 'Game continued after a win.'.
    ENDIF.
  ENDMETHOD.

  METHOD check_win.
    LOOP AT board INTO DATA(row).
      IF row(1) = row(2) AND row(2) = row(3) AND row(1) <> ` `.
        has_winner = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    DATA: transposed TYPE board_type.
    DO 3 TIMES.
      DATA(column) = |{ board[ 1 ]( sy-index ) }{ board[ 2 ]( sy-index ) }{ board[ 3 ]( sy-index ) }|.
      APPEND column TO transposed.
    ENDDO.

    LOOP AT transposed INTO row.
      IF row(1) = row(2) AND row(2) = row(3) AND row(1) <> ` `.
        has_winner = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    IF board  = board  AND board  = board  AND board  <> ` ` OR
       board  = board  AND board  = board  AND board  <> ` `.
      has_winner = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD count_marks.
    LOOP AT board INTO DATA(row).
      count += strlen( replace( val = row sub = player ) ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
