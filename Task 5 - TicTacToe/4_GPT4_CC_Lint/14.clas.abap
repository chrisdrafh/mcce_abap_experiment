CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE TABLE OF string INITIAL SIZE 3 WITH DEFAULT KEY.

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
      IMPORTING
        board        TYPE board_type
      RETURNING
        VALUE(state) TYPE string
      RAISING
        cx_parameter_invalid.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS count_marks
      IMPORTING
        board   TYPE board_type
        mark    TYPE player_type
      RETURNING
        VALUE(count) TYPE i.
    METHODS check_winner
      IMPORTING
        board TYPE board_type
      RETURNING
        VALUE(has_winner) TYPE abap_bool.
ENDCLASS.


CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: x_count TYPE i,
          o_count TYPE i.

    x_count = count_marks( board = board mark = player_enum-one ).
    o_count = count_marks( board = board mark = player_enum-two ).

    " Check for invalid board configurations
    IF x_count < o_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    IF check_winner( board = board ).
      state = state_enum-win.
    ELSEIF x_count + o_count = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.

  ENDMETHOD.

  METHOD count_marks.
    count = 0.
    LOOP AT board INTO DATA(row).
      count = count + strlen( row ) - strlen( replace_all( val = row sub = mark with = '' ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD check_winner.
    has_winner = abap_false.
    " Check rows, columns, and diagonals
    LOOP AT board INTO DATA(row) FROM 1 TO 3.
      IF row CP 'XXX' OR row CP 'OOO'.
        has_winner = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Check columns and diagonals
    DATA(win_conditions) = VALUE stringtab( FOR j = 1 THEN 2 UNTIL j > 3
                                            FOR i = 1 THEN 2 UNTIL i > 3
                                            LET diag1 = board[ 1 ]( j ) & board[ 2 ]( j + 1 ) & board[ 3 ]( j + 2 )
                                            LET diag2 = board[ 3 ]( j ) & board[ 2 ]( j + 1 ) & board[ 1 ]( j + 2 )
                                            IN diag1 & diag2 ).
    LOOP AT win_conditions INTO DATA(condition).
      IF condition CP 'XXX' OR condition CP 'OOO'.
        has_winner = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
