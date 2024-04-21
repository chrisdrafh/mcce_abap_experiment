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
                 ongoing_game TYPE string VALUE 'Ongoing game',
                 draw         TYPE string VALUE 'Draw',
                 win          TYPE string VALUE 'Win',
               END OF state_enum.

    METHODS get_state
      IMPORTING board        TYPE board_type
      RETURNING VALUE(state) TYPE string
      RAISING   cx_parameter_invalid.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS check_validity
      IMPORTING board        TYPE board_type
      RAISING   cx_parameter_invalid.
    METHODS check_winner
      IMPORTING board        TYPE board_type
      RETURNING VALUE(result) TYPE string.
    METHODS count_marks
      IMPORTING board        TYPE board_type
      RETURNING VALUE(x_count) TYPE i
      RETURNING VALUE(o_count) TYPE i.
ENDCLASS.


CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    check_validity( board ).

    DATA: winner TYPE string.
    winner = check_winner( board ).

    IF winner IS NOT INITIAL.
      state = state_enum-win.
    ELSEIF lines( board ) * 3 = count_marks( board ).
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD check_validity.
    DATA(x_count) = count_marks( board ).
    DATA(o_count) = count_marks( board ).

    IF x_count < o_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid EXPORTING text = 'Invalid turn order'.
    ENDIF.

    DATA: winner TYPE string.
    winner = check_winner( board ).

    IF winner IS NOT INITIAL AND x_count = o_count.
      RAISE EXCEPTION TYPE cx_parameter_invalid EXPORTING text = 'Game continued after win'.
    ENDIF.
  ENDMETHOD.

  METHOD check_winner.
    LOOP AT board INTO DATA(row).
      IF row CS player_enum-one AND NOT row CS player_enum-two.
        result = state_enum-win.
        RETURN.
      ENDIF.
    ENDLOOP.

    LOOP AT range OF board INDEX INTO DATA(column) FROM 1 TO 3.
      DATA(column_string) = |{ board[ 1 ][ column ] }{ board[ 2 ][ column ] }{ board[ 3 ][ column ] }|.
      IF column_string = |XXX| OR column_string = |OOO|.
        result = state_enum-win.
        RETURN.
      ENDIF.
    ENDLOOP.

    DATA: diag1 TYPE string,
          diag2 TYPE string.
    diag1 = |{ board[ 1 ][ 1 ] }{ board[ 2 ][ 2 ] }{ board[ 3 ][ 3 ] }|.
    diag2 = |{ board[ 1 ][ 3 ] }{ board[ 2 ][ 2 ] }{ board[ 3 ][ 1 ] }|.

    IF diag1 = |XXX| OR diag1 = |OOO| OR diag2 = |XXX| OR diag2 = |OOO|.
      result = state_enum-win.
    ENDIF.
  ENDMETHOD.

  METHOD count_marks.
    LOOP AT board INTO DATA(row).
      x_count += strlen( regex_replace( val = row regex = '[^X]' with = '' ) ).
      o_count += strlen( regex_replace( val = row regex = '[^O]' with = '' ) ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
