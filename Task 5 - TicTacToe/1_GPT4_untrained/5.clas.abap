CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES player_type TYPE c LENGTH 1.
    TYPES board_type TYPE TABLE OF string INITIAL SIZE 3.

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
    METHODS check_win
      IMPORTING player TYPE player_type
                board  TYPE board_type
      RETURNING VALUE(result) TYPE abap_bool.
    METHODS count_chars
      IMPORTING board   TYPE board_type
                char    TYPE player_type
      RETURNING VALUE(count) TYPE i.
ENDCLASS.

CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: x_count TYPE i,
          o_count TYPE i.
    
    x_count = count_chars( board = board char = player_enum-one ).
    o_count = count_chars( board = board char = player_enum-two ).
    
    IF x_count < o_count OR x_count - o_count > 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    DATA: x_win TYPE abap_bool,
          o_win TYPE abap_bool.

    x_win = check_win( player = player_enum-one board = board ).
    o_win = check_win( player = player_enum-two board = board ).

    IF x_win = abap_true AND o_win = abap_true.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    IF x_win = abap_true.
      state = state_enum-win.
    ELSEIF o_win = abap_true.
      state = state_enum-win.
    ELSEIF x_count + o_count = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.

  ENDMETHOD.

  METHOD check_win.
    result = abap_false.
    LOOP AT board INTO DATA(line).
      IF line = |{ player }{ player }{ player }|.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    DATA: diag1 TYPE string,
          diag2 TYPE string.

    diag1 = |{ board[1][1] }{ board[2][2] }{ board[3][3] }|.
    diag2 = |{ board[1][3] }{ board[2][2] }{ board[3][1] }|.
    IF diag1 = |{ player }{ player }{ player }| OR diag2 = |{ player }{ player }{ player }|.
      result = abap_true.
      RETURN.
    ENDIF.

    DO 3 TIMES.
      IF |{ board[1][sy-index] }{ board[2][sy-index] }{ board[3][sy-index] }| = |{ player }{ player }{ player }|.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDDO.
  ENDMETHOD.

  METHOD count_chars.
    count = 0.
    LOOP AT board INTO DATA(line).
      count = count + strlen( replace( val = line sub = player rep = '' ) ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
