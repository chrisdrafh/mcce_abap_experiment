CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES player_type TYPE c LENGTH 1.
    TYPES board_type TYPE TABLE OF string INITIAL SIZE 3 WITH NON-UNIQUE DEFAULT KEY.

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
    METHODS check_board_validity
      IMPORTING
        board TYPE board_type
      RAISING
        cx_parameter_invalid.
    METHODS check_win
      IMPORTING
        board TYPE board_type
      RETURNING
        VALUE(is_win) TYPE abap_bool.
    METHODS count_marks
      IMPORTING
        board TYPE board_type
      RETURNING
        VALUE(x_count) TYPE i
        VALUE(o_count) TYPE i.
ENDCLASS.

CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: x_count TYPE i,
          o_count TYPE i.

    check_board_validity( board ).

    x_count, o_count = count_marks( board ).

    IF check_win( board ) = abap_true.
      state = state_enum-win.
    ELSEIF x_count + o_count = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD check_board_validity.
    DATA: x_count TYPE i,
          o_count TYPE i.

    x_count, o_count = count_marks( board ).

    IF x_count < o_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    IF check_win( board ) = abap_true AND x_count + o_count < 9.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.
  ENDMETHOD.

  METHOD check_win.
    DATA: i TYPE i.

    " Check rows and columns for a win
    DO 3 TIMES.
      i = sy-index - 1.
      IF board  = board  AND board  = board  AND board  <> ' '.
        is_win = abap_true.
        RETURN.
      ENDIF.
      IF board[0](1 + (i * 2)) = board[1](1 + (i * 2)) AND board[1](1 + (i * 2)) = board[2](1 + (i * 2)) AND board[0](1 + (i * 2)) <> ' '.
        is_win = abap_true.
        RETURN.
      ENDIF.
    ENDDO.

    " Check diagonals for a win
    IF ( board  = board  AND board  = board  OR
         board  = board  AND board  = board  ) AND board  <> ' '.
      is_win = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD count_marks.
    DATA: char TYPE c.

    LOOP AT board INTO DATA(row).
      DO 5 TIMES.
        char = row+sy-index(1).
        CASE char.
          WHEN 'X'.
            x_count = x_count + 1.
          WHEN 'O'.
            o_count = o_count + 1.
        ENDCASE.
      ENDDO.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
