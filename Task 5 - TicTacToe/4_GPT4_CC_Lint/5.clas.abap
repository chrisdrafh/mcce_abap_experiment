CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES player_type TYPE c LENGTH 1.
    "! E.g., ( ( `XOO` ) ( ` X ` ) ( `  X` ) )
    TYPES board_type TYPE TABLE OF string INITIAL SIZE 3.

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

    METHODS check_win
      IMPORTING board TYPE board_type
      RETURNING VALUE(result) TYPE abap_bool.

    METHODS count_marks
      IMPORTING board TYPE board_type
      RETURNING VALUE(x_count) TYPE i
      RETURNING VALUE(o_count) TYPE i.
ENDCLASS.


CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: x_count TYPE i,
          o_count TYPE i.

    " Count the marks on the board
    (x_count, o_count) = count_marks( board ).

    " Check for basic invalid conditions
    IF x_count < o_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    " Determine if someone has won
    IF check_win( board ).
      state = state_enum-win.
    ELSEIF x_count + o_count = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD check_win.
    " Check rows, columns and diagonals for a win
    LOOP AT board INTO DATA(row).
      IF row = 'XXX' OR row = 'OOO'.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Check columns and diagonals
    DO 3 TIMES.
      IF board  = board  AND board  = board  AND board  <> ' '.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDDO.

    " Check diagonals
    IF ( board  = board  AND board  = board  AND board  <> ' ' ) OR
       ( board  = board  AND board  = board  AND board  <> ' ' ).
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD count_marks.
    DATA: char TYPE c LENGTH 1.

    LOOP AT board INTO DATA(line).
      DO strlen( line ) TIMES.
        char = line+sy-index(1).
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
