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
                 ongoing_game TYPE string VALUE 'Ongoing game',
                 draw         TYPE string VALUE 'Draw',
                 win          TYPE string VALUE 'Win',
               END OF state_enum.

    "! @parameter state | Possible values are enumerated in state_enum
    "! @raising cx_parameter_invalid | Board is invalid
    METHODS get_state
      IMPORTING board        TYPE board_type
      RETURNING VALUE(state) TYPE string
      RAISING   cx_parameter_invalid.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS check_win FOR TESTING RAISING cx_parameter_invalid.
    METHODS count_symbols RETURNING VALUE(cnt_x) TYPE i VALUE(cnt_o) TYPE i.
    METHODS validate_board USING board TYPE board_type RAISING cx_parameter_invalid.
ENDCLASS.

CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: cnt_x TYPE i,
          cnt_o TYPE i.

    validate_board( board ).

    cnt_x = count_occurrences_of( val = 'X' in_table = board ).
    cnt_o = count_occurrences_of( val = 'O' in_table = board ).

    IF cnt_x < cnt_o OR cnt_x > cnt_o + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    DATA(win) = check_win( board ).
    IF win IS NOT INITIAL.
      state = win.
    ELSEIF cnt_x + cnt_o = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.

  ENDMETHOD.

  METHOD check_win.
    LOOP AT board INTO DATA(row).
      IF row CS 'XXX'.
        RETURN state_enum-win.
      ELSEIF row CS 'OOO'.
        RETURN state_enum-win.
      ENDIF.
    ENDLOOP.

    " Check columns and diagonals
    DO 3 TIMES.
      VARIABLE: col TYPE string,
                dia1 TYPE string,
                dia2 TYPE string.
      LOOP AT board INTO DATA(row) INDEX INTO DATA(idx).
        col = col && row+sy-index(1).
        dia1 = dia1 && row+sy-index(sy-index).
        dia2 = dia2 && row+sy-index(4-sy-index).
      ENDLOOP.
      IF col = 'XXX' OR dia1 = 'XXX' OR dia2 = 'XXX'.
        RETURN state_enum-win.
      ELSEIF col = 'OOO' OR dia1 = 'OOO' OR dia2 = 'OOO'.
        RETURN state_enum-win.
      ENDIF.
    ENDDO.
    RETURN ''.
  ENDMETHOD.

  METHOD count_occurrences_of.
    DATA(count) = 0.
    LOOP AT table ASSIGNING FIELD-SYMBOL(<fs_line>).
      count = count + strlen( |{ <fs_line> }| ).
    ENDLOOP.
    RETURN count.
  ENDMETHOD.

  METHOD validate_board USING board TYPE board_type.
    " Implementation to check the validity based on the rules mentioned
  ENDMETHOD.

ENDCLASS.
