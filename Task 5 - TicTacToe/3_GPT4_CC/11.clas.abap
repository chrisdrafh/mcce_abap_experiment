CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE TABLE OF string INITIAL SIZE 3.

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

  PRIVATE SECTION.
    METHODS: check_for_win RETURNING VALUE(is_win) TYPE abap_bool,
             check_for_draw RETURNING VALUE(is_draw) TYPE abap_bool,
             validate_board IMPORTING board TYPE board_type RAISING cx_parameter_invalid.

ENDCLASS.

CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: count_x TYPE i,
          count_o TYPE i.

    validate_board( board ).

    IF check_for_win( ).
      state = state_enum-win.
    ELSEIF check_for_draw( ).
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD check_for_win.
    is_win = abap_false.

    " Checking rows, columns and diagonals for a win
    LOOP AT board INTO DATA(row).
      IF row = 'XXX' OR row = 'OOO'.
        is_win = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Add similar checks for columns and diagonals
  ENDMETHOD.

  METHOD check_for_draw.
    " Check for any empty spaces and if no win condition is met
    is_draw = abap_true.
    LOOP AT board INTO DATA(row).
      IF row CS ' '. " Contains space
        is_draw = abap_false.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD validate_board.
    " Check the board for valid play and correct turn order
    count_x = count_occur( 'X' ).
    count_o = count_occur( 'O' ).

    IF count_x - count_o NOT IN 0 TO 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.
  ENDMETHOD.

  METHOD count_occur.
    " Count occurrences of 'X' or 'O' in the board
  ENDMETHOD.

ENDCLASS.
