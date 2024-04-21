CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES player_type TYPE c LENGTH 1.
    TYPES board_type TYPE TABLE OF string INITIAL SIZE 3 WITH DEFAULT KEY.

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
    METHODS validate_board
      IMPORTING board TYPE board_type
      RAISING   cx_parameter_invalid.
    METHODS check_win
      IMPORTING board TYPE board_type
      RETURNING VALUE(result) TYPE abap_bool.
    METHODS check_draw
      IMPORTING board TYPE board_type
      RETURNING VALUE(result) TYPE abap_bool.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    " Validate the board for correct turn order and rules
    validate_board( board ).

    " Check for a win condition
    IF check_win( board ).
      state = state_enum-win.
    ELSEIF check_draw( board ).
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD validate_board.
    DATA: count_x TYPE i,
          count_o TYPE i.

    " Count X's and O's
    LOOP AT board INTO DATA(line).
      count_x = count_x + strlen( REPLACE ALL OCCURRENCES OF ' ' IN line WITH '' ) WHERE ( line CO 'X' ).
      count_o = count_o + strlen( REPLACE ALL OCCURRENCES OF ' ' IN line WITH '' ) WHERE ( line CO 'O' ).
    ENDLOOP.

    " Check valid turn order
    IF count_x - count_o NOT IN 0 TO 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid EXPORTING textid = text-001.
    ENDIF.

    " Check for playing after a win
    IF count_x + count_o = 9 AND NOT check_win( board ).
      RETURN.
    ENDIF.

    IF check_win( board ) AND count_x + count_o < 9.
      RAISE EXCEPTION TYPE cx_parameter_invalid EXPORTING textid = text-002.
    ENDIF.
  ENDMETHOD.

  METHOD check_win.
    LOOP AT board INTO DATA(line).
      IF line = 'XXX' OR line = 'OOO'.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Check columns and diagonals
    DO 3 TIMES.
      IF board[1](+sy-index(1)) = board[2](+sy-index(1)) = board[3](+sy-index(1)) AND board[1](+sy-index(1)) IS NOT INITIAL.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDDO.

    IF ( board  = board  = board  OR board  = board  = board  ) AND board  IS NOT INITIAL.
      result = abap_true.
      RETURN.
    ENDIF.

    result = abap_false.
  ENDMETHOD.

  METHOD check_draw.
    DATA: free_space TYPE abap_bool VALUE abap_false.

    LOOP AT board INTO DATA(line).
      IF line CP '* '.
        free_space = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    result = NOT free_space AND NOT check_win( board ).
  ENDMETHOD.

ENDCLASS.
