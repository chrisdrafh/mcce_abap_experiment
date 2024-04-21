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

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS check_win FOR TESTING
      IMPORTING board        TYPE board_type
                player       TYPE player_type
      RETURNING VALUE(result) TYPE abap_bool.
    METHODS count_marks
      IMPORTING board        TYPE board_type
                player       TYPE player_type
      RETURNING VALUE(count)  TYPE i.
    METHODS validate_board
      IMPORTING board        TYPE board_type
      RAISING   cx_parameter_invalid.
ENDCLASS.



CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    " Validate the board first
    validate_board( board ).

    " Check for a win for both players
    IF check_win( board, player_enum-one ) = abap_true.
      RETURN state_enum-win.
    ELSEIF check_win( board, player_enum-two ) = abap_true.
      RETURN state_enum-win.
    ENDIF.

    " Check for draw or ongoing
    IF lines( board ) = 3 AND NOT lines( board ) = 0.
      DATA(is_full) TYPE abap_bool VALUE abap_true.
      LOOP AT board INTO DATA(row).
        IF row CS space.
          is_full = abap_false.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF is_full = abap_true.
        RETURN state_enum-draw.
      ELSE.
        RETURN state_enum-ongoing_game.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD check_win.
    " Check rows
    LOOP AT board INTO DATA(row).
      IF row CP ( player && player && player ).
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Check columns
    DO 3 TIMES.
      DATA(column) TYPE string.
      LOOP AT board INTO DATA(row).
        column = column && row+sy-index(1).
      ENDLOOP.
      IF column = player && player && player.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDDO.

    " Check diagonals
    IF ( board  = player AND
         board  = player AND
         board  = player ) OR
       ( board  = player AND
         board  = player AND
         board  = player ).
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD count_marks.
    LOOP AT board INTO DATA(row).
      count += strlen( replace( val = row regex = '[^' && player && ']' with = '' ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD validate_board.
    DATA(x_count) = count_marks( board, player_enum-one ).
    DATA(o_count) = count_marks( board, player_enum-two ).

    IF x_count < o_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid EXPORTING text = 'Invalid turn order or number of marks'.
    ENDIF.

    IF check_win( board, player_enum-one ) = abap_true AND x_count = o_count.
      RAISE EXCEPTION TYPE cx_parameter_invalid EXPORTING text = 'Invalid game state: X won with incorrect turn'.
    ENDIF.

    IF check_win( board, player_enum-two ) = abap_true AND x_count > o_count.
      RAISE EXCEPTION TYPE cx_parameter_invalid EXPORTING text = 'Invalid game state: O won with incorrect turn'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
