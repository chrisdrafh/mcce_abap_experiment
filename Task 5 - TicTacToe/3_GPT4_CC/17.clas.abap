CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE TABLE OF char100 WITH DEFAULT KEY INITIAL SIZE 3,  " Adjusted to use char100 for representing rows.
           state_type TYPE string.

    CONSTANTS: BEGIN OF player_enum,
                 x TYPE player_type VALUE 'X',
                 o TYPE player_type VALUE 'O',
               END OF player_enum.

    CONSTANTS: BEGIN OF state_enum,
                 ongoing TYPE state_type VALUE `Ongoing game`,
                 draw    TYPE state_type VALUE `Draw`,
                 win     TYPE state_type VALUE `Win`,
               END OF state_enum.

    METHODS: get_state
      IMPORTING
        board        TYPE board_type
      RETURNING
        VALUE(state) TYPE state_type
      RAISING
        cx_parameter_invalid.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS: check_win_for_player
      IMPORTING
        board       TYPE board_type
        player      TYPE player_type
      RETURNING
        VALUE(win)  TYPE abap_bool.
ENDCLASS.

CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: x_count TYPE i,
          o_count TYPE i.

    " Count the occurrences of X and O
    LOOP AT board INTO DATA(row).
      x_count += strlen( regex_replace( row, '[^X]', '' ) ).
      o_count += strlen( regex_replace( row, '[^O]', '' ) ).
    ENDLOOP.

    " Check for invalid board conditions
    IF x_count < o_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    " Determine if there is a winner
    DATA(player) = player_enum-x.
    IF check_win_for_player( board, player ).
      RETURN state_enum-win.
    ENDIF.

    player = player_enum-o.
    IF check_win_for_player( board, player ).
      RETURN state_enum-win.
    ENDIF.

    " Check if the game is a draw or ongoing
    IF x_count + o_count = 9.
      RETURN state_enum-draw.
    ELSE.
      RETURN state_enum-ongoing.
    ENDIF.
  ENDMETHOD.

  METHOD check_win_for_player.
    " Check rows, columns and diagonals for a win
    LOOP AT board INTO DATA(row) WHERE row CP |*{ player }*{ player }*{ player }|.
      win = abap_true.
      RETURN.
    ENDLOOP.

    " Columns
    DO 3 TIMES.
      IF board[ 1 ]+sy-index(1) = player AND
         board[ 2 ]+sy-index(1) = player AND
         board[ 3 ]+sy-index(1) = player.
        win = abap_true.
        RETURN.
      ENDIF.
    ENDDO.

    " Diagonals
    IF ( board  = player AND board  = player AND board  = player ) OR
       ( board  = player AND board  = player AND board  = player ).
      win = abap_true.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
