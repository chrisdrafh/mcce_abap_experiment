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
                 ongoing_game TYPE string VALUE 'Ongoing game',
                 draw         TYPE string VALUE 'Draw',
                 win          TYPE string VALUE 'Win',
               END OF state_enum.

    METHODS: get_state
      IMPORTING board        TYPE board_type
      RETURNING VALUE(state) TYPE string
      RAISING   cx_parameter_invalid.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS: check_win_for_player
      IMPORTING
        board   TYPE board_type
        player  TYPE player_type
      RETURNING VALUE(result) TYPE abap_bool,
             check_draw
      IMPORTING
        board   TYPE board_type
      RETURNING VALUE(result) TYPE abap_bool.
ENDCLASS.


CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: x_count TYPE i,
          o_count TYPE i,
          idx     TYPE i.

    " Count occurrences of X and O
    LOOP AT board INTO DATA(row).
      x_count += strlen( regex_replace( val = row regex = '[^X]' with = '' global = abap_true ) ).
      o_count += strlen( regex_replace( val = row regex = '[^O]' with = '' global = abap_true ) ).
    ENDLOOP.

    " Check if the board state is valid
    IF abs( x_count - o_count ) > 1 OR o_count > x_count.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    " Check for wins and draws
    DATA(x_win) = check_win_for_player( board = board player = player_enum-one ).
    DATA(o_win) = check_win_for_player( board = board player = player_enum-two ).

    IF x_win AND o_win OR ( x_win AND o_count >= x_count ) OR ( o_win AND x_count > o_count ).
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    IF x_win.
      state = state_enum-win.
    ELSEIF o_win.
      state = state_enum-win.
    ELSEIF check_draw( board ).
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD check_win_for_player.
    LOOP AT board INTO DATA(row).
      IF row = player && player && player.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    LOOP AT 1 UP TO 3 INTO idx.
      IF board[ 1 ]+idx(1) = player AND
         board[ 2 ]+idx(1) = player AND
         board[ 3 ]+idx(1) = player.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    IF board[ 1 ]+1(1) = player AND board[ 2 ]+2(1) = player AND board[ 3 ]+3(1) = player OR
       board[ 1 ]+3(1) = player AND board[ 2 ]+2(1) = player AND board[ 3 ]+1(1) = player.
      result = abap_true.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD check_draw.
    IF NOT result.
      result = x_count + o_count = 9.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
