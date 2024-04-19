CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE TABLE OF string LENGTH 3 INITIAL SIZE 3 WITH NON-UNIQUE DEFAULT KEY.

    CONSTANTS: BEGIN OF player_enum,
                 one TYPE player_type VALUE 'X',
                 two TYPE player_type VALUE 'O',
               END OF player_enum.

    CONSTANTS: BEGIN OF state_enum,
                 ongoing_game TYPE string VALUE 'Ongoing game',
                 draw         TYPE string VALUE 'Draw',
                 win          TYPE string VALUE 'Win',
               END OF state_enum.

    CLASS-METHODS: get_state
      IMPORTING
        board        TYPE board_type
      RETURNING
        VALUE(state) TYPE string
      RAISING
        cx_parameter_invalid.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS: count_characters
      IMPORTING
        board TYPE board_type
      RETURNING
        VALUE(result) TYPE i.

    METHODS: check_win
      IMPORTING
        board TYPE board_type
        player TYPE player_type
      RETURNING
        VALUE(winner_found) TYPE abap_bool.

    METHODS: check_draw
      IMPORTING
        board TYPE board_type
      RETURNING
        VALUE(is_draw) TYPE abap_bool.
ENDCLASS.


CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: x_count TYPE i,
          o_count TYPE i.

    x_count = count_characters( board = board
                                player = player_enum-one ).
    o_count = count_characters( board = board
                                player = player_enum-two ).

    " Check for invalid board configurations
    IF x_count < o_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    " Check for win states for X and O
    IF check_win( board = board
                  player = player_enum-one ) = abap_true.
      IF o_count >= x_count.
        RAISE EXCEPTION TYPE cx_parameter_invalid.
      ENDIF.
      state = state_enum-win.
    ELSEIF check_win( board = board
                      player = player_enum-two ) = abap_true.
      IF x_count > o_count.
        RAISE EXCEPTION TYPE cx_parameter_invalid.
      ENDIF.
      state = state_enum-win.
    ELSEIF check_draw( board = board ) = abap_true.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.

  ENDMETHOD.

  METHOD count_characters.
    DATA(result) = 0.
    LOOP AT board INTO DATA(row).
      result = result + strlen( condense( row ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD check_win.
    " Horizontal, vertical, and diagonal checks
    LOOP AT board INTO DATA(row).
      IF row CS repeat( player, 3 ).
        winner_found = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Vertical and diagonal checks to be implemented
    " Note: Implement logic here to check the rest of the win conditions
    winner_found = abap_false.
  ENDMETHOD.

  METHOD check_draw.
    " Assuming all cells are filled and no winners
    is_draw = xsdbool( count_characters( board = board ) = 9 AND
                       check_win( board = board
                                  player = player_enum-one ) = abap_false AND
                       check_win( board = board
                                  player = player_enum-two ) = abap_false ).
  ENDMETHOD.

ENDCLASS.
