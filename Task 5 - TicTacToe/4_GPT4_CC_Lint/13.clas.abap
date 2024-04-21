CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE TABLE OF string INITIAL SIZE 3 WITH DEFAULT KEY,
           line_type TYPE string LENGTH 3.

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
    METHODS count_characters
      IMPORTING char    TYPE c
                str     TYPE string
      RETURNING VALUE(count) TYPE i.

    METHODS validate_board
      IMPORTING board        TYPE board_type
      RAISING   cx_parameter_invalid.

    METHODS check_winner
      IMPORTING board TYPE board_type
      RETURNING VALUE(winner) TYPE player_type.

    METHODS check_draw
      IMPORTING board TYPE board_type
      RETURNING VALUE(is_draw) TYPE abap_bool.
ENDCLASS.

CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: winner TYPE player_type,
          is_draw TYPE abap_bool.

    validate_board( board ).
    winner = check_winner( board ).
    is_draw = check_draw( board ).

    IF winner IS NOT INITIAL.
      state = state_enum-win.
    ELSEIF is_draw = abap_true.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD count_characters.
    count = strlen( replace( val = str regex = replace( val = char regex = '' occ = 0 ) with = '' ) ).
  ENDMETHOD.

  METHOD validate_board.
    DATA: num_x TYPE i,
          num_o TYPE i.

    num_x = count_characters( char = player_enum-one str = condense( board ) ).
    num_o = count_characters( char = player_enum-two str = condense( board ) ).

    IF num_x < num_o OR num_x - num_o > 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    IF check_winner( board ) IS NOT INITIAL AND num_x <> num_o.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.
  ENDMETHOD.

  METHOD check_winner.
    DATA: lines TYPE TABLE OF line_type.

    lines = VALUE #( ( board[ 1 ] )
                      ( board[ 2 ] )
                      ( board[ 3 ] )
                      ( FOR i = 1 THEN i + 1 WHILE i <= 3 ( ( board[ 1 ]+board[ 2 ]+board[ 3 ] )+i(1) ) )
                      ( FOR i = 1 THEN i + 1 WHILE i <= 3 ( ( board[ 1 ]+board[ 2 ]+board[ 3 ] )(i) ) )
                      ( board[ 1 ]+1(1) & board[ 2 ]+2(1) & board[ 3 ]+3(1) )
                      ( board[ 3 ]+1(1) & board[ 2 ]+2(1) & board[ 1 ]+3(1) ) ).

    LOOP AT lines INTO DATA(line).
      IF line = 'XXX'.
        winner = player_enum-one.
        EXIT.
      ELSEIF line = 'OOO'.
        winner = player_enum-two.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD check_draw.
    is_draw = xsdbool( NOT line_exists( TABLE board WHERE table_line CS ' ' ) AND check_winner( board ) IS INITIAL ).
  ENDMETHOD.

ENDCLASS.
