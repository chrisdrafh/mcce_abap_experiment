CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE TABLE OF string LENGTH 3 INITIAL SIZE 3.

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
        VALUE(result) TYPE i,
      check_winner
        IMPORTING
          board TYPE board_type
          char  TYPE c
        RETURNING
          VALUE(is_winner) TYPE abap_bool,
      is_board_full
        IMPORTING
          board TYPE board_type
        RETURNING
          VALUE(full) TYPE abap_bool.
ENDCLASS.


CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: x_count TYPE i,
          o_count TYPE i.

    x_count = count_characters( board = board char = 'X' ).
    o_count = count_characters( board = board char = 'O' ).

    IF x_count < o_count OR x_count > o_count + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          text = 'Invalid board configuration: Wrong number of Xs and Os'.
    ENDIF.

    DATA: x_wins TYPE abap_bool,
          o_wins TYPE abap_bool.

    x_wins = check_winner( board = board char = 'X' ).
    o_wins = check_winner( board = board char = 'O' ).

    IF x_wins = abap_true AND o_wins = abap_true.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING
          text = 'Invalid board configuration: Both players cannot win simultaneously'.
    ELSEIF x_wins = abap_true.
      state = state_enum-win.
    ELSEIF o_wins = abap_true.
      state = state_enum-win.
    ELSE.
      IF is_board_full( board = board ) = abap_true.
        state = state_enum-draw.
      ELSE.
        state = state_enum-ongoing_game.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD count_characters.
    LOOP AT board INTO DATA(line).
      result = result + strlen( replace( val = line sub = ' ' with = '' ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD check_winner.
    " Check rows, columns, and diagonals for a win condition
    LOOP AT board INTO DATA(line).
      IF line = |{ char }{ char }{ char }|.
        is_winner = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Check columns
    DO 3 TIMES.
      IF board[ 1 ]( 1 ) = char AND board[ 2 ]( 1 ) = char AND board[ 3 ]( 1 ) = char.
        is_winner = abap_true.
        RETURN.
      ENDIF.
    ENDDO.

    " Check diagonals
    IF ( board[ 1 ]( 1 ) = char AND board[ 2 ]( 2 ) = char AND board[ 3 ]( 3 ) = char ) OR
       ( board[ 1 ]( 3 ) = char AND board[ 2 ]( 2 ) = char AND board[ 3 ]( 1 ) = char ).
      is_winner = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD is_board_full.
    LOOP AT board INTO DATA(line).
      IF line CP '* '.
        full = abap_false.
        RETURN.
      ENDIF.
    ENDLOOP.
    full = abap_true.
  ENDMETHOD.

ENDCLASS.
