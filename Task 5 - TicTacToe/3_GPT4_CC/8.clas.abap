CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: player_type TYPE c LENGTH 1,
           board_type  TYPE TABLE OF string WITH DEFAULT KEY INITIAL SIZE 3,
           game_state  TYPE string.

    CONSTANTS: BEGIN OF player_enum,
                 x TYPE player_type VALUE 'X',
                 o TYPE player_type VALUE 'O',
               END OF player_enum.

    CONSTANTS: BEGIN OF state_enum,
                 ongoing_game TYPE game_state VALUE 'Ongoing game',
                 draw         TYPE game_state VALUE 'Draw',
                 win          TYPE game_state VALUE 'Win',
               END OF state_enum.

    METHODS: get_state
      IMPORTING board TYPE board_type
      RETURNING VALUE(state) TYPE game_state
      RAISING   cx_parameter_invalid.

  PRIVATE SECTION.
    METHODS: validate_board
      IMPORTING board TYPE board_type
      RAISING   cx_parameter_invalid,
             check_winner
      IMPORTING board  TYPE board_type
      RETURNING VALUE(result) TYPE abap_bool,
             count_moves
      IMPORTING board  TYPE board_type
                player TYPE player_type
      RETURNING VALUE(count) TYPE i.

ENDCLASS.

CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    " Validate board
    validate_board( board ).

    " Determine if there is a winner
    IF check_winner( board ).
      state = state_enum-win.
    ELSEIF lines( board ) * 3 = count_moves( board, player_enum-x ) + count_moves( board, player_enum-o ).
      " All cells are filled
      state = state_enum-draw.
    ELSE.
      " Game is still ongoing
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD validate_board.
    DATA: x_count TYPE i,
          o_count TYPE i.

    x_count = count_moves( board, player_enum-x ).
    o_count = count_moves( board, player_enum-o ).

    " Check valid number of moves
    IF x_count - o_count NOT IN 0 TO 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid EXPORTING text = 'Invalid board configuration due to incorrect turn order'.
    ENDIF.

    " Check if game continued after win
    IF check_winner( board ) AND x_count + o_count NOT IN 9 TO 9.
      RAISE EXCEPTION TYPE cx_parameter_invalid EXPORTING text = 'Game continued after a win'.
    ENDIF.
  ENDMETHOD.

  METHOD check_winner.
    DATA: line TYPE string.
    LOOP AT board INTO line.
      IF line CS 'XXX'.
        result = abap_true.
        RETURN.
      ELSEIF line CS 'OOO'.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Check columns and diagonals
    DO 3 TIMES.
      line = board[ 1 ][ sy-index ] & board[ 2 ][ sy-index ] & board[ 3 ][ sy-index ].
      IF line = 'XXX' OR line = 'OOO'.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDDO.

    " Check diagonals
    line = board[ 1 ][ 1 ] & board[ 2 ][ 2 ] & board[ 3 ][ 3 ].
    IF line = 'XXX' OR line = 'OOO'.
      result = abap_true.
      RETURN.
    ENDIF.

    line = board[ 1 ][ 3 ] & board[ 2 ][ 2 ] & board[ 3 ][ 1 ].
    IF line = 'XXX' OR line = 'OOO'.
      result = abap_true.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD count_moves.
    DATA(character TYPE c LENGTH 1).
    character = COND #( WHEN player = 'X' THEN 'X' ELSE 'O' ).
    LOOP AT board INTO DATA(line).
      count += strlen( CONDENSE( line ) ) - strlen( REPLACE ALL OCCURRENCES OF character IN CONDENSE( line ) WITH '' ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.

