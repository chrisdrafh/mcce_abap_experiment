CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE TABLE OF string INITIAL SIZE 3 WITH DEFAULT KEY.

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
      IMPORTING
        board        TYPE board_type
      RETURNING
        VALUE(state) TYPE string
      RAISING
        cx_parameter_invalid.

  PRIVATE SECTION.
    METHODS count_characters
      IMPORTING
        board TYPE board_type
      RETURNING
        VALUE(result) TYPE i.

    METHODS check_winner
      IMPORTING
        board TYPE board_type
      RETURNING
        VALUE(winner) TYPE player_type.

    METHODS is_valid_board
      IMPORTING
        board TYPE board_type
      RETURNING
        VALUE(is_valid) TYPE abap_bool.
ENDCLASS.

CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: winner TYPE player_type,
          is_valid TYPE abap_bool.

    is_valid = is_valid_board( board ).
    IF is_valid = abap_false.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    winner = check_winner( board ).
    IF winner IS NOT INITIAL.
      state = state_enum-win.
    ELSEIF count_characters( board ) = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.

  ENDMETHOD.

  METHOD count_characters.
    DATA(character_count) = 0.
    LOOP AT board INTO DATA(row).
      character_count = character_count + strlen( CONDENSE( row ) ).
    ENDLOOP.
    result = character_count.
  ENDMETHOD.

  METHOD check_winner.
    DATA: patterns TYPE TABLE OF string,
          row_string TYPE string.

    patterns = VALUE #( ( `XXX` ) ( `OOO` ) ).

    LOOP AT board INTO row_string.
      IF row_string IS IN patterns.
        winner = row_string+0(1).
        RETURN.
      ENDIF.
    ENDLOOP.

    " Check columns and diagonals
    DO 3 TIMES.
      row_string = |{ board[1]+sy-index(1) }{ board[2]+sy-index(1) }{ board[3]+sy-index(1) }|.
      IF row_string IS IN patterns.
        winner = row_string+0(1).
        RETURN.
      ENDIF.
    ENDDO.

    row_string = |{ board[1]+1(1) }{ board[2]+2(1) }{ board[3]+3(1) }|.
    IF row_string IS IN patterns.
      winner = row_string+0(1).
      RETURN.
    ENDIF.

    row_string = |{ board[1]+3(1) }{ board[2]+2(1) }{ board[3]+1(1) }|.
    IF row_string IS IN patterns.
      winner = row_string+0(1).
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD is_valid_board.
    DATA(count_x) = count_characters( VALUE board_type( FOR <row> IN board ( TRANSLATE <row> USING `X ` ) ) ).
    DATA(count_o) = count_characters( VALUE board_type( FOR <row> IN board ( TRANSLATE <row> USING `O ` ) ) ).
    
    " X starts the game, so there must be equal numbers or one more X than O
    is_valid = xsdbool( count_x = count_o OR count_x = count_o + 1 ).

    " Check if the game continued after a win
    IF is_valid = abap_true AND check_winner( board ) IS NOT INITIAL.
      DATA(count_empty) = 9 - ( count_x + count_o ).
      IF count_empty > 0.
        is_valid = abap_false.
      ENDIF.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
