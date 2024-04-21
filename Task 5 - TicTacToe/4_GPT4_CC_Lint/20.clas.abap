CLASS zcx_invalid_board DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    METHODS constructor IMPORTING textid LIKE textid OPTIONAL
                         previous LIKE previous OPTIONAL.
ENDCLASS.

CLASS zcx_invalid_board IMPLEMENTATION.
  METHOD constructor.
    super->constructor( EXPORTING textid = textid previous = previous ).
  ENDMETHOD.
ENDCLASS.

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
                 ongoing_game TYPE string VALUE 'Ongoing game',
                 draw         TYPE string VALUE 'Draw',
                 win          TYPE string VALUE 'Win',
               END OF state_enum.

    METHODS get_state
      IMPORTING board        TYPE board_type
      RETURNING VALUE(state) TYPE string
      RAISING   zcx_invalid_board.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS check_validity
      IMPORTING board        TYPE board_type
      RAISING   zcx_invalid_board.
    METHODS check_winner
      IMPORTING board        TYPE board_type
      RETURNING VALUE(winner) TYPE player_type.
ENDCLASS.

CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    check_validity( board ).

    DATA(winner) = check_winner( board ).

    IF winner = player_enum-one OR winner = player_enum-two.
      state = state_enum-win.
    ELSEIF lines( CONDENSE( board ) ) = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD check_validity.
    DATA(x_count) = 0.
    DATA(o_count) = 0.
    LOOP AT board INTO DATA(row).
      x_count += occurrences( val = row sub = 'X' ).
      o_count += occurrences( val = row sub = 'O' ).
    ENDLOOP.

    IF x_count - o_count NOT IN 0 AND 1.
      RAISE EXCEPTION TYPE zcx_invalid_board EXPORTING textid = 'Invalid turn order or extra moves'.
    ENDIF.
  ENDMETHOD.

  METHOD check_winner.
    LOOP AT board INTO DATA(row).
      IF row = 'XXX'.
        winner = player_enum-one.
        RETURN.
      ELSEIF row = 'OOO'.
        winner = player_enum-two.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Check columns and diagonals for a win condition
    " Implementation of column and diagonal check
  ENDMETHOD.

ENDCLASS.
