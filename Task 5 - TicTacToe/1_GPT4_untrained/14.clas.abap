CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES player_type TYPE c LENGTH 1.
    TYPES board_type TYPE TABLE OF string INITIAL SIZE 3 WITH NON-UNIQUE DEFAULT KEY.

    CONSTANTS: BEGIN OF player_enum,
                 one TYPE player_type VALUE 'X',
                 two TYPE player_type VALUE 'O',
               END OF player_enum.

    CONSTANTS: BEGIN OF state_enum,
                 ongoing_game TYPE string VALUE `Ongoing game`,
                 draw         TYPE string VALUE `Draw`,
                 win          TYPE string VALUE `Win`,
               END OF state_enum.

    CLASS-DATA: wrong_turn_order TYPE string VALUE `Wrong turn order`.
    CLASS-DATA: continued_playing_after_win TYPE string VALUE `Continued playing after win`.

    METHODS get_state
      IMPORTING board        TYPE board_type
      RETURNING VALUE(state) TYPE string
      RAISING   cx_parameter_invalid.

  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS check_for_winner
      IMPORTING
        board TYPE board_type
      RETURNING
        VALUE(winner) TYPE player_type.

    METHODS count_markers
      IMPORTING
        board TYPE board_type
        player TYPE player_type
      RETURNING
        VALUE(count) TYPE i.

    METHODS check_validity
      IMPORTING
        board TYPE board_type
      RAISING
        cx_parameter_invalid.

ENDCLASS.


CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: winner TYPE player_type.

    " First, check if the board is valid.
    check_validity( board ).

    " Check if there's a winner.
    winner = check_for_winner( board ).

    IF winner IS NOT INITIAL.
      state = state_enum-win.
    ELSEIF lines( board ) = 3 AND NOT line_exists( table = board where table_line CS space ).
      " All cells are filled and no winner, it's a draw.
      state = state_enum-draw.
    ELSE.
      " Game is still ongoing.
      state = state_enum-ongoing_game.
    ENDIF.

  ENDMETHOD.

  METHOD check_for_winner.
    " Define all winning combinations.
    DATA: winning_combinations TYPE TABLE OF string WITH NON-UNIQUE DEFAULT KEY.
    winning_combinations = VALUE #( ( `XXX......` ) ( `...XXX...` ) ( `......XXX` ) 
                                    ( `X..X..X..` ) ( `.X..X..X.` ) ( `..X..X..X` ) 
                                    ( `X...X...X` ) ( `..X.X.X..` ) ).
    " Check board against each winning pattern.
    LOOP AT winning_combinations INTO DATA(combination).
      IF 
        board[ 1 ] CP ( combination+0(3) ) AND
        board[ 2 ] CP ( combination+3(3) ) AND
        board[ 3 ] CP ( combination+6(3) ).
        " Determine winner based on pattern match.
        winner = combination+0(1).
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD count_markers.
    count = 0.
    LOOP AT board INTO DATA(line).
      count += strlen( line ) - strlen( replace( val = line sub = player with = `` ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD check_validity.
    DATA: count_x TYPE i.
    DATA: count_o TYPE i.
    count_x = count_markers( board = board player = `X` ).
    count_o = count_markers( board = board player = `O` ).

    IF count_x < count_o OR count_x - count_o > 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid EXPORTING textid = wrong_turn_order.
    ENDIF.

    " Check if someone has won already and the game continued.
    IF check_for_winner( board ) IS NOT INITIAL AND count_x + count_o = 9.
      RAISE EXCEPTION TYPE cx_parameter_invalid EXPORTING textid = continued_playing_after_win.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
