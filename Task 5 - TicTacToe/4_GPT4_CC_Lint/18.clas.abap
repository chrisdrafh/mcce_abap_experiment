CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE TABLE OF string INITIAL SIZE 3 WITH NON-UNIQUE DEFAULT KEY,
           t_checks TYPE STANDARD TABLE OF string WITH NON-UNIQUE DEFAULT KEY.

    CONSTANTS: BEGIN OF player_enum,
                 one TYPE player_type VALUE 'X',
                 two TYPE player_type VALUE 'O',
               END OF player_enum.

    CONSTANTS: BEGIN OF state_enum,
                 ongoing_game TYPE string VALUE `Ongoing game`,
                 draw         TYPE string VALUE `Draw`,
                 win          TYPE string VALUE `Win`,
               END OF state_enum.

    METHODS: constructor,
             get_state IMPORTING board TYPE board_type
                       RETURNING VALUE(state) TYPE string
                       RAISING   cx_parameter_invalid.

  PROTECTED SECTION.
    METHODS: check_win_for_player IMPORTING player TYPE player_type
                                   CHANGING board TYPE board_type
                                   RETURNING VALUE(result) TYPE abap_bool,
             check_draw CONDITIONAL,
             validate_board IMPORTING board TYPE board_type
                             RAISING   cx_parameter_invalid.

  PRIVATE SECTION.
    DATA: mv_x_count TYPE i,
          mv_o_count TYPE i.
ENDCLASS.



CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD constructor.
    " Constructor to initialize the counts of X and O
    mv_x_count = 0.
    mv_o_count = 0.
  ENDMETHOD.

  METHOD get_state.
    DATA: lv_x_won TYPE abap_bool,
          lv_o_won TYPE abap_bool.

    " Validate the board before checking state
    validate_board( board ).

    " Check for a win for both players
    lv_x_won = check_win_for_player( player_enum-one, board ).
    lv_o_won = check_win_for_player( player_enum-two, board ).

    " Determine the game state based on the results of checks
    IF lv_x_won = abap_true AND lv_o_won = abap_true.
      RAISE EXCEPTION TYPE cx_parameter_invalid EXPORTING text = 'Invalid: Multiple winners detected'.
    ELSEIF lv_x_won = abap_true.
      state = state_enum-win.
    ELSEIF lv_o_won = abap_true.
      state = state_enum-win.
    ELSEIF check_draw( ) = abap_true.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD check_win_for_player.
    DATA: winning_combinations TYPE t_checks.
    winning_combinations = VALUE #( ( `XXX` ) ( `OOO` ) ).

    LOOP AT board INTO DATA(lv_row).
      IF lv_row = winning_combinations[ 1 ] OR
         lv_row = winning_combinations[ 2 ].
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Check columns
    DO 3 TIMES.
      DATA(lv_column) = board[ 1 ]( sy-index ) & board[ 2 ]( sy-index ) & board[ 3 ]( sy-index ).
      IF lv_column = winning_combinations[ 1 ] OR
         lv_column = winning_combinations[ 2 ].
        result = abap_true.
        RETURN.
      ENDIF.
    ENDDO.

    " Check diagonals
    DATA(lv_diagonal1) = board[ 1 ]( 1 ) & board[ 2 ]( 2 ) & board[ 3 ]( 3 ).
    DATA(lv_diagonal2) = board[ 1 ]( 3 ) & board[ 2 ]( 2 ) & board[ 3 ]( 1 ).
    IF lv_diagonal1 = winning_combinations[ 1 ] OR
       lv_diagonal1 = winning_combinations[ 2 ] OR
       lv_diagonal2 = winning_combinations[ 1 ] OR
       lv_diagonal2 = winning_combinations[ 2 ].
      result = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD check_draw.
    " Assume draw if no spaces (' ') left and no winners
    LOOP AT board INTO DATA(lv_row).
      IF lv_row CS ` `.
        RETURN abap_false.
      ENDIF.
    ENDLOOP.

    result = abap_true.
  ENDMETHOD.

  METHOD validate_board
