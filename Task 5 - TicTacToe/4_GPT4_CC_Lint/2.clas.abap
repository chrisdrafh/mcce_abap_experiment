CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE TABLE OF string LENGTH 3 WITH DEFAULT KEY.

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

    METHODS check_for_win
      IMPORTING
        board TYPE board_type
      RETURNING
        VALUE(result) TYPE boolean.

    METHODS count_player_moves
      IMPORTING
        board TYPE board_type
      RETURNING
        VALUE(x_count) TYPE i
        VALUE(o_count) TYPE i.

ENDCLASS.



CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA(x_count) = 0.
    DATA(o_count) = 0.
    DATA(win) = abap_false.

    count_player_moves(
      EXPORTING board = board
      IMPORTING x_count = x_count
                o_count = o_count ).

    " Validate turn order and check for excess moves
    IF x_count - o_count > 1 OR x_count < o_count.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    " Check for a win
    win = check_for_win( board ).

    IF win = abap_true.
      state = state_enum-win.
    ELSEIF x_count + o_count = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD check_for_win.
    LOOP AT board ASSIGNING FIELD-SYMBOL(<line>).
      " Check rows
      IF <line> CP 'XXX' OR <line> CP 'OOO'.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Check columns and diagonals
    DATA(diagonal1) = |{ board  }{ board  }{ board  }|.
    DATA(diagonal2) = |{ board  }{ board  }{ board  }|.
    IF diagonal1 CP 'XXX' OR diagonal1 CP 'OOO' OR
       diagonal2 CP 'XXX' OR diagonal2 CP 'OOO'.
      result = abap_true.
      RETURN.
    ENDIF.

    DO 3 TIMES.
      DATA(column) = |{ board[ 1 ]( sy-index ) }{ board[ 2 ]( sy-index ) }{ board[ 3 ]( sy-index ) }|.
      IF column CP 'XXX' OR column CP 'OOO'.
        result = abap_true.
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.

  METHOD count_player_moves.
    LOOP AT board ASSIGNING FIELD-SYMBOL(<line>).
      x_count = x_count + strlen( regex_replace( val = <line> regex = '[^X]' with = '' global = abap_true ) ).
      o_count = o_count + strlen( regex_replace( val = <line> regex = '[^O]' with = '' global = abap_true ) ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
