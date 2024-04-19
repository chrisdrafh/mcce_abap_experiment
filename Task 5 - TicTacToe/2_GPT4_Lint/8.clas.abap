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
                 ongoing_game TYPE string VALUE 'Ongoing game',
                 draw         TYPE string VALUE 'Draw',
                 win          TYPE string VALUE 'Win',
               END OF state_enum.

    METHODS get_state
      IMPORTING board        TYPE board_type
      RETURNING VALUE(state) TYPE string
      RAISING   cx_parameter_invalid.

  PROTECTED SECTION.

  PRIVATE SECTION.
    METHODS count_players
      IMPORTING board          TYPE board_type
      RETURNING VALUE(count_x) TYPE i
      RETURNING VALUE(count_o) TYPE i.
    METHODS check_win_condition
      IMPORTING board         TYPE board_type
                player        TYPE player_type
      RETURNING VALUE(result) TYPE abap_bool.
ENDCLASS.


CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: count_x TYPE i,
          count_o TYPE i,
          win_x TYPE abap_bool,
          win_o TYPE abap_bool.

    count_players(
      EXPORTING board    = board
      IMPORTING count_x  = count_x
                 count_o  = count_o ).

    IF count_x < count_o OR count_x > count_o + 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING text = 'Invalid player turn count'.
    ENDIF.

    win_x = check_win_condition( board = board player = player_enum-one ).
    win_o = check_win_condition( board = board player = player_enum-two ).

    IF win_x = abap_true AND win_o = abap_true.
      RAISE EXCEPTION TYPE cx_parameter_invalid
        EXPORTING text = 'Invalid state: both players cannot win simultaneously'.
    ENDIF.

    IF win_x = abap_true.
      state = state_enum-win.
    ELSEIF win_o = abap_true.
      state = state_enum-win.
    ELSEIF count_x + count_o = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.

  ENDMETHOD.

  METHOD count_players.
    DATA line TYPE string.
    count_x = 0.
    count_o = 0.

    LOOP AT board INTO line.
      count_x = count_x + strlen( regex_replace( val = line regex = '[^X]' with = '' ) ).
      count_o = count_o + strlen( regex_replace( val = line regex = '[^O]' with = '' ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD check_win_condition.
    DATA: row TYPE string,
          win_patterns TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
          combined TYPE string.

    win_patterns = VALUE #( ( `XXX` ) ( `OOO` ) ).
    result = abap_false.

    " Check rows
    LOOP AT board INTO row.
      IF row CS player.
        result = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

    " Check columns and diagonals
    combined = |{ board  }{ board  }{ board  }|.
    APPEND combined TO win_patterns.
    combined = |{ board  }{ board  }{ board  }|.
    APPEND combined TO win_patterns.
    combined = |{ board  }{ board  }{ board  }|.
    APPEND combined TO win_patterns.
    combined = |{ board  }{ board  }{ board  }|.
    APPEND combined TO win_patterns.
    combined = |{ board  }{ board  }{ board  }|.
    APPEND combined TO win_patterns.

    LOOP AT win_patterns INTO row WHERE row = |{ player }{ player }{ player }|.
      result = abap_true.
      EXIT.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
