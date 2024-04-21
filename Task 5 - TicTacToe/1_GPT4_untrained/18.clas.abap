CLASS zcl_state_of_tic_tac_toe DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES: player_type TYPE c LENGTH 1,
           board_type TYPE TABLE OF string INITIAL SIZE 3 WITH NON-UNIQUE DEFAULT KEY.
    
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
    METHODS check_winner
      IMPORTING board TYPE board_type
      RETURNING VALUE(winner) TYPE player_type.

    METHODS count_chars
      IMPORTING board  TYPE board_type
                char   TYPE c LENGTH 1
      RETURNING VALUE(count) TYPE i.

ENDCLASS.

CLASS zcl_state_of_tic_tac_toe IMPLEMENTATION.

  METHOD get_state.
    DATA: x_count TYPE i,
          o_count TYPE i,
          winner  TYPE player_type.

    x_count = count_chars( board, player_enum-one ).
    o_count = count_chars( board, player_enum-two ).

    " Check for invalid board
    IF x_count - o_count NOT IN 0 TO 1.
      RAISE EXCEPTION TYPE cx_parameter_invalid.
    ENDIF.

    winner = check_winner( board ).

    IF winner IS NOT INITIAL.
      state = state_enum-win.
    ELSEIF x_count + o_count = 9.
      state = state_enum-draw.
    ELSE.
      state = state_enum-ongoing_game.
    ENDIF.
  ENDMETHOD.

  METHOD check_winner.
    " Check rows, columns, and diagonals for a winner
    LOOP AT board INTO DATA(row) FROM 1 TO 3.
      " Check rows
      IF row(1) = row(2) AND row(2) = row(3) AND row(1) <> ' '.
        winner = row(1).
        RETURN.
      ENDIF.
    ENDLOOP.

    " Check columns and diagonals
    DO 3 TIMES.
      ASSIGN COMPONENT sy-index OF STRUCTURE board INDEX 1 TO FIELD-SYMBOL(<first>).
      ASSIGN COMPONENT sy-index OF STRUCTURE board INDEX 2 TO FIELD-SYMBOL(<second>).
      ASSIGN COMPONENT sy-index OF STRUCTURE board INDEX 3 TO FIELD-SYMBOL(<third>).

      IF <first>(1) = <second>(1) AND <second>(1) = <third>(1) AND <first>(1) <> ' '.
        winner = <first>(1).
        RETURN.
      ENDIF.
    ENDDO.

    " Check diagonals
    IF board  = board  AND board  = board  AND board  <> ' '.
      winner = board .
    ELSEIF board  = board  AND board  = board  AND board  <> ' '.
      winner = board .
    ENDIF.
  ENDMETHOD.

  METHOD count_chars.
    LOOP AT board INTO DATA(line).
      count += strlen( line ) - strlen( replace( line, char, '' ) ).
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
