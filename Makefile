# Fortran compiler
FC = gfortran
FFLAGS = -g3 -O0 -Wall -Wextra -Wimplicit-interface -Wcharacter-truncation -Wsurprising -Werror -fbacktrace -fcheck=all -ffpe-trap=invalid,zero,overflow
GUI_LDFLAGS = -mwindows -lgdiplus -lgdi32 -luser32

# Shared engine files
CORE_SRC = app_defaults.f90 board_utils.f90 chess_types.f90 evaluation.f90 game_time_utils.f90 make_unmake.f90 move_generation.f90 search_debug_log.f90 gui_debug_log.f90 search.f90 transposition_table.f90 notation_utils.f90 opening_book.f90 move_suggestion.f90 opening_sequence.f90 user_input_processor.f90 move_ordering_heuristics.f90 game_state_checker.f90 uci_driver.f90
CORE_OBJ = $(CORE_SRC:.f90=.o)
CONSOLE_OBJ = chess.o
GUI_OBJ = chessgui.o
CLICK_GUI_OBJ = chessclickgui.o
BOOK_EDITOR_OBJ = bookeditgui.o
BOOK_TEXT_EDITOR_OBJ = booktexteditgui.o

# Executables
CONSOLE_EXE = chess
GUI_EXE = chessgui
CLICK_GUI_EXE = chessclickgui
BOOK_EDITOR_EXE = bookeditgui
BOOK_TEXT_EDITOR_EXE = booktexteditgui

all: $(CONSOLE_EXE) $(GUI_EXE) $(CLICK_GUI_EXE) $(BOOK_EDITOR_EXE) $(BOOK_TEXT_EDITOR_EXE)

$(CONSOLE_EXE): $(CORE_OBJ) $(CONSOLE_OBJ)
	$(FC) $(FFLAGS) -o $@ $^

$(GUI_EXE): $(CORE_OBJ) $(GUI_OBJ)
	$(FC) $(FFLAGS) -o $@ $^ $(GUI_LDFLAGS)

$(CLICK_GUI_EXE): $(CORE_OBJ) $(CLICK_GUI_OBJ)
	$(FC) $(FFLAGS) -o $@ $^ $(GUI_LDFLAGS)

$(BOOK_EDITOR_EXE): chess_types.o transposition_table.o board_utils.o move_generation.o make_unmake.o notation_utils.o opening_book.o $(BOOK_EDITOR_OBJ)
	$(FC) $(FFLAGS) -o $@ $^ $(GUI_LDFLAGS)

$(BOOK_TEXT_EDITOR_EXE): chess_types.o transposition_table.o board_utils.o move_generation.o make_unmake.o notation_utils.o opening_book.o $(BOOK_TEXT_EDITOR_OBJ)
	$(FC) $(FFLAGS) -o $@ $^ $(GUI_LDFLAGS)

# Object file dependencies (based on which modules each file USEs)
app_defaults.o:
transposition_table.o: chess_types.o
board_utils.o: chess_types.o transposition_table.o
evaluation.o: chess_types.o board_utils.o
game_time_utils.o: chess_types.o
make_unmake.o: chess_types.o board_utils.o transposition_table.o
move_generation.o: chess_types.o board_utils.o
search_debug_log.o: chess_types.o
gui_debug_log.o:
search.o: chess_types.o board_utils.o move_generation.o make_unmake.o evaluation.o transposition_table.o move_ordering_heuristics.o notation_utils.o search_debug_log.o
notation_utils.o: chess_types.o board_utils.o move_generation.o make_unmake.o
opening_book.o: chess_types.o notation_utils.o
move_suggestion.o: chess_types.o notation_utils.o opening_book.o search.o
opening_sequence.o: chess_types.o move_generation.o make_unmake.o notation_utils.o
user_input_processor.o: chess_types.o evaluation.o notation_utils.o move_suggestion.o opening_book.o
move_ordering_heuristics.o: chess_types.o board_utils.o
game_state_checker.o: chess_types.o board_utils.o move_generation.o
uci_driver.o: chess_types.o board_utils.o move_generation.o make_unmake.o search.o transposition_table.o
chess.o: app_defaults.o board_utils.o chess_types.o game_time_utils.o make_unmake.o move_generation.o search_debug_log.o search.o user_input_processor.o transposition_table.o game_state_checker.o notation_utils.o opening_book.o move_suggestion.o opening_sequence.o uci_driver.o
chessgui.o: app_defaults.o board_utils.o chess_types.o evaluation.o game_state_checker.o game_time_utils.o gui_debug_log.o make_unmake.o move_generation.o search_debug_log.o search.o transposition_table.o notation_utils.o move_suggestion.o
chessclickgui.o: app_defaults.o board_utils.o chess_types.o evaluation.o game_state_checker.o game_time_utils.o gui_debug_log.o make_unmake.o move_generation.o search_debug_log.o search.o transposition_table.o notation_utils.o move_suggestion.o
bookeditgui.o: board_utils.o chess_types.o move_generation.o make_unmake.o notation_utils.o opening_book.o transposition_table.o
booktexteditgui.o: board_utils.o chess_types.o move_generation.o make_unmake.o notation_utils.o opening_book.o transposition_table.o

# Compile rule
%.o: %.f90
	$(FC) $(FFLAGS) -c $<

clean:
	rm -f *.o *.mod $(CONSOLE_EXE) $(GUI_EXE) $(CLICK_GUI_EXE) $(BOOK_EDITOR_EXE) $(BOOK_TEXT_EDITOR_EXE)
