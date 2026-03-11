# FortranChess

FortranChess is a console chess engine and playable chess program written in
Fortran. It supports:

- human vs computer play
- self-play
- SAN-style and coordinate move entry
- optional opening books for White and Black
- starting a game from a PGN opening sequence
- automatic PGN logging
- optional shared chess clocks such as `3+2`
- a basic UCI mode

This project started from code by Dean Menezes in
[FortranChess](https://github.com/menezesd/FortranChess).

This README describes how to build it and how to use the current program.

## Build

The repository includes a simple `Makefile`.

On Windows with `gfortran` and `make` in `PATH`:

```powershell
make
```

This produces:

- `chess.exe` for the console program
- `chessgui.exe` for the native Windows GUI window

To clean build products:

```powershell
make clean
```

## Quick Start

Start the console program:

```powershell
.\chess.exe
```

Start the GUI window:

```powershell
.\chessgui.exe
```

Show help:

```powershell
.\chess.exe --help
```

Start UCI mode:

```powershell
.\chess.exe --uci
```

Start from a PGN opening sequence:

```powershell
.\chess.exe --open myline.pgn
```

## Console Setup

At startup the program asks for:

1. your side:
   `White`, `Black`, or `N` for self-play
2. a shared time control for both sides:
   for example `3+2`
3. the AI maximum search depth
4. whether to show the evaluation after each computer move
5. the resignation threshold in pawns

If you leave the time-control prompt blank, no shared clock is used. In that
case the computer uses a default time limit of 60 seconds per move.

During console or GUI play, the engine also appends a behind-the-scenes
`search_debug.log` file in the project directory. It records each AI turn's move
history, opening-book decision, root candidate scores by depth, and final move
choice so suspicious moves can be diagnosed after the game.

Saved games in `games.pgn` include per-move elapsed-time annotations as concise
comments such as `{4s}`.

## Windows GUI

The repository also builds `chessgui.exe`, a native Win32 GUI executable.

Current scope:

- opens a graphical window on Windows
- draws Staunton-style piece images on the board
- lets you choose color, time control, search depth, and an optional opening-book move cap before starting a game
- accepts move entry in SAN or coordinate form through a text box
- lets you press Enter in the move box to play a move
- includes `Suggest Move`, `Autoplay`, and `Takeback` buttons
- displays White and Black clock times
- shows the current evaluation from White's perspective
- includes `Offer Draw` and `Resign` buttons
- uses the opening books during GUI play and aborts start if a book file is invalid
- keeps the console program and UCI mode unchanged

When `chessgui.exe` runs, it also appends `gui_debug.log` in the project
directory. That file records GUI startup checkpoints, key Win32 messages, button
actions, move-entry attempts, and other high-risk GUI paths so a crash usually
leaves a readable trail up to the last completed step.

## Command-Line Options

The program currently accepts:

- `--help`
- `-help`
- `help`
- `--uci`
- `-uci`
- `--open FILE`
- `-open FILE`

Examples:

```powershell
.\chess.exe --help
.\chess.exe --uci
.\chess.exe --open qgd_line.pgn
```

## Move Entry

The console parser accepts both SAN-style moves and coordinate moves.

Examples of accepted input:

- `d4`
- `Nf6`
- `cxd5`
- `cd5`
- `exd5`
- `ed5`
- `O-O`
- `O-O-O`
- `e2e4`
- `1.d4`
- `3...Nf6`

Notes:

- `x` in captures is optional
- `+` for check is allowed but not required
- case is ignored for matching user input
- castling with zeros is normalized, so `0-0` and `0-0-0` are accepted too

The computer prints its own moves in algebraic notation.

## In-Game Commands

During a console game you can type:

- `?`
  ask the engine for a suggested move
- `suggest`
  ask the engine for a suggested move
- `autoplay`
  let the engine play both sides from the current position
- `takeback`
  take back the last move in self-play, or the last move pair against the engine
- `score`
  print the current evaluation from White's perspective
- `eval on`
  show the evaluation after each computer move
- `eval off`
  stop showing the evaluation after each computer move
- `resign 5`
  set the resignation threshold to 5 pawns
- `resign off`
  disable engine resignation
- `resign on`
  re-enable engine resignation using the current threshold
- `help`
  show in-game help
- `quit`
  end the game

## Time Controls and Clocks

The startup time-control prompt applies the same clock to both sides.

Use the format:

```text
minutes+increment_seconds
```

Examples:

- `3+2`
- `5+0`
- `15+10`

Behavior:

- both sides start with the same initial time
- the increment is added after each completed move
- remaining clock times are shown before each turn
- if a side runs out of time, the game ends by time forfeit

When a shared time control is active, the engine chooses its think time from the
remaining clock and increment. The entered depth is then a ceiling, not a fixed
promise.

Without a shared time control, the engine still uses timed search, but with a
default limit of 60 seconds per move.

## Resignation

The engine can resign when its evaluation falls below the configured threshold.

Behavior:

- the default threshold is 5 pawns
- you can disable resignation entirely
- if the engine offers resignation, the human may refuse and continue the game
- the engine will make at most 3 resignation attempts per game

## Self-Play

Choose `N` or `n` when asked for a color to let the engine play both sides.

This is useful for:

- quick testing
- checking opening books
- generating games for later inspection

## Starting From a PGN Sequence

Use `--open FILE` to begin from a sequence of moves stored in a PGN-like file.

Example:

```powershell
.\chess.exe --open french_setup.pgn
```

The loader accepts normal PGN movetext and ignores:

- tag lines such as `[Event "..."]`
- brace comments `{...}`
- semicolon comments
- parenthesized variations
- result markers such as `1-0`, `0-1`, `1/2-1/2`, `*`

If a token cannot be applied as a legal move, startup stops with an error
message that includes the offending token and line number.

## Opening Books

At console startup, before other game setup, the program looks for:

- `book_white.txt`
- `book_black.txt`

If a file is absent, no opening book is used for that side.

If a book file exists but has invalid syntax, the program reports the error at
startup, prints the offending line, and, where possible, suggests a correction
or replacement line. When an automatic fix is available, it offers to overwrite
the file and recheck it before continuing. If you decline, startup exits.

### Book Format

Book files use one opening continuation per non-comment line.

Rules:

- blank lines are allowed for readability
- comment lines starting with `!` or `#` are ignored
- each active line must start with a numbered move token
- use `n.` for a White move and `n...` for a Black move
- moves are written in SAN-like notation

Examples:

```text
1.d4 Nf6 2.c4 e6 3.Nc3 Bb4
4.e3 O-O
4...d5

1.e4 c5 2.Nf3 d6
3.d4 cxd4 4.Nxd4 Nf6
```

### Continuations and Variations

A later line is interpreted relative to the previous nonblank, noncomment line:

- if it starts at the next half-move, it continues the previous line
- if it starts earlier, it branches from that earlier point
- if it starts later than the next expected half-move, it is a syntax error

Examples:

```text
1.d4 Nf6 2.c4 e6 3.Nc3 Bb4 4.e3 O-O 5.Bd3 d5
5...c5
6.Nf3
```

Here:

- `5...c5` means "same moves through `5.Bd3`, then branch with `...c5`"
- `6.Nf3` means "continue the immediately previous line"

### Book Move Selection

For the current position, the engine:

1. finds all book lines matching the played move history
2. collects the distinct next moves for that side
3. chooses uniformly among them

If there is only one matching next move, it always plays that move.

## PGN Output

At the end of each console game, the program appends the game to:

```text
games.pgn
```

Notes:

- elapsed move times are written as comments like `{4s}`
- games are appended, not overwritten
- each game is separated by blank lines
- movetext is wrapped at 100 columns
- the result is written for normal endings, including threefold-repetition draws, resignation, and time forfeit

If you want to start fresh, simply delete `games.pgn`.

## UCI Mode

The program includes a basic UCI driver.

Start it with:

```powershell
.\chess.exe --uci
```

Supported command families include:

- `uci`
- `isready`
- `ucinewgame`
- `position startpos ...`
- `position fen ...`
- `go`
- `quit`

The current UCI implementation is intentionally simple. It is suitable for
basic engine testing and GUI connection, but it is not a full-strength UCI
frontend with every option exposed.

## Files You May Want to Edit

- `book_white.txt`
  White opening book
- `book_black.txt`
  Black opening book
- `games.pgn`
  accumulated saved games

## Typical Console Session

```text
.\chess.exe
Choose your color (White/Black):
White
Time control for both sides (e.g., 3+2 for 3 minutes plus 2 seconds increment, blank for none):
3+2
Enter AI max search depth (e.g., 5, actual think time comes from the shared clock):
5
Show evaluation after computer moves? (Y/n):
y
Resign threshold in pawns (default 5, type off for no resignation):
5
```

Then play moves such as:

```text
d4
Nf3
cxd5
?
score
```

## Current Limitations

- the main user interface is console-based
- opening books are used in console play, not as a general-purpose PGN database
- the UCI driver is basic
- build settings are simple and intended for straightforward local compilation

## Summary

For most users, the usual workflow is:

1. `make`
2. `.\chess.exe`
3. choose side and optional time control
4. enter moves in SAN or coordinate notation
5. review the saved game in `games.pgn`
